-module(sudukosolver).
-compile(export_all).

main() ->
 File = "suduko.txt",
 {ok,Bin} = file:read_file(File),

 Board = sudukoutils:parse_board(Bin),
 sudukoutils:test_board(Board),

 GridMap = create_grid_map(Board, 1, []),
 %%good bit. get our workable stacks.
 Rows = get_stacks(lists:seq(1,9), "Rows", GridMap, []),
 Columns = get_stacks(lists:seq(1,9), "Columns", GridMap, []),
 Grids = get_3by3_grids(2, GridMap, []),
 %%print(Columns),
 sudukoutils:pretty_print(GridMap),
 Solution = solve(Rows, Columns, Grids, []),
 sudukoutils:solution_pretty_print(Solution).
 %%pretty_print(GridMap).



%% creates our tuples per item
create_grid_map([],_, Acc) -> lists:reverse(Acc);
create_grid_map([B1, B2, B3, B4, B5, B6, B7, B8, B9|Rest], Y, Acc) ->
 List = [B1, B2, B3, B4, B5, B6, B7, B8, B9],
 create_grid_map(Rest, Y+1, create_row(List, 1, Y, Acc)). 

create_row([],_,_,Acc) -> Acc;
create_row([R|T], X, Y, Acc) ->
  create_row(T, X+1, Y, [{{X,Y}, R, is_fixed(R)} | Acc]).
 
is_fixed(V) -> 
 if V == 0 -> "false";
  true -> "true"
  end.

%%helper function to stacks.
get_stacks([],_,_,Stacks) -> lists:reverse(Stacks);
get_stacks([C|T],Type, Map, Stacks) ->
 if Type == "Columns" ->  get_stacks(T, Type, Map, [get_stack({C, -1}, Map,[])|Stacks]);
    Type == "Rows" ->  get_stacks(T,Type, Map, [get_stack({-1, C}, Map,[])|Stacks]);
    true -> erlang:error("non supported type")
 end.

get_stack(_,[], Stack) -> lists:reverse(Stack);
get_stack(Exp, [Map|T], Stack) ->
 Resp = contains_value(Map,Exp),
 if Resp == true ->
  get_stack(Exp, T, [Map|Stack]);
 true -> get_stack(Exp, T, Stack)
 end. 

contains_value({{X,Y},_,_}, {Col,Row}) ->
 if X == Col -> true;
    Y == Row -> true;
 true -> false
 end. 

%% help function to get our 3by3 grids
get_3by3_grids(11, _, Stack) -> lists:reverse(Stack);
get_3by3_grids(X, Board, Stack) ->
 get_3by3_grids(X+3, Board, [get_grids(X,8, Board, []), get_grids(X,5, Board, []), get_grids(X,2, Board, []) | Stack]).

get_grids(_,_, [], Stack) -> lists:reverse(Stack);
get_grids(X, Y, [Board|T], Stack) ->
 Resp = find_3by3_element(X, Y, Board),
 if  Resp == true -> get_grids(X, Y, T, [ Board |Stack]);
  true -> get_grids(X,Y,T, Stack)
 end.


%% need to find 3by 3.
find_3by3_element(StartX,StartY,{{X,Y},_,_}) ->
 
 if X == StartX, Y == StartY -> true;
    X == StartX+1, Y == StartY -> true;
    X == StartX-1, Y == StartY -> true;
    X == StartX, Y == StartY+1 -> true;
    X == StartX, Y == StartY-1 -> true;
    X == StartX-1, Y == StartY+1 -> true;
    X == StartX+1, Y == StartY-1 -> true;
    X == StartX+1, Y == StartY+1 -> true;
    X == StartX-1, Y == StartY-1 -> true;
    true -> false
 end. 



%% filter grids by row
filter_grids_by_row([], _, Filter) -> lists:reverse(Filter);
filter_grids_by_row([CellX, CellY, CellZ|Tail], Grids, Filter) ->
  %% to review seems to work of sorts. wont work for the grids in tail...fix later on.
 filter_grids_by_row(Tail, Grids, [filter_by_cell(Grids, CellX, [])|Filter]).

%% filter list of lists by a cell
filter_by_cell([], _, Filter) -> lists:reverse(Filter);
filter_by_cell([List|Tail], Cell, Filter) ->
  Res = lists:any(fun(X) -> X == Cell end,List),
  if Res == true ->  filter_by_cell(Tail, Cell, List);
  true -> filter_by_cell(Tail, Cell, Filter) 
  end.

%% logic section.
solve([],_,_,Solution) -> lists:reverse(Solution);
solve([Row|Tail], Columns, Grids, Solution) -> 
 %% functions required. Firstly we need an exit case.
 %% test out row 1.. and its matching columns, and grids...then go crazy with the full board.
 %% for 1 row, all columns are applicable.  And vice versa, grids are only those grids that contain the row elements
 %% once row 1 is done, need to update the data, in order for the solution to be factored in for the next check.  not for today.
 RowSolution = test_solution(Row, Row, Columns, filter_grids_by_row(Row, Grids, []),  []),
%% solve(Tail, Columns, Grids, [RowSolution|Solution]).
 solve(Tail, update_solution(RowSolution, Columns, []), update_solution(RowSolution, Grids, []), [RowSolution|Solution]).


%% update with solution
update_solution([],Map, Acc) -> lists:reverse(Acc); 
update_solution([Solution|RowSolution], [Map|Rest], Acc) ->
 %% we have a list of lists.
 update_solution(RowSolution, Rest, [update_map(Map, Solution, [])|Acc]).
 
update_map([], _, Acc) -> lists:reverse(Acc);
update_map([Map|T], {{Sx,Sy},Sv,Sf}, Acc) ->
  update_map(T, {{Sx,Sy},Sv,Sf}, [update_cell(Map,{{Sx,Sy},Sv,Sf})|Acc]).

update_cell({{X,Y},V,F}, {{Sx,Sy},Sv,Sf}) ->
 if {X,Y} == {Sx,Sy} -> {{Sx,Sy},Sv,Sf};
 true -> {{X,Y},V,F}
 end.


%% test row solution
test_solution([],_,_,_, Solution) -> lists:reverse(Solution);
test_solution([Row|T], Fixed, Columns, Grids, Solution) -> 
 {{X,Y},V,F} = Row,
 if F == "false", V == 0 -> 
   Unique = get_unique_value(lists:seq(1,9),Fixed, Solution,
                 filter_by_cell(Columns, Row, []),
                 filter_by_cell(Grids, Row, []), {{X,Y},V,F}),
   %% given this we now need to check against our column....
   test_solution(T, Fixed, Columns, Grids, [Unique|Solution]); 
   true -> test_solution(T,Fixed, Columns, Grids, [Row|Solution])
 end.

%% get a unique value to the solution we need to add our unique column and grid (we can only be in one of each).
get_unique_value([],_,_,_,_,NewValue) -> NewValue;
get_unique_value([Value|Rest], Row, Solution, Column, Grid, NewValue) -> 
 {{X,Y},V,F} = NewValue,
 Res = lists:any(fun({{X,Y},V,F}) -> V == Value end, lists:append([Column, Solution,Grid, Row])),
   if Res == false -> 
%%   io:format("{~w,~w} value will be set to ~w ~n",[X,Y,Value]),
   get_unique_value([],Row, Solution, Column, Grid, {{X,Y},Value,F});
   true -> 
%%    io:format("{~w,~w} no match value will be set to ~w ~n",[X,Y,0]),
    get_unique_value(Rest,Row, Solution, Column, Grid, {{X,Y},0,F})
  end.





 





   









