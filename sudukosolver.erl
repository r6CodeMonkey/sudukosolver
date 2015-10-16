-module(sudukosolver).
-compile(export_all).

main(File) ->
 {ok,Bin} = file:read_file(File),

 Board = sudukoutils:parse_board(Bin),
 sudukoutils:test_board(Board),

 GridMap = create_grid_map(Board, 1, []),
 %%good bit. get our workable stacks.
 Rows = get_stacks(lists:seq(1,9), "Rows", GridMap, []),
 Columns = get_stacks(lists:seq(1,9), "Columns", GridMap, []),
 Grids = get_3by3_grids(2, GridMap, []),
 sudukoutils:pretty_print(GridMap),
 Solution = solve(Grids, Grids, Rows, Columns, [], count_zeros(Grids, 0)),
 %%solve(Grids, Rows, Columns, [], count_zeros(Grids, 0)).
 %% need to covert grids to rows again.  annoying.
 sudukoutils:solution_pretty_print(Solution).



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
 filter_grids_by_row(Tail, Grids, [filter_by_cell(Grids, CellX, [])|Filter]).

%% filter list of lists by a cell
filter_by_cell([], _, Filter) -> lists:reverse(Filter);
filter_by_cell([List|Tail], Cell, Filter) ->
  Res = lists:any(fun(X) -> X == Cell end,List),
  if Res == true ->  filter_by_cell(Tail, Cell, List);
  true -> filter_by_cell(Tail, Cell, Filter) 
  end.

    

%% logic section.
solve([],Grids, Rows,Columns, Solution, Counter)  -> 
 ZeroCounter = count_zeros(Solution, 0) ,
 if ZeroCounter < Counter -> 
  solve(Solution, Grids, Rows, Columns,[], ZeroCounter);
   true -> lists:reverse(Solution)
 end;
solve([Grid|Tail],Grids, Rows, Columns, Solution, Counter) -> 
GridOptions = test_grids(Grid, Grid, Rows, Columns, []),
UniqueValues = option_grid_solution_values(lists:seq(1,9),grid_solution_values(GridOptions, []),1,[]),
Cell = find_option_cell(UniqueValues, GridOptions,[]),

UpdatedRows = update_maps(Rows, Cell, []),
UpdatedColumns = update_maps(Columns, Cell, []),
UpdatedGrid = update_map(Grid, Cell, []),


PairValues = option_grid_solution_values(lists:seq(1,9),grid_solution_values(GridOptions, []),2,[]),
Pairs = find_option_cell(PairValues, GridOptions, []),

%%if length(Pairs) > 2 ->
%% Res = pair_evaluation(PairValues, Grids, UpdatedRows, UpdatedColumns, Pairs);
%%true -> ok
%%end,

solve(Tail,[UpdatedGrid|Tail], UpdatedRows, UpdatedColumns, [UpdatedGrid|Solution], Counter).

%% need to stop.  notes lost, but concept ok.  how to share the pair routine

%% function to calculate number of 0s in board
count_zeros([], Counter) -> Counter;
count_zeros([Grid|Tail], Counter) ->
 count_zeros(Tail, count_grid_zeros(Grid, Counter)).

count_grid_zeros([], Counter) -> Counter;
count_grid_zeros([Grid|Tail], Counter) ->
 {{X,Y},V,F} = Grid,
 if V == 0 -> count_grid_zeros(Tail, Counter+1);
 true -> count_grid_zeros(Tail, Counter)
 end.


%% pair evaluation. see above.  return the cell we update, or nothing.
pair_evaluation(_,_,_,_,[]) -> 0;
%% this does not work.  but its nearly ready.  pairs can be more than 1 cell it seems.
pair_evaluation(Pair, Grids, Rows, Columns, [{{CellX,CellX},_,_},{{PairCellX,PairCellY},_,_}]) ->
 Direction = "horizontal",
 if CellX == PairCellX -> Direction = "vertical";
  true -> Direction = "horizontal"
 end,

 %% so need direction 
 if Direction == "horizontal" -> ok;
 true -> ok
 end.



%%
grid_solution_values([], Acc) -> lists:reverse(Acc);
grid_solution_values([{{X,Y},V}|T], Acc) ->
 grid_solution_values(T, lists:append(V, Acc)).

%% create a list of our unique values
option_grid_solution_values([],_,_, Acc) -> lists:reverse(Acc);
option_grid_solution_values([Value|T], GridSolution, Option, Acc) ->
List = lists:filter(fun(X) -> X == Value end, GridSolution),
if length(List) == Option -> option_grid_solution_values(T, GridSolution, Option, lists:append(Acc, List));
  true -> option_grid_solution_values(T, GridSolution, Option, Acc)
end.


%% find the unique cell and update
find_option_cell([],_, Acc) -> lists:reverse(Acc);
find_option_cell([Unique|T], Grid, Acc) ->
  find_option_cell(T, Grid, [find_grid_cell(Grid, Unique)|Acc]).

%% check each grid.
find_grid_cell([], _) -> erlang:error("value not in cell");
find_grid_cell([{{X,Y},V}|T], Value) ->  
 Res = lists:any(fun(X) -> X == Value end, V),
 if Res == true -> {{X,Y}, Value, "true"};
  true -> find_grid_cell(T, Value)
  end.

%% working fine now.
test_grids([],_,_,_,Acc) -> lists:reverse(Acc);
test_grids([G|T], Grid, Rows, Columns, Acc) ->
 test_grids(T, Grid, Rows, Columns, [test_grid(G,Grid,Rows, Columns) |Acc]).


test_grid({{X,Y},V,F}, Grid, Rows, Columns) ->
  Column = filter_by_cell(Columns, {{X,Y},V,F}, []),
  Row = filter_by_cell(Rows, {{X,Y},V,F}, []),
 if F == "false", V == 0 ->
    {{X,Y},get_unique_values(lists:seq(1,9),Row, Column, Grid, [])};
  true -> {{X,Y},[]} 
  end.

 
%% get unique values
get_unique_values([],_,_,_, Acc) -> lists:reverse(Acc);
get_unique_values([Value|T],Row,Column, Grid, Acc) -> 
 Res = lists:any(fun({{X,Y},V,F}) -> V == Value end, lists:append([Column,Grid, Row])),
 if Res == false -> get_unique_values(T, Row, Column, Grid, [Value|Acc]);
 true -> get_unique_values(T, Row, Column, Grid, Acc)
 end.
 
 
%% need to review later if required.  Need to improve this for a list of solutions.

%% update maps
update_maps([],_, Acc) -> lists:reverse(Acc); 
update_maps([Map|Rest], Solutions, Acc) ->
 update_maps(Rest, Solutions, [update_map(Map, Solutions, [])|Acc]).
 
update_map([], _, Acc) -> lists:reverse(Acc);
update_map({{X,Y},V,F}, Solutions, Acc) ->
 update_map([], Solutions,[update_solution(Solutions, {{X,Y},V,F})|Acc]);
update_map([Map|T], Solutions, Acc) ->
 update_map(T, Solutions,[update_solution(Solutions, Map)|Acc]).

 
update_solution([],{{X,Y},V,F}) ->  {{X,Y},V,F};
update_solution([{{Sx,Sy},Sv,Sf}|Tail],{{X,Y},V,F}) ->  
update_cell({{X,Y},V,F} , {{Sx,Sy},Sv,Sf}).
  
update_cell({{X,Y},V,F}, {{Sx,Sy},Sv,Sf}) ->
 if {X,Y} == {Sx,Sy} -> {{Sx,Sy},Sv,Sf};
 true -> {{X,Y},V,F}
 end.






 





   









