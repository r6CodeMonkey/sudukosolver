-module(sudukosolver).
-compile(export_all).

main() ->
 File = "suduko.txt",
 {ok,Bin} = file:read_file(File),

 Board = parse_board(Bin),
 test_board(Board),

 GridMap = create_grid_map(Board, 1, []),
 %%good bit. get our workable stacks.
 Rows = get_stacks(lists:seq(1,9), "Rows", GridMap, []),
 Columns = get_stacks(lists:seq(1,9), "Columns", GridMap, []).

 parse_board(Bin) when is_binary(Bin) -> parse_board(binary_to_list(Bin));
 parse_board(Str) when is_list(Str) -> [list_to_integer(X)||X <- string:tokens(Str,"\r\n\t ")].

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

%%3by3 after paper analysis.  2,2 = start point. x= 2 then x || y = (x + 3) < 9.  simples.  i like functional langs.
%% why did i not fight being thrown of course by Poli, i like prolog! stupid uni cunt accused me of cheating. heyho
get_3by3_grids(_,_, [], Stack) -> lists:reverse(Stack);
get_3by3_grids(X, Y, [Board|T], Stack) ->
 Grid = [ %% not happening now.  lol.  bunk work and fix this.  
 get_3by3_grids(X+2, Y T, [find_3d3_elements(Start, Start |Stack]).          


%% need to find 3by 3.
find_3by3_element(StartX,StartY,{X,Y}) ->
 
 if X == StartX and Y == StartY -> true;
    X == StartX+1 and Y == StartY -> true;
    X == StartX-1 and Y == StartY -> true;
    X == StartX and Y = StartY+1 -> true;
    X == StartX and Y == StartY-1 -> true;
    X == StartX-1 and Y == StartY+1 -> true;
    X == StartX+1 and Y == StartY-1 -> true;
    X == StartX+1 and Y == StartY+1 -> true;
    X == StartX-1 and Y == StartY-1 -> true;
    true -> false
 end. 
     

%% some basic tests to ensure board is valid before starting
test_board(Board) when length(Board) == 81 ->  "Ok";
test_board(Board) when length(Board) < 81 -> erlang:error("Board too small");
test_board(Board) when length(Board) > 81 -> erlang:error("Board too large").
   
%% print function for testing
print([]) ->  io:format("End ~n", []);
print([GridMap|T]) ->
{{X,Y},V,F} = GridMap,
 io:format("Value is ~w For Cell ~w  ~w , fixed = ~s ~n", [V,X,Y,F]),
 print(T). 
 





   









