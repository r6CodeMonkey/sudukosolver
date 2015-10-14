-module(sudukosolver).
-compile(export_all).

main() ->
 File = "suduko.txt",
 {ok,Bin} = file:read_file(File),
 Board = parse_board(Bin),
 test_Board(Board),
 GridMap = create_grid_map(Board, 1, []),
 %%print(GridMap),
 get_column_stack(1, GridMap, []),
 get_row_stack(1, GridMap, []).
 


parse_board(Bin) when is_binary(Bin) ->
parse_board(binary_to_list(Bin));
parse_board(Str) when is_list(Str) ->
[list_to_integer(X)||X <- string:tokens(Str,"\r\n\t ")].

create_grid_map([],_, Acc) ->
   lists:reverse(Acc);
create_grid_map([B1, B2, B3, B4, B5, B6, B7, B8, B9|Rest], Y, Acc) ->
 List = [B1, B2, B3, B4, B5, B6, B7, B8, B9],
 create_grid_map(Rest, Y+1, create_row(List, 1, Y, Acc)). 

create_row([],_,_,Acc) -> Acc;
create_row([R|T], X, Y, Acc) ->
  create_row(T, X+1, Y, [{{X,Y}, R} | Acc]).
 

get_column_stack(_, [], Stack) -> lists:reverse(Stack);
get_column_stack(Col, [Map|T], Stack) ->
 Resp = contains_col_value(Map,  Col),
 if  Resp == true ->
  get_column_stack(Col, T, [get_value(Map)|Stack]);
  true -> get_column_stack(Col, T,Stack)
  end.
  
get_row_stack(_, [], Stack) -> lists:reverse(Stack);
get_row_stack(Col, [Map|T], Stack) ->
 Resp = contains_row_value(Map,  Col),
 if  Resp == true ->
  get_row_stack(Col, T, [get_value(Map)|Stack]);
  true -> get_row_stack(Col, T,Stack)
  end.
  
%%get_3by3_grid(_,_,[])  
%%get_3by3_grid(CentreRow, CentreCol, Map, Stack) ->  
  
    
contains_col_value({{X,_},_}, Col) ->
  if X == Col -> true;
  true -> false
  end.
  
contains_row_value({{_,Y},_}, Col) ->
  if Y == Col -> true;
  true -> false
  end.
  
get_value({{_,_},V}) -> V.  
  
%% some basic tests to ensure board is valid before starting
test_Board(Board) when length(Board) == 81 ->
   "Ok";
test_Board(Board) when length(Board) < 81 ->
   erlang:error("Board too small");
test_Board(Board) when length(Board) > 81 ->
   erlang:error("Board too large").
   

%% print function for testing
print([]) ->  io:format("End ~n", []);
print([GridMap|T]) ->
{{X,Y},V} = GridMap,
 io:format("Value is ~w For Cell ~w  ~w ~n", [V,X,Y]),
 print(T). 
 





   









