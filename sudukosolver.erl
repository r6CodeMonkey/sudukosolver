-module(sudukosolver).
-compile(export_all).

main() ->
 File = "suduko.txt",
 {ok,Bin} = file:read_file(File),
 Board = parse_board(Bin),
 test_Board(Board),
 create_grid_map(Board, 1, []).


parse_board(Bin) when is_binary(Bin) ->
parse_board(binary_to_list(Bin));
parse_board(Str) when is_list(Str) ->
[list_to_integer(X)||X <- string:tokens(Str,"\r\n\t ")].

create_grid_map([],_, Acc) ->
   lists:reverse(Acc);
create_grid_map([B1, B2, B3, B4, B5, B6, B7, B8, B9|Rest], Y, Acc) ->
 List = [B1, B2, B3, B4, B5, B6, B7, B8, B9],
 create_grid_map(Rest, Y+1, create_row(List, 1, Y, Acc)). 

create_row([],X,Y,Acc) -> Acc;
create_row([R|T], X, Y, Acc) ->
  create_row(T, X+1, Y, [{{X,Y}, R} | Acc]).
  
  
  
%% some basic tests to ensure board is valid before starting
test_Board(Board) when length(Board) == 81 ->
   "Ok";
test_Board(Board) when length(Board) < 81 ->
   erlang:error("Board too small");
test_Board(Board) when length(Board) > 81 ->
   erlang:error("Board too large").
   


  
 





   









