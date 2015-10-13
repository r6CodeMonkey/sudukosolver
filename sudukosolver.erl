-module(sudukosolver).
-compile(export_all).

main() ->
 File = "suduko.txt",
 {ok,Bin} = file:read_file(File),
 create_grid_map(parse_board(Bin), 1,[]).

 

parse_board(Bin) when is_binary(Bin) ->
parse_board(binary_to_list(Bin));
parse_board(Str) when is_list(Str) ->
[list_to_integer(X)||X <- string:tokens(Str,"\r\n ")].


%% we now want to create grid map
create_grid_map([],_, Acc) -> lists:reverse(Acc);
create_grid_map(Board, X, Acc) ->  
 Row = create_row(hd(Board), X, 1),
   create_grid_map(tl(Board), X+1,[Row|Acc]).


%% so its only hitting the other case.  so that is wrong
%% basically we cant bump the X fo some fucking reason.
create_row([H|T], X, Y) ->
  {{X,Y}, H} ++ create_row(T, X, Y+1).
% keep hitting fucking 2,1,1 which is the final fucking case.....i am going to kill myself.



   









