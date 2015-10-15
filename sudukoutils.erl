-module(sudukoutils).
-compile(export_all).

%%load the board 
 parse_board(Bin) when is_binary(Bin) -> parse_board(binary_to_list(Bin));
 parse_board(Str) when is_list(Str) -> [list_to_integer(X)||X <- string:tokens(Str,"\r\n\t ")].


%% some basic tests to ensure board is valid before starting
test_board(Board) when length(Board) == 81 ->  "Ok";
test_board(Board) when length(Board) < 81 -> erlang:error("Board too small");
test_board(Board) when length(Board) > 81 -> erlang:error("Board too large").
   
%% print function for testing
print([]) ->  io:format("End ~n", []);
print([GridMap|T]) ->
 sub_print(GridMap),
 print(T).
%% List = GridMap,
%%{{X,Y},V,F} = hd(GridMap),
%% io:format("Value is ~w For Cell ~w  ~w , fixed = ~s ~n", [V,X,Y,F]),
%% print(T). 

sub_print([]) ->  io:format("End ~n", []);
sub_print([H|T]) ->
 {{X,Y},V,F} = H,
 io:format("Value is ~w For Cell ~w  ~w , fixed = ~s ~n", [V,X,Y,F]),
 sub_print(T).

%% pretty print
pretty_print([]) -> io:format(" ",[]);
pretty_print([GridMap|T]) when length(T) == 80  ->
 {{_,_},V,F} = GridMap,
 io:format("------------------------------------ ~n",[]),
 io:format("|            Start Grid             | ~n",[]),
 io:format("------------------------------------ ~n",[]),
 if F == "true" -> io:format("| ~w ", [V]);
  true -> io:format("|  ", [])
  end,   
 pretty_print(T);
pretty_print([GridMap|T]) when length(T) rem 9 == 0  ->
 {{_,_},V,F} = GridMap,
 if F == "true" -> io:format("| ~w | ~n------------------------------------ ~n", [V]);
 true ->   io:format("|   | ~n------------------------------------ ~n", [])
 end, 
 pretty_print(T);
pretty_print([GridMap|T]) ->
 {{_,_},V,F} = GridMap,
 if F == "true" -> io:format("| ~w ", [V]);
 true -> io:format("|   ", [])
 end,   
 pretty_print(T).

solution_pretty_print([]) -> io:format("------------------------------------ ~n",[]);
solution_pretty_print([Solution|T]) when length(T) == 8 -> 
  io:format("------------------------------------ ~n",[]),
  io:format("|        SOLUTION                   |~n",[]),
  io:format("------------------------------------ ~n",[]),
  sol_pp_helper(Solution),
  solution_pretty_print(T);
solution_pretty_print([Solution|T]) ->
  io:format("------------------------------------ ~n",[]),
  sol_pp_helper(Solution),
  solution_pretty_print(T).

sol_pp_helper([]) -> io:format("|~n",[]);
sol_pp_helper([Sol|T]) ->
  {{_,_},V,_} = Sol,
  io:format("| ~w ",[V]),
  sol_pp_helper(T).

%%test_solution_validity([]) -> io:format("~n",[]);
%%test_solution_validity(Solution) ->
%% need to test each row, then each column, then each grid.  bit of a pain seeing as its bust.
