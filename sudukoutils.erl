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
solution_pretty_print([SolutionX1, SolutionX2, SolutionX3, SolutionY1, SolutionY2, SolutionY3, SolutionZ1, SolutionZ2, SolutionZ3]) ->
  io:format("------------------------------------ ~n",[]),
  io:format("|        SOLUTION                   |~n",[]),
  io:format("------------------------------------ ~n",[]), 
  sol_pp_helper(SolutionX1, SolutionY1, SolutionZ1),
  sol_pp_helper(SolutionX2, SolutionY2, SolutionZ2),
  sol_pp_helper(SolutionX3, SolutionY3, SolutionZ3),
  solution_pretty_print([]).


sol_pp_helper([],[],[]) -> io:format("",[]);
sol_pp_helper([SolX1, SolX2, SolX3 |T1],
              [Sol2X1, Sol2X2, Sol2X3|T2], 
			  [Sol3X1, Sol3X2, Sol3X3|T3] ) ->
  {{_,_},V1,_} = SolX1,
  {{_,_},V2,_} = SolX2,
  {{_,_},V3,_} = SolX3,
  {{_,_},V4,_} = Sol2X1,
  {{_,_},V5,_} = Sol2X2,
  {{_,_},V6,_} = Sol2X3,
  {{_,_},V7,_} = Sol3X1,
  {{_,_},V8,_} = Sol3X2,
  {{_,_},V9,_} = Sol3X3,
  io:format("| ~w | ~w | ~w | ~w | ~w | ~w | ~w | ~w | ~w | ~n",[V1, V2, V3, V4, V5, V6, V7, V8, V9]),
  io:format("------------------------------------ ~n",[]),

  sol_pp_helper(T1, T2, T3).

    


%%test_solution_validity([]) -> io:format("~n",[]);
%%test_solution_validity(Solution) ->
%% need to test each row, then each column, then each grid.  bit of a pain seeing as its bust.
