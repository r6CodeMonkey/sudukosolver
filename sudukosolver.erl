-module(sudukosolver).
-compile(export_all).

main(File) ->
  {ok, Bin} = file:read_file(File),

  Board = sudukoutils:parse_board(Bin),
  sudukoutils:test_board(Board),

  GridMap = create_grid_map(Board, 1, []),
  %%good bit. get our workable stacks.
  Rows = get_stacks(lists:seq(1, 9), "Rows", GridMap, []),
  Columns = get_stacks(lists:seq(1, 9), "Columns", GridMap, []),
  Grids = get_3by3_grids(2, GridMap, []),
  sudukoutils:pretty_print(GridMap),
  Solution = solve(Grids, Grids, Rows, Columns, [], count_zeros(Grids, 0)),
  %%solve(Grids, Grids, Rows, Columns, [], count_zeros(Grids, 0)).
  %% need to covert grids to rows again.  annoying.
  sudukoutils:solution_pretty_print(Solution).


%% creates our tuples per item
create_grid_map([], _, Acc) -> lists:reverse(Acc);
create_grid_map([B1, B2, B3, B4, B5, B6, B7, B8, B9 | Rest], Y, Acc) ->
  List = [B1, B2, B3, B4, B5, B6, B7, B8, B9],
  create_grid_map(Rest, Y + 1, create_row(List, 1, Y, Acc)).

create_row([], _, _, Acc) -> Acc;
create_row([R | T], X, Y, Acc) ->
  create_row(T, X + 1, Y, [{{X, Y}, R, is_fixed(R)} | Acc]).

is_fixed(V) ->
  if V == 0 -> "false";
    true -> "true"
  end.

%%helper function to stacks.
get_stacks([], _, _, Stacks) -> lists:reverse(Stacks);
get_stacks([C | T], Type, Map, Stacks) ->
  if Type == "Columns" -> get_stacks(T, Type, Map, [get_stack({C, -1}, Map, []) | Stacks]);
    Type == "Rows" -> get_stacks(T, Type, Map, [get_stack({-1, C}, Map, []) | Stacks]);
    true -> erlang:error("non supported type")
  end.

get_stack(_, [], Stack) -> lists:reverse(Stack);
get_stack(Exp, [Map | T], Stack) ->
  Resp = contains_value(Map, Exp),
  if Resp == true ->
    get_stack(Exp, T, [Map | Stack]);
    true -> get_stack(Exp, T, Stack)
  end.

contains_value({{X, Y}, _, _}, {Col, Row}) ->
  if X == Col -> true;
    Y == Row -> true;
    true -> false
  end.

%% help function to get our 3by3 grids
get_3by3_grids(11, _, Stack) -> lists:reverse(Stack);
get_3by3_grids(X, Board, Stack) ->
  get_3by3_grids(X + 3, Board, [get_grids(X, 8, Board, []), get_grids(X, 5, Board, []), get_grids(X, 2, Board, []) | Stack]).

get_grids(_, _, [], Stack) -> lists:reverse(Stack);
get_grids(X, Y, [Board | T], Stack) ->
  Resp = find_3by3_element(X, Y, Board),
  if Resp == true -> get_grids(X, Y, T, [Board | Stack]);
    true -> get_grids(X, Y, T, Stack)
  end.


%% need to find 3by 3.
find_3by3_element(StartX, StartY, {{X, Y}, _, _}) ->

  if X == StartX, Y == StartY -> true;
    X == StartX + 1, Y == StartY -> true;
    X == StartX - 1, Y == StartY -> true;
    X == StartX, Y == StartY + 1 -> true;
    X == StartX, Y == StartY - 1 -> true;
    X == StartX - 1, Y == StartY + 1 -> true;
    X == StartX + 1, Y == StartY - 1 -> true;
    X == StartX + 1, Y == StartY + 1 -> true;
    X == StartX - 1, Y == StartY - 1 -> true;
    true -> false
  end.


%% filter list of lists by a cell
filter_by_cell([], _, Filter) -> lists:reverse(Filter);
filter_by_cell([List | Tail], Cell, Filter) ->
  Res = lists:any(fun(X) -> X == Cell end, List),
  if Res == true -> filter_by_cell(Tail, Cell, List);
    true -> filter_by_cell(Tail, Cell, Filter)
  end.


%% logic section.
solve([], Grids, Rows, Columns, Solution, Counter) ->
  ZeroCounter = count_zeros(Solution, 0),
  io:format("Zero counter ~w and Counter ~w ~n", [ZeroCounter, Counter]),
  if ZeroCounter < Counter ->
    solve(lists:reverse(Solution), Grids, Rows, Columns, [], ZeroCounter);
    true -> lists:reverse(Solution)
  end;
solve([Grid | Tail], Grids, Rows, Columns, Solution, Counter) ->
  GridOptions = test_grids(Grid, Grid, Rows, Columns, []),
  UniqueValues = option_grid_solution_values(lists:seq(1, 9), grid_solution_values(GridOptions, []), 1, []),
  Cell = find_option_cell(UniqueValues, GridOptions, []),

  if length(Cell) > 0 ->
    {{X1, Y1}, V1, F1} = hd(Cell),
    io:format("unique solution {~w,~w} ~w ~n", [X1, Y1, V1]);
    true -> io:format("no match ~n")
  end,

  UpdatedRows = update_maps(Rows, Cell, []),
  UpdatedColumns = update_maps(Columns, Cell, []),
  UpdatedGrid = update_map(Grid, Cell, []),

%% need to update our grid, basically by replacing the updated grid and grid.
  UpdatedGrids = update_maps(Grids, Cell, []),

  PairGridOptions = test_grids(UpdatedGrid, UpdatedGrid, UpdatedRows, UpdatedColumns, []),

  PairValues = option_grid_solution_values(lists:seq(1, 9), grid_solution_values(PairGridOptions, []), 2, []),
  Pairs = find_option_cell(PairValues, PairGridOptions, []),

  if length(PairValues) == 1 -> io:format("pair is ~w ~n", [hd(PairValues)]);
    true -> io:format("")
   end,

  SolutionPairs = pair_evaluation(Pairs, UpdatedGrids, UpdatedRows, UpdatedColumns, PairGridOptions, []),


  if length(SolutionPairs) == 1 ->

    if length(SolutionPairs) > 0 ->
      {{X, Y}, V, F} = hd(SolutionPairs),
      io:format("pair solution {~w,~w} ~w ~n", [X, Y, V]);
      true -> io:format("no match ~n")
    end,

    PairUpdatedRows = update_maps(UpdatedRows, SolutionPairs, []),
    PairUpdatedColumns = update_maps(UpdatedColumns, SolutionPairs, []),
    PairUpdatedGrid = update_map(UpdatedGrid, SolutionPairs, []),
    PairUpdatedGrids = update_maps(UpdatedGrids, SolutionPairs, []),
    solve(Tail, PairUpdatedGrids, PairUpdatedRows, PairUpdatedColumns, [PairUpdatedGrid | Solution], Counter);

    true -> solve(Tail, UpdatedGrids, UpdatedRows, UpdatedColumns, [UpdatedGrid | Solution], Counter)
  end.


%% function to calculate number of 0s in board
count_zeros([], Counter) -> Counter;
count_zeros([Grid | Tail], Counter) ->
  count_zeros(Tail, count_grid_zeros(Grid, Counter)).

count_grid_zeros([], Counter) -> Counter;
count_grid_zeros([Grid | Tail], Counter) ->
  {{_, _}, V, _} = Grid,
  if V == 0 -> count_grid_zeros(Tail, Counter + 1);
    true -> count_grid_zeros(Tail, Counter)
  end.


%% pair evaluation.  needs more checks but as long as we only return 1 field in the list (called recursively) then that field is good to use
pair_evaluation([], _, _,_,_, Acc) -> Acc;
pair_evaluation([{{CellX, CellY}, V, F}, {{PairCellX, PairCellY}, PV, PF} | Rest], Grids,Rows, Columns, GridOptions, Acc) ->

  Direction = get_direction({{CellX, CellY}, V, F}, {{PairCellX, PairCellY}, PV, PF}),
  io:format("direction ~s ~n", [Direction]),

  CellList = lists:filter(fun(X) -> X /= V end, cell_solution_values(GridOptions, {{CellX, CellY}, V, F}, [])),
  PairCellList = lists:filter(fun(X) ->
    X /= PV end, cell_solution_values(GridOptions, {{PairCellX, PairCellY}, PV, PF}, [])),


  UniqueOption = get_single_option(CellList,Grids,Direction, {{CellX,CellY},V,F}, Rows, Columns, 0),
  UniquePairOption = get_single_option(PairCellList,Grids,Direction,{{PairCellX,PairCellY},PV,PF}, Rows, Columns, 0),


  if UniquePairOption /= 0, UniqueOption == 0 ->
    pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, [{{CellX, CellY}, V, "true"} | Acc]);
    UniquePairOption == 0, UniqueOption /= 0 ->
      pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, [{{PairCellX, PairCellY}, PV, "true"} | Acc]);
    true -> pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, Acc)
  end.



%% got head on and bug fixed..need more info for evil board
get_single_option([],_,_,_,_,_,UniqueOption) -> UniqueOption;

get_single_option([Option | T],Grids,Direction, {{X,Y}, V, F},Rows,Columns, UniqueOption) ->
  DirectionGrids  = get_direction_grids(Grids, Direction, {X, Y}, get_grid_centre(X), get_grid_centre(Y), []),

  CountOptions = count_options(DirectionGrids,{{X,Y},V,F},Direction, Rows, Columns, Option, 0),
 %% io:format("count options ~w grid length ~w ~n", [CountOptions, length(Grids)]),
  if CountOptions > 0 -> get_single_option(T,Grids,Direction,{{X,Y},V,F},Rows, Columns,CountOptions+UniqueOption);
    true -> get_single_option(T,Grids,Direction,{{X,Y},V,F}, Rows, Columns, UniqueOption)
  end.



count_options([],_,_,_,_,_, Count) -> Count;
count_options([Grid | Tail],{{X,Y},V,F}, Direction, Rows, Columns, Option, Count) ->
  %% so given our direction, what rows do we want.  well we want our rows and cols based on any 0 value in our direction.
  Column = filter_by_cell(Columns, {{X, Y}, V, F}, []),
  Row = filter_by_cell(Rows, {{X, Y}, V, F}, []),


  Res = lists:any(fun({{_, _}, V, _}) -> V == Option end, lists:append([Grid, Row, Column])),

  if Res == true -> count_options(Tail,{{X,Y},V,F}, Direction, Rows, Columns, Option, Count);
    true -> count_options(Tail, {{X,Y},F,V}, Direction, Rows, Columns, Option, Count + 1)
  end.

%% ie we need get option row and column for the given free values (ie zero) in the grid for our row . columns based on direction
%% something for tomorrow.!


get_direction({{PairCellX, _}, _, _}, {{CellX, _}, _, _}) ->
  if CellX == PairCellX -> "horizontal";
    true -> "vertical"
  end.


get_grid_centre(Val) ->
  if Val < 4 -> 2;
    Val > 3, Val < 7 -> 5;
    true -> 8
  end.

get_direction_grids([], _, _, _, _, Acc) -> lists:reverse(Acc);
get_direction_grids([Grid | T], Direction, Cell, CentreX, CentreY, Acc) ->
  Filter = get_direction_grid(Grid, Direction, Cell, CentreX, CentreY),
  if Filter == [] -> get_direction_grids(T, Direction, Cell, CentreX, CentreY, Acc);
    true ->
      get_direction_grids(T, Direction, Cell, CentreX, CentreY, [Filter | Acc])
  end.

get_direction_grid(Grid, Direction, Cell, CentreX, CentreY) ->
  Filter = filter_this_direction_grid(Grid, fun({{X, Y}, V, F}) -> {X, Y} == Cell end),

  if Direction == "vertical" -> check_direction_grid(Filter, fun({{X, Y}, V, F}) -> X == CentreX end);
    true -> check_direction_grid(Filter, fun({{X, Y}, V, F}) -> Y == CentreY end)
  end.

check_direction_grid(Grid, F) ->
  Res = lists:any(F, Grid),
  if Res == true -> Grid;
    true -> []
  end.

filter_this_direction_grid(Grid, F) ->
  Res = lists:any(F, Grid),
  if Res == true -> [];
    true -> Grid
  end.



cell_solution_values([], _, Acc) -> lists:reverse(Acc);
cell_solution_values([{{GX, GY}, GV} | T], {{X, Y}, V, F}, Acc) ->
%%  io:format("cell solution values for {~w, ~w} ~n", [X,Y]),
  if GX == X, GY == Y -> cell_solution_values(T, {{X, Y}, V, F}, grid_solution_values([{{GX, GY}, GV}], Acc));
    true -> cell_solution_values(T, {{X, Y}, V, F}, Acc)
  end.


%% check solutions options for the grid
grid_solution_values([], Acc) -> lists:reverse(Acc);
grid_solution_values([{{X, Y}, V} | T], Acc) ->
 %% io:format("grid solution values for {~w, ~w} ~n", [X,Y]),
  grid_solution_values(T, lists:append(V, Acc)).

%% create a list of our unique values
option_grid_solution_values([], _, _, Acc) -> lists:reverse(Acc);
option_grid_solution_values([Value | T], GridSolution, Option, Acc) ->
  List = lists:filter(fun(X) -> X == Value end, GridSolution),
  if length(List) == Option -> option_grid_solution_values(T, GridSolution, Option, lists:append(Acc, [Value]));
    true -> option_grid_solution_values(T, GridSolution, Option, Acc)
  end.


%% find the unique cell and update
find_option_cell([], _, Acc) -> lists:reverse(Acc);
find_option_cell([Unique | T], Grid, Acc) ->
  find_option_cell(T, Grid, find_grid_cell(Grid, Unique, Acc)).

%% check each grid.
find_grid_cell([], _, Acc) -> lists:reverse(Acc);
find_grid_cell([{{X, Y}, V} | T], Value, Acc) ->
  Res = lists:any(fun(X) -> X == Value end, V),
  if Res == true -> find_grid_cell(T, Value, [{{X, Y}, Value, "true"} | Acc]);
    true -> find_grid_cell(T, Value, Acc)
  end.

%% working fine now.
test_grids([], _, _, _, Acc) -> lists:reverse(Acc);
test_grids([G | T], Grid, Rows, Columns, Acc) ->
  test_grids(T, Grid, Rows, Columns, [test_grid(G, Grid, Rows, Columns) | Acc]).


test_grid({{X, Y}, V, F}, Grid, Rows, Columns) ->
  Column = filter_by_cell(Columns, {{X, Y}, V, F}, []),
  Row = filter_by_cell(Rows, {{X, Y}, V, F}, []),
  if F == "false", V == 0 ->
    {{X, Y}, get_unique_values(lists:seq(1, 9), Row, Column, Grid, [])};
    true -> {{X, Y}, []}
  end.


%% get unique values
get_unique_values([], _, _, _, Acc) -> lists:reverse(Acc);
get_unique_values([Value | T], Row, Column, Grid, Acc) ->
  Res = lists:any(fun({{X, Y}, V, F}) -> V == Value end, lists:append([Column, Grid, Row])),
  if Res == false -> get_unique_values(T, Row, Column, Grid, [Value | Acc]);
    true -> get_unique_values(T, Row, Column, Grid, Acc)
  end.


%% need to review later if required.  Need to improve this for a list of solutions.

%% update maps
update_maps([], _, Acc) -> lists:reverse(Acc);
update_maps([Map | Rest], Solutions, Acc) ->
  update_maps(Rest, Solutions, [update_map(Map, Solutions, []) | Acc]).

update_map([], _, Acc) -> lists:reverse(Acc);
update_map([Map | T], Solutions, Acc) ->
  update_map(T, Solutions, [update_solution(Solutions, Map) | Acc]).


update_solution([], {{X, Y}, V, F}) -> {{X, Y}, V, F};
update_solution([{{Sx, Sy}, Sv, Sf} | Tail], {{X, Y}, V, F}) ->
  update_cell({{X, Y}, V, F}, {{Sx, Sy}, Sv, Sf}).

update_cell({{X, Y}, V, F}, {{Sx, Sy}, Sv, Sf}) ->
  if {X, Y} == {Sx, Sy} -> {{Sx, Sy}, Sv, Sf};
    true -> {{X, Y}, V, F}
  end.






 





   









