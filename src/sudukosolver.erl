-module(sudukosolver).
-compile(export_all).

main(File) ->
  {ok, Bin} = file:read_file(File),

  Board = sudukoutils:parse_board(Bin),
  sudukoutils:test_board(Board),

  GridMap = sudukogrid:create_grid_map(Board, 1, []),

  Rows = sudukogrid:get_stacks(lists:seq(1, 9), "Rows", GridMap, []),
  Columns = sudukogrid:get_stacks(lists:seq(1, 9), "Columns", GridMap, []),
  Grids = sudukogrid:get_3by3_grids(2, GridMap, []),
  sudukoutils:pretty_print(GridMap),
  Solution = solve(Grids, Grids, Rows, Columns, [], count_zeros(Grids, 0)),
  sudukoutils:solution_pretty_print(Solution).


%%pair solve.  needs to go later...bingo,
pair_solve([],_,_,_,Solution) -> lists:reverse(Solution);
pair_solve([Grid|Tail], Grids, Rows, Columns, Solution) ->

  PairGridOptions = test_grids(Grid, Grid, Rows, Columns, []),
  PairValues = option_grid_solution_values(lists:seq(1, 9), grid_solution_values(PairGridOptions, []), 2, []),
  Pairs = find_option_cell(PairValues, PairGridOptions, []),


  SolutionPairs = pair_evaluation(Pairs, Grids, Rows, Columns, PairGridOptions, []),

  if length(SolutionPairs) == 1 -> io:format("pair is ~w ~n", [hd(SolutionPairs)]);
    true -> io:format("")
  end,

  PairUpdatedRows = sudukogrid:update_maps(Rows, SolutionPairs, []),
  PairUpdatedColumns = sudukogrid:update_maps(Columns, SolutionPairs, []),
  PairUpdatedGrid = sudukogrid:update_map(Grid, SolutionPairs, []),
  PairUpdatedGrids = sudukogrid:update_maps(Grids, SolutionPairs, []),
  pair_solve(Tail, PairUpdatedGrids, PairUpdatedRows, PairUpdatedColumns, [PairUpdatedGrid | Solution]).


%% logic section.
solve([], Grids, Rows, Columns, Solution, Counter) ->
  %% need to update our grids and rows with each solution and return them too.  annoying.
  ZeroCounter = count_zeros(Solution, 0),
  io:format("Zero counter ~w and Counter ~w ~n", [ZeroCounter, Counter]),
  if ZeroCounter < Counter ->
    solve(lists:reverse(Solution), Grids, Rows, Columns, [], ZeroCounter);
    ZeroCounter == 0 -> lists:reverse(Solution);
    true ->   pair_solve(lists:reverse(Solution), Grids, Rows, Columns, [])

  end;
solve([Grid | Tail], Grids, Rows, Columns, Solution, Counter) ->
  GridOptions = test_grids(Grid, Grid, Rows, Columns, []),
  UniqueValues = option_grid_solution_values(lists:seq(1, 9), grid_solution_values(GridOptions, []), 1, []),
  Cell = find_option_cell(UniqueValues, GridOptions, []),

  if length(Cell) > 0 ->
    {{X1, Y1}, V1, _} = hd(Cell),
    io:format("unique solution {~w,~w} ~w ~n", [X1, Y1, V1]);
    true -> io:format("no match ~n")
  end,

  UpdatedRows = sudukogrid:update_maps(Rows, Cell, []),
  UpdatedColumns = sudukogrid:update_maps(Columns, Cell, []),
  UpdatedGrid = sudukogrid:update_map(Grid, Cell, []),
  UpdatedGrids = sudukogrid:update_maps(Grids, Cell, []),

  solve(Tail, UpdatedGrids, UpdatedRows, UpdatedColumns, [UpdatedGrid | Solution], Counter).

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
  io:format("direction ~s for ~w ~w ~n", [Direction, V, PV]),

 %% CellList = lists:filter(fun(X) -> X /= V end, cell_solution_values(GridOptions, {{CellX, CellY}, V, F}, [])),

 %% io:format("cell list ~w ~w ~n", [hd(CellList), tl(CellList)]),

 %% PairCellList = lists:filter(fun(X) ->
 %%   X /= PV end, cell_solution_values(GridOptions, {{PairCellX, PairCellY}, PV, PF}, [])),


  UniqueOption = get_single_option(V,Grids,Direction, {{CellX,CellY},V,F}, Rows, Columns, 0),
  UniquePairOption = get_single_option(PV,Grids,Direction,{{PairCellX,PairCellY},PV,PF}, Rows, Columns, 0),

  io:format("option count ~w vs pair option count ~w for option{~w,~w} and pair{~w,~w} for value ~w ~n",[UniqueOption, UniquePairOption, CellX, CellY, PairCellX, PairCellY, V]),

  if UniquePairOption /= 0, UniqueOption == 0 ->
    pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, [{{CellX, CellY}, V, "true"} | Acc]);
    UniquePairOption == 0, UniqueOption /= 0 ->
      pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, [{{PairCellX, PairCellY}, PV, "true"} | Acc]);
    true -> pair_evaluation(Rest, Grids, Rows, Columns, GridOptions, Acc)
  end.

%% got head on and bug fixed..need more info for evil board
%%get_single_option([],_,_,_,_,_,UniqueOption) -> UniqueOption;

get_single_option(Option,Grids,Direction, {{X,Y}, V, F},Rows,Columns, UniqueOption) ->
  DirectionGrids  = get_direction_grids(Grids, Direction, {X, Y}, get_grid_centre(X), get_grid_centre(Y), []),

  CountOptions = count_options(DirectionGrids,{{X,Y},V,F},Direction, Rows, Columns, Option, 0),

  io:format("count options ~w for value ~w ~n", [CountOptions, Option]),
  if CountOptions > 0 -> CountOptions + UniqueOption;
    true -> UniqueOption
  end.
%%  if CountOptions > 0 -> get_single_option(T,Grids,Direction,{{X,Y},V,F},Rows, Columns,CountOptions+UniqueOption);
%%    true -> get_single_option(T,Grids,Direction,{{X,Y},V,F}, Rows, Columns, UniqueOption)
%%  end.

count_options([],_,_,_,_,_, Count) -> Count;
count_options([Grid | Tail],{{X,Y},V,F}, Direction, Rows, Columns, Option, Count) ->
  %% so given our direction, what rows do we want.  well we want our rows and cols based on any 0 value in our direction.
  OptionColumn = get_option_for_grid_column(Grid, Y, Columns, {{X,Y},V,F}, []),
  OptionRow = get_option_for_grid_row(Grid, X, Rows, {{X,Y},V,F}, []),

  io:format("the lists ~w ~n ", [length(lists:append([Grid,OptionColumn, OptionRow]))]),


  Res = lists:any(fun({{_, _}, V, _}) -> V == Option end, lists:append([Grid,OptionColumn, OptionRow])),

  if Res == true ->
    io:format("has value ~w for option ~w ~n", [hd(Grid),Option]),
    count_options(Tail,{{X,Y},V,F}, Direction, Rows, Columns, Option, Count);
    true ->
      io:format("has not got value ~w for option ~w ~n", [hd(Grid),Option]),
      count_options(Tail, {{X,Y},F,V}, Direction, Rows, Columns, Option, Count + 1)
  end.

get_option_for_grid_column([],_,_,_,Acc) -> lists:reverse(Acc);
get_option_for_grid_column([{{X,Y},V,F}|Tail], FixedAxis, Options, Cell, Acc) ->
  if V == 0, FixedAxis == X  -> get_option_for_grid_column(Tail,FixedAxis, Options,Cell, lists:append([Acc,sudukogrid:filter_by_cell(Options,{{X,Y},V,F},[])]));
    true -> get_option_for_grid_column(Tail, FixedAxis, Options, Cell, Acc)
  end.
%% we need to also look at each row or column for our 0 value space in the grid. on our same axis.
%% note: we need to maintain our axis.
get_option_for_grid_row([],_,_,_,Acc) -> lists:reverse(Acc);
get_option_for_grid_row([{{X,Y},V,F}|Tail], FixedAxis, Options, Cell, Acc) ->
 if V == 0, FixedAxis == Y  -> get_option_for_grid_row(Tail,FixedAxis, Options,Cell, lists:append([Acc,sudukogrid:filter_by_cell(Options,{{X,Y},V,F},[])]));
   true -> get_option_for_grid_row(Tail, FixedAxis, Options, Cell, Acc)
 end.


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
  Filter = sudukogrid:filter_this_direction_grid(Grid, fun({{X, Y}, V, F}) -> {X, Y} == Cell end),

  if Direction == "vertical" -> check_direction_grid(Filter, fun({{X, Y}, V, F}) -> X == CentreX end);
    true -> check_direction_grid(Filter, fun({{X, Y}, V, F}) -> Y == CentreY end)
  end.

check_direction_grid(Grid, F) ->
  Res = lists:any(F, Grid),
  if Res == true -> Grid;
    true -> []
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
  Column = sudukogrid:filter_by_cell(Columns, {{X, Y}, V, F}, []),
  Row = sudukogrid:filter_by_cell(Rows, {{X, Y}, V, F}, []),
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


