-module(sudukogrid).
-compile(export_all).

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

filter_this_direction_grid(Grid, F) ->
  Res = lists:any(F, Grid),
  if Res == true -> [];
    true -> Grid
  end.

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

