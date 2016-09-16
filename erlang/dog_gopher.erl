-module(dog_gopher).
-export([run_simulation/1]).

run_simulation(File) ->
  Data = parse_input_file(File),
  lists:foreach(fun print_output/1, lists:map(fun run_set_simulation/1, Data)).

parse_input_file(File) ->
  {ok, Handle} = file:open(File, read),
  Data = parse_file(Handle, file:read_line(Handle), []),
  file:close(Handle),
  Data.

parse_file(Handle, {ok, Line}, Sets) ->
  parse_file(Handle, file:read_line(Handle), update_sets(lists:map(fun string:to_float/1, string:tokens(Line, " ")),Sets)) ;
parse_file(_, eof, Sets) ->
  lists:reverse(lists:map(fun ({G, D, Hs}) -> {G, D, lists:reverse(Hs)} end, Sets)).

update_sets([_, {Gx,_}, {Gy,_}, {Dx,_}, {Dy,_}], Sets) ->
  [{{Gx, Gy}, {Dx, Dy}, []} | Sets] ;
update_sets([{X, _}, {Y, _}], [{G,D,Hs} | T]) ->
  [{G, D, [{X,Y} | Hs]} | T].

run_set_simulation({Gopher, Dog, Holes}) ->
  lists:filter(fun (Hole) -> can_gopher_escape(Gopher, Dog, Hole) end, Holes).

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

can_gopher_escape(Gopher, Dog, Hole) ->
  2 * distance(Gopher, Hole) =< distance(Dog, Hole).

print_output([])      -> io:format("The gopher cannot escape.~n") ;
print_output([H | _]) -> io:format("The gopher can escape through the hole at ~w.~n",[H]).
