-module(dog_gopher).
-compile(export_all).

run_set_simulation(Gopher, Dog, Holes) ->
  lists:filter(fun (Hole) -> can_gopher_escape(Gopher, Dog, Hole) end, Holes).

distance({X1, Y1}, {X2, Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

can_gopher_escape(Gopher, Dog, Hole) ->
  2 * distance(Gopher, Hole) =< distance(Dog, Hole).

print_output([])      -> io:format("The gopher cannot escape.~n") ;
print_output([H | _]) -> io:format("The gopher can escape through the hole at ~w.~n",[H]).
