% Functions to write and to show routes - common to expert and user

show_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  write_route(user_output, Rname).
show_route :-
  write('Could not display the route.'),nl.

show_all_routes :-
  findall(X, route(X), Rnames),
  write_routes(user_output, Rnames).
show_all_routes :-
  write('Could not display routes.'),nl.

write_routes(Fdd, [Rname|Rest]) :-
  write_route(Fdd, Rname),
  write_routes(Fdd, Rest).
write_routes(_, []).

write_route(Fdd, Rname) :-
  bagof(Y, Rname^rlength(Rname, Y), Rlengths),
  bagof(Y, Rname^uphill(Rname, Y), Ruphills),
  bagof(Y, Rname^level(Rname, Y), Rlevels),
  bagof(Y, Rname^historic(Rname, Y), Rhistorics),
  bagof(Y, Rname^attractions(Rname, Y), Rattractions),
  bagof(Y, Rname^castles(Rname, Y), Rcastles),
  bagof(Y, Rname^region(Rname, Y), Rregions),
  bagof(Y, Rname^goes_abroad(Rname, Y), Rabroads),
  bagof(Y, Rname^santiago(Rname, Y), Rsantiagos),
  write_rname(Fdd, Rname),
  write_lengths(Fdd, Rname, Rlengths),
  write_uphills(Fdd, Rname, Ruphills),
  write_levels(Fdd, Rname, Rlevels),
  write_historics(Fdd, Rname, Rhistorics),
  write_attractions(Fdd, Rname, Rattractions),
  write_castles(Fdd, Rname, Rcastles),
  write_regions(Fdd, Rname, Rregions),
  write_abroads(Fdd, Rname, Rabroads),
  write_santiagos(Fdd, Rname, Rsantiagos),
  nl(Fdd).

write_rname(Fdd, Rname) :-
  write(Fdd, 'route('),
  write_string(Fdd, Rname),
  write(Fdd, ')'), nl(Fdd).

write_string(Fdd, Str) :-
  write(Fdd, '\''), write(Fdd, Str), write(Fdd, '\'').
write_attr(Fdd, Rname, Attr, Value) :-
  write(Fdd, Attr),
  write(Fdd, '('),
  write_string(Fdd, Rname),
  write(Fdd, ', '),
  write(Fdd, Value),
  write(Fdd, ').'),
  nl(Fdd).

write_lengths(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'rlength', First),
  write_lengths(Fdd, Rname, Rest).
write_lengths(_, _, []).

write_uphills(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'uphill', First),
  write_uphills(Fdd, Rname, Rest).
write_uphills(_, _, []).

write_levels(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'level', First),
  write_levels(Fdd, Rname, Rest).
write_levels(_, _, []).

write_historics(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'historic', First),
  write_historics(Fdd, Rname, Rest).
write_historics(_, _, []).

write_attractions(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'attractions', First),
  write_attractions(Fdd, Rname, Rest).
write_attractions(_, _, []).

write_castles(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'castles', First),
  write_castles(Fdd, Rname, Rest).
write_castles(_, _, []).

write_regions(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'region', First),
  write_regions(Fdd, Rname, Rest).
write_regions(_, _, []).

write_abroads(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'goes_abroad', First),
  write_abroads(Fdd, Rname, Rest).
write_abroads(_, _, []).

write_santiagos(Fdd, Rname, [First|Rest]) :-
  write_attr(Fdd, Rname, 'santiago', First),
  write_santiagos(Fdd, Rname, Rest).
write_santiagos(_, _, []).
