:- dynamic route/1.
:- dynamic rlength/2.
:- dynamic uphill/2.
:- dynamic level/2.
:- dynamic historic/2.
:- dynamic attractions/2.
:- dynamic castles/2.
:- dynamic region/2.
:- dynamic goes_abroad/2.
:- dynamic santiago/2.
:- dynamic fuzzy/1.
:- dynamic binary/1.
:- dynamic multivalued/1.
:- dynamic mutually_exclusive/4.

main :-
	help,
  consult('routes_kb_utils.pl'),
	repeat,
	write('> '),
	read(X),
	do(X),
  help,
	X == quit.

do(help) :- help, !.
do(load) :- load_kb, !.
do(add_route) :- add_route, !.
do(delete_route) :- delete_route, !.
do(delete_all_routes) :- delete_all_routes, !.
do(edit_route) :- edit_route, !.
do(save_changes) :- save_changes, !.
do(restore_defaults) :- restore_defaults, !.
do(show_route) :- show_route, !.
do(show_all_routes) :- show_all_routes, !.
do(quit) :- halt(0).
do(end_of_file) :- halt(0).
do(X) :-
	write(X),
	write(' is not a legal command.'), nl,
	fail.

help :-
  write('>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'), nl,
	write('Running Grandes Randonnees...'), nl,
	nl,
	write('OPTIONS:'), nl,
  write('help. - show this help'), nl,
  write('load. - load a database'), nl,
  write('add_route. - add a route to the database'), nl,
  write('delete_route. - delete a route from the database'), nl,
  write('delete_all_routes. - delete all routes from the database'), nl,
  write('edit_route. - edit a route'), nl,
  write('save_changes. - save database changes to disc'), nl,
  write('restore_defaults. - restore database to default'), nl,
  write('show_route. - show all information about given route'), nl,
  write('show_all_routes. - show all information about all routes'), nl,
  write('quit. - exit the program'), nl,
  write('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'), nl.

load_kb :-
	style_check(-discontiguous),
	consult('routes_kb.pl'),
	style_check(+discontiguous).
load_kb :-
  write('Could not load the database.'), nl.

add_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  add_route(Rname).
add_route :-
  write('Could not add the route.'),nl.

add_route(Rname) :-
	route(Rname),
  write('Route already exist'),
  nl.
add_route(Rname) :-
  not(route(Rname)),
  write('Enter the length of route in km: '),
  read(Rlength),
  write('Enter elevation difference (the uphill value) in m: '),
  read(Ruphill),
  what_level(Rlevel),
  if_historic(Rhistoric),
  what_attractions(Rattractions),
  if_castles(Rcastles),
  what_region(Rregion),
  if_goes_abroad(Rabroad),
  if_santiago(Rsantiago),
  assert((route(Rname))),
  assert((rlength(Rname, Rlength))),
  assert((uphill(Rname, Ruphill))),
  assert((level(Rname, Rlevel))),
  assert((historic(Rname, Rhistoric))),
  assert((attractions(Rname, Rattractions))),
  assert((castles(Rname, Rcastles))),
  assert((region(Rname, Rregion))),
  assert((goes_abroad(Rname, Rabroad))),
  assert((santiago(Rname, Rsantiago))),
  write('Route added.'),nl.

delete_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  retractall(route(Rname)),
  retractall(rlength(Rname, _)),
  retractall(uphill(Rname, _)),
  retractall(level(Rname, _)),
  retractall(historic(Rname, _)),
  retractall(attractions(Rname, _)),
  retractall(castles(Rname, _)),
  retractall(region(Rname, _)),
  retractall(goes_abroad(Rname, _)),
  retractall(santiago(Rname, _)),
  write('Route deleted.'), nl.
delete_route :-
  write('Could not delete the route.'),nl.

delete_all_routes :-
  retractall(route(_)),
  retractall(rlength(_, _)),
  retractall(uphill(_, _)),
  retractall(level(_, _)),
  retractall(historic(_, _)),
  retractall(attractions(_, _)),
  retractall(castles(_, _)),
  retractall(region(_, _)),
  retractall(goes_abroad(_, _)),
  retractall(santiago(_, _)),
  write('Routes deleted.'), nl.
delete_all_routes :-
  write('Could not delete routes.').

edit_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  write('Enter feature to edit in single quotes: '),
  read(Rattr),
  write('Enter new value: '),
  read(Rvalue),
  write('Remove old value(s)? (yes/no)'),
  read(Ans),
  remove_attr(Ans, Rname, Rattr),
  set_attr(Rattr, Rname, Rvalue),
  write('Route edited.'), nl.
edit_route :-
  write('Could not edit the route.'),nl.

set_attr('length', Rname, Rvalue) :-
  assert((rlength(Rname, Rvalue))).
set_attr('uphill', Rname, Rvalue) :-
  assert((uphill(Rname, Rvalue))).
set_attr('level', Rname, Rvalue) :-
  assert((level(Rname, Rvalue))).
set_attr('historic', Rname, Rvalue) :-
  assert((historic(Rname, Rvalue))).
set_attr('attractions', Rname, Rvalue) :-
  assert((attractions(Rname, Rvalue))).
set_attr('castles', Rname, Rvalue) :-
  assert((castles(Rname, Rvalue))).
set_attr('regions', Rname, Rvalue) :-
  assert((region(Rname, Rvalue))).
set_attr('goes_abroad', Rname, Rvalue) :-
  assert((goes_abroad(Rname, Rvalue))).
set_attr('santiago', Rname, Rvalue) :-
  assert((santiago(Rname, Rvalue))).
set_attr(_, _, _) :-
  write('Feature does not exist'),nl.

remove_attr(yes, Rname, Rattr) :-
  remove_attr(Rname, Rattr),
  write('Attribute '), write(Rattr), write(' removed.'), nl.
remove_attr(_, _, _) :- fail.
remove_attr(Rname, 'length') :-
  retractall(rlength(Rname, _)).
remove_attr(Rname, 'uphill') :-
  retractall(uphill(Rname, _)).
remove_attr(Rname, 'level') :-
  retractall(level(Rname, _)).
remove_attr(Rname, 'historic') :-
  retractall(historic(Rname, _)).
remove_attr(Rname, 'attractions') :-
  retractall(attractions(Rname, _)).
remove_attr(Rname, 'castles') :-
  retractall(castles(Rname, _)).
remove_attr(Rname, 'region') :-
  retractall(region(Rname, _)).
remove_attr(Rname, 'goes_abroad') :-
  retractall(goes_abroad(Rname, _)).
remove_attr(Rname, 'santiago') :-
  retractall(santiago(Rname, _)).
remove_attr(_, _) :-
  write('Feature does not exist'),nl.

save_changes :-
	open('routes_kb_gen.pl', write, Fdd),
	nl(Fdd),
  write_routes(Fdd),
	nl(Fdd),
  close(Fdd).
save_changes :-
  write('Could not save the database.'), nl.

write_routes(Fdd) :-
  findall(X, route(X), Rnames),
  findall([X, Y], rlength(X, Y), Rlengths),
  findall([X, Y], uphill(X, Y), Ruphills),
  findall([X, Y], level(X, Y), Rlevels),
  findall([X, Y], historic(X, Y), Rhistorics),
  findall([X, Y], attractions(X, Y), Rattractions),
  findall([X, Y], castles(X, Y), Rcastles),
  findall([X, Y], region(X, Y), Rregions),
  findall([X, Y], goes_abroad(X, Y), Rabroads),
  findall([X, Y], santiago(X, Y), Rsantiagos),
  write_rnames(Fdd, Rnames),
  write_lengths(Fdd, Rlengths),
  write_uphills(Fdd, Ruphills),
  write_levels(Fdd, Rlevels),
  write_historics(Fdd, Rhistorics),
  write_attractions(Fdd, Rattractions),
  write_castles(Fdd, Rcastles),
  write_regions(Fdd, Rregions),
  write_abroads(Fdd, Rabroads),
  write_santiagos(Fdd, Rsantiagos),
  nl(Fdd).

write_rnames(Fdd, [First|Rest]) :-
  write(Fdd, 'route('),
  write_string(Fdd, First),
  write(Fdd, ')'), nl(Fdd),
  write_rnames(Fdd, Rest).
write_rnames(_, []).

write_string(Fdd, Rname) :-
  write(Fdd, '\''), write(Fdd, Rname), write(Fdd, '\'').
write_attr(Fdd, Rname, Attr, [Value]) :-
  write(Fdd, Attr),
  write(Fdd, '('),
  write_string(Fdd, Rname),
  write(Fdd, ', '),
  write(Fdd, Value),
  write(Fdd, ').'),
  nl(Fdd).

write_lengths(Fdd, [First|Rest]) :-
  write_length(Fdd, First),
  write_lengths(Fdd, Rest).
write_lengths(_, []).
write_length(Fdd, [Rname|Rlength]) :-
  write_attr(Fdd, Rname, 'rlength', Rlength).

write_uphills(Fdd, [First|Rest]) :-
  write_uphill(Fdd, First),
  write_uphills(Fdd, Rest).
write_uphills(_, []).
write_uphill(Fdd, [Rname|Ruphill]) :-
  write_attr(Fdd, Rname, 'uphill', Ruphill).

write_levels(Fdd, [First|Rest]) :-
  write_level(Fdd, First),
  write_levels(Fdd, Rest).
write_levels(_, []).
write_level(Fdd, [Rname|Rlevel]) :-
  write_attr(Fdd, Rname, 'level', Rlevel).

write_historics(Fdd, [First|Rest]) :-
  write_historic(Fdd, First),
  write_historics(Fdd, Rest).
write_historics(_, []).
write_historic(Fdd, [Rname|Rhistoric]) :-
  write_attr(Fdd, Rname, 'historic', Rhistoric).

write_attractions(Fdd, [First|Rest]) :-
  write_attraction(Fdd, First),
  write_attractions(Fdd, Rest).
write_attractions(_, []).
write_attraction(Fdd, [Rname|Rattraction]) :-
  write_attr(Fdd, Rname, 'attraction', Rattraction).

write_castles(Fdd, [First|Rest]) :-
  write_castle(Fdd, First),
  write_castles(Fdd, Rest).
write_castles(_, []).
write_castle(Fdd, [Rname|Rcastle]) :-
  write_attr(Fdd, Rname, 'castle', Rcastle).

write_regions(Fdd, [First|Rest]) :-
  write_region(Fdd, First),
  write_regions(Fdd, Rest).
write_regions(_, []).
write_region(Fdd, [Rname|Rregion]) :-
  write_attr(Fdd, Rname, 'region', Rregion).

write_abroads(Fdd, [First|Rest]) :-
  write_abroad(Fdd, First),
  write_abroads(Fdd, Rest).
write_abroads(_, []).
write_abroad(Fdd, [Rname|Rabroad]) :-
  write_attr(Fdd, Rname, 'goes_abroad', Rabroad).

write_santiagos(Fdd, [First|Rest]) :-
  write_santiago(Fdd, First),
  write_santiagos(Fdd, Rest).
write_santiagos(_, []).
write_santiago(Fdd, [Rname|Rsantiago]) :-
  write_attr(Fdd, Rname, 'santiago', Rsantiago).

restore_defaults :-
	style_check(-discontiguous),
	unload_file('routes_kb.pl'),
	consult('routes_default_kb.pl'),
	style_check(+discontiguous).
restore_defaults :-
  write('Could not restore the default database.'),nl.

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

:- main.
