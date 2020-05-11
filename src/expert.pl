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
  consult('show_routes.pl'),
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
remove_attr(_, _, _).
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
	open('routes_gen_kb.pl', write, Fdd),
  write(Fdd, '% ROUTES - Grandes Randonnees data base'), nl(Fdd), nl(Fdd),
  findall(X, route(X), Rnames),
  write_routes(Fdd, Rnames),
	nl(Fdd),
  close(Fdd).
save_changes :-
  write('Could not save the database.'), nl.

restore_defaults :-
	style_check(-discontiguous),
	unload_file('routes_kb.pl'),
	consult('routes_default_kb.pl'),
	style_check(+discontiguous).
restore_defaults :-
  write('Could not restore the default database.'),nl.

:- main.
