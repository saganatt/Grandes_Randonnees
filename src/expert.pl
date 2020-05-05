main :-
	greeting,
  consult('routes_kb_utils.pl'),
  open('expert_debug.txt',write,Fd,[alias(debug)]),
	repeat,
	write('> '),
	read(X),
	do(X),
  help,
	X == quit,
  close(Fd).

greeting :-
	write('This is the native Prolog shell.'), nl,
	help.

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

ask(A,N,V) :- ask(A,N,V,[]).
ask(_,N,V,Hist) :-
	write(N),
	write('? (yes or no) '),
	get_user(V,Hist).

menuask(Name,AskValue,Menu) :- menuask(_,Name,AskValue,Menu).
menuask(Attribute,Name,AskValue,Menu) :- menuask(Attribute,Name,AskValue,Menu,[]).
menuask(_,Name,AskValue,Menu,Hist) :-
	nl,write('What is the value for '),write(Name),write('?'),nl,
	display_menu(Menu),
	write('Enter the number of choice: '),
	get_user(Num,Hist),nl,
	pick_menu(Num,AskValue,Menu).

display_menu(Menu) :-
	disp_menu(1,Menu), !.             % make sure we fail on backtracking

disp_menu(_,[]).
disp_menu(N,[Item | Rest]) :-            % recursively write the head of
	write(N),write('  : '),write(Item),nl, % the list and disp_menu the tail
	NN is N + 1,
	disp_menu(NN,Rest).

pick_menu(N,Val,Menu) :-
	integer(N),                     % make sure they gave a number
	pic_menu(1,N,Val,Menu), !.      % start at one
pick_menu(Val,Val,_).             % if they didn't enter a number, use
	                                % what they entered as the value

pic_menu(_,_,none_of_the_above,[]).  % if we've exhausted the list
pic_menu(N,N, Item, [Item|_]).       % the counter matches the number
pic_menu(Ctr,N, Val, [_|Rest]) :-
	NextCtr is Ctr + 1,                % try the next one
	pic_menu(NextCtr, N, Val, Rest).

get_user(X,Hist) :-
	repeat,
	write('> '),
	read(X),
  process_ans(X,Hist), !.

process_ans(why,Hist) :-
	write_list(4,Hist), !, fail.
process_ans(_,_).
  
write_list(_,[]).
write_list(N,[H|T]) :-
	tab(N),write(H),nl,
	write_list(N,T).

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
  convert_length(Rlength, Rlength_cat),
  write('Enter elevation difference (the uphill value) in m: '),
  read(Ruphill),
  convert_uphill(Ruphill, Ruphill_cat),
  what_level(Rlevel),
  if_historic(Rhistoric),
  what_attractions(Rattractions),
  if_castles(Rcastles),
  what_region(Rregion),
  if_goes_abroad(Rabroad),
  if_santiago(Rsantiago),
  assert((route(Rname))),
  assert((length_cat(Rname, Rlength_cat))),
  assert((uphill_cat(Rname, Ruphill_cat))),
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
  retractalll(length_cat(Rname, _)),
  retractalll(uphill_cat(Rname, _)),
  retractalll(level(Rname, _)),
  retractalll(historic(Rname, _)),
  retractalll(attractions(Rname, _)),
  retractalll(castles(Rname, _)),
  retractalll(region(Rname, _)),
  retractalll(goes_abroad(Rname, _)),
  retractalll(santiago(Rname, _)),
  write('Route deleted.'), nl.
delete_route :-
  write('Could not delete the route.'),nl.

delete_all_routes :-
  retractall(route(_)),
  retractalll(length_cat(_, _)),
  retractalll(uphill_cat(_, _)),
  retractalll(level(_, _)),
  retractalll(historic(_, _)),
  retractalll(attractions(_, _)),
  retractalll(castles(_, _)),
  retractalll(region(_, _)),
  retractalll(goes_abroad(_, _)),
  retractalll(santiago(_, _)),
  write('Routes deleted.'), nl.
delete_all_routes :-
  write('Could not delete routes.').

edit_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  write('Enter feature to edit: '),
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

set_attr('length_cat', Rname, Rvalue) :-
  assert((length_cat(Rname, Rvalue))).
set_attr('uphill_cat', Rname, Rvalue) :-
  assert((uphill_cat(Rname, Rvalue))).
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
remove_attr(Rname, 'length_cat') :-
  retractall(length_cat(Rname, _)).
remove_attr(Rname, 'uphill_cat') :-
  retractall(uphill_cat(Rname, _)).
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
  findall(X, route(X), Rnames),
  write_routes(Fdd, Rnames),
	nl(Fdd),
  close(Fdd).
save_changes :-
  write('Could not save the database.'), nl.

write_routes(Fdd, [First|Rest]) :-
  write_route(Fdd, First),
	write_routes(Fdd, Rest).
write_routes(_, []).

write_route(Fdd, Rname) :-
  findall(X, length_cat(Rname, X), Rlengths),
  findall(X, uphill_cat(Rname, X), Ruphills),
  findall(X, level(Rname, X), Rlevels),
  findall(X, historic(Rname, X), Rhistorics),
  findall(X, attractions(Rname, X), Rattractions),
  findall(X, castles(Rname, X), Rcastles),
  findall(X, region(Rname, X), Rregions),
  findall(X, goes_abroad(Rname, X), Rabroads),
  findall(X, santiago(Rname, X), Rsantiagos),
	write(Fdd, 'route('), write(Rname), write(Fdd, ').'), nl(Fdd),
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

write_lengths(Fdd, [First|Rest]) :-
  write(Fdd, 'length_cat('), write(First), write(Fdd, ').'), nl(Fdd),
  write_lengths(Fdd, Rest).
write_lengths(_, []).

write_uphills(Fdd, [First|Rest]) :-
  write(Fdd, 'uphill_cat('), write(First), write(Fdd, ').'), nl(Fdd),
  write_uphills(Fdd, Rest).
write_uphills(_, []).

write_levels(Fdd, [First|Rest]) :-
  write(Fdd, 'level('), write(First), write(Fdd, ').'), nl(Fdd),
  write_levels(Fdd, Rest).
write_levels(_, []).

write_historics(Fdd, [First|Rest]) :-
  write(Fdd, 'historic('), write(First), write(Fdd, ').'), nl(Fdd),
  write_historics(Fdd, Rest).
write_historics(_, []).

write_attractions(Fdd, [First|Rest]) :-
  write(Fdd, 'attraction('), write(First), write(Fdd, ').'), nl(Fdd),
  write_attractions(Fdd, Rest).
write_attractions(_, []).

write_castles(Fdd, [First|Rest]) :-
  write(Fdd, 'castle('), write(First), write(Fdd, ').'), nl(Fdd),
  write_castles(Fdd, Rest).
write_castles(_, []).

write_regions(Fdd, [First|Rest]) :-
  write(Fdd, 'region('), write(First), write(Fdd, ').'), nl(Fdd),
  write_regions(Fdd, Rest).
write_regions(_, []).

write_abroads(Fdd, [First|Rest]) :-
  write(Fdd, 'goes_abroad('), write(First), write(Fdd, ').'), nl(Fdd),
  write_abroads(Fdd, Rest).
write_abroads(_, []).

write_santiagos(Fdd, [First|Rest]) :-
  write(Fdd, 'santiago('), write(First), write(Fdd, ').'), nl(Fdd),
  write_santiagos(Fdd, Rest).
write_santiagos(_, []).

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
