% Based on Native - a simple shell for use with Prolog
% knowledge bases.

%:-dynamic(known/3).

main :-
	greeting,
  open('expert_debug.txt',write,Fd,[alias(debug)]),
	repeat,
	write('> '),
	read(X),
	do(X),
  native_help,
	X == quit,
  close(Fd).

greeting :-
	write('This is the native Prolog shell.'), nl,
	native_help.

do(help) :- native_help, !.
do(load) :- load_kb, !.
do(add_route) :- add_route, !.
do(delete_route) :- delete_route, !.
do(delete_all_routes) :- delete_all_routes, !.
do(edit_route) :- edit_route, !.
do(show_route) :- show_route, !.
do(show_all_routes) :- show_all_routes, !.
do(quit).
do(X) :-
	write(X),
	write(' is not a legal command.'), nl,
	fail.

native_help :-
	write('Type help. load. add_route. delete_route. delete_all_routes. edit_route. show_route. show_all_routes. or quit.'),nl,
	write('at the prompt.'), nl.

load_kb :-
	write('Enter file name in single quotes (ex. ''birds.nkb''.): '),
	read(F),
	consult(F).

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
	write('Enter the number of choice> '),
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
  write('Enter the length of route in km: '),
  read(Rlength),
  convert_length(Rlength, Rlength_cat),
  write('Enter elevation difference (the uphill value) in m: '),
  read(Ruphill),
  convert_uphill(Ruphill, Ruphill_cat),
  level(Rlevel),
  historic(Rhistoric),
  attractions(Rattractions),
  castles(Rcastles),
  region(Rregion),
  goes_abroad(Rabroad),
  santiago(Rsantiago),
  assert((
  route(Rname) :-
    length_cat(Rlength_cat),
    uphill_cat(Ruphill_cat),
    level(Rlevel),
    historic(Rhistoric),
    attractions(Rattractions),
    castles(Rcastles),
    region(Rregion),
    goes_abroad(Rabroad),
    santiago(Rsantiago)
  )),
  write('Route saved.'),nl.
add_route :-
  write('Could not add the route.'),nl.

delete_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  retractall(route(Rname) :- _).
delete_route :-
  write('Could not delete the route.'),nl.

delete_all_routes :-
  retractall(route(_) :- _).
delete_all_routes :-
  write('Could not delete routes.').

edit_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  retract(route(Rname) :- _).
edit_route :-
  write('Could not edit the route.'),nl.

show_route :-
  write('Enter route name in single quotes: '),
  read(Rname),
  listing(route(Rname)).
show_route :-
  write('Could not display the route.'),nl.

show_all_routes :-
  listing(route(_)).
show_all_routes :-
  write('Could not display routes.'),nl.
