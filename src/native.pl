% Native - a simple shell for use with Prolog
% knowledge bases.  It includes expanations.

% fixed for SWI Prolog by GJN 12.2005

:-dynamic(known/3).

:-op(900,xfy, :).

main :-
	greeting,
  open('native_debug.txt',write,Fd,[alias(debug)]),
  % open('debug.txt',write,Fd),
  % set_prolog_IO(current_input,current_output,Fd),
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
do(solve) :- solve, !.
do(how(Goal)) :- how(Goal), !.
do(whynot(Goal)) :- whynot(Goal), !.
do(quit).
do(X) :-
	write(X),
	write(' is not a legal command.'), nl,
	fail.

native_help :-
	write('Type help. load. solve. how(Goal). whynot(Goal). or quit.'),nl,
	write('at the prompt.'), nl.

load_kb :-
	write('Enter file name in single quotes (ex. ''birds.nkb''.): '),
	read(F),
	consult(F).

solve :-
	retractall(known),
	prove(top_goal(X),[]),
	write('The answer is '),write(X),nl.
solve :-
	write('No answer found.'),nl.
	
ask(Attribute,_,Value,_) :-
	known(yes,Attribute,Value),     % succeed if we know its true
	!.                              % and dont look any further
ask(Attribute,_,Value,_) :-
	known(_,Attribute,Value),       % fail if we know its false
	!, fail.

ask(Attribute,_,_,_) :-
	\+ multivalued(Attribute),
	known(yes,Attribute,_),         % fail if its some other value.
	!, fail.                        % the cut in clause #1 ensures
					                        % this is the wrong value
ask(A,N,V,Hist) :-
	write(N :V),                    % if we get here, we need to ask.
	write('? (yes or no) '),
	get_user(Y,Hist),               % get the answer
	asserta(known(Y,A,V)),          % remember it so we dont ask again.
	Y = yes.                        % succeed or fail based on answer.

% "menuask" is like ask, only it gives the user a menu to to choose
% from rather than a yes on no answer.  In this case there is no
% need to check for a negative since "menuask" ensures there will
% be some positive answer.

menuask(Attribute,_,Value,_,_) :-
	known(yes,Attribute,Value),     % succeed if we know
	!.
menuask(Attribute,_,_,_,_) :-
	known(yes,Attribute,_),         % fail if its some other value
	!, fail.

menuask(Attribute,Name,AskValue,Menu,Hist) :-
	nl,write('What is the value for '),write(Name),write('?'),nl,
	display_menu(Menu),
	write('Enter the number of choice> '),
	get_user(Num,Hist),nl,
	pick_menu(Num,AnswerValue,Menu),
	asserta(known(yes,Attribute,AnswerValue)),
  write(debug, 'answer value: '),write(debug, AnswerValue),nl(debug),
	AskValue = AnswerValue.         % succeed or fail based on answer

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

prove(true,_) :- !.
prove((Goal,Rest),Hist) :-
  write(debug, 'prove goal: '),write(debug, Goal),nl(debug),
  write(debug, 'prove rest: '),write(debug, Rest),nl(debug),
  write(debug, 'prove hist: '),write(debug, Hist),nl(debug),
	prov(Goal,[Goal|Hist]),
  write(debug, 'prove hist after prov: '),write(debug, Hist),nl(debug),
	prove(Rest,Hist).
prove(Goal,Hist) :-
  write(debug, 'prove2 goal: '),write(debug, Goal),nl(debug),
  write(debug, 'prove2 hist: '),write(debug, Hist),nl(debug),
	prov(Goal,[Goal|Hist]).

prov(true,_) :- !.
prov(menuask(X,Y,Z),Hist) :-
  write(debug, 'prov with menuask'),nl(debug),
  write(debug, 'prov x: '),write(debug, X),nl(debug),
  write(debug, 'prov y: '),write(debug, Y),nl(debug),
  write(debug, 'prov z: '),write(debug, Z),nl(debug),
  write(debug, 'prov Hist: '),write(debug, Hist),nl(debug),
  menuask(X,Y,Z,Hist), !.
prov(ask(X,Y),Hist) :-
  write(debug, 'prov with ask'),nl(debug),
  write(debug, 'prov x: '),write(debug, X),nl(debug),
  write(debug, 'prov y: '),write(debug, Y),nl(debug),
  write(debug, 'prov Hist: '),write(debug, Hist),nl(debug),
  ask(X,Y,Hist), !.
prov(Goal,Hist) :-
  write(debug, 'prov with goal'),nl(debug),
  write(debug, 'prov goal: '),write(debug, Goal),nl(debug),
  write(debug, 'prov Hist: '),write(debug, Hist),nl(debug),
	clause(Goal,Body),
  write(debug, 'prov Body: '),write(debug, Body),nl(debug),
	prove(Body,Hist).

% Explanations

how(Goal) :-
	clause(Goal,Body),
	prove(Body,[]),
	write_body(4,Body).

whynot(Goal) :-
	clause(Goal,Body),
	write_line([Goal,'fails because: ']),
	explain(Body).
whynot(_).

explain(true).
explain((Head,Body)) :-
	check(Head),
	explain(Body).

check(H) :- prove(H,[]), write_line([H,succeeds]), !.
check(H) :- write_line([H,fails]), fail.

write_list(_,[]).
write_list(N,[H|T]) :-
	tab(N),write(H),nl,
	write_list(N,T).

write_body(N,(First,Rest)) :-
	tab(N),write(First),nl,
	write_body(N,Rest).
write_body(N,Last) :-
	tab(N),write(Last),nl.

write_line(L) :-
	flatten(L,LF),
	write_lin(LF).
	
write_lin([]) :- nl.
write_lin([H|T]) :-
	write(H), tab(1),
	write_lin(T).

flatten([],[]) :- !.
flatten([[]|T],T2) :-
	flatten(T,T2), !.
flatten([[X|Y]|T], L) :-
	flatten([X|[Y|T]],L), !.
flatten([H|T],[H|T2]) :-
	flatten(T,T2).
                                                                    
