% Functions to manipulate / convert values from the database

convert_length(L, C, very_short) :-
  L < 80,
  C is 1.
convert_length(L, C, very_short) :-
  L >= 80,
  L < 100,
  C is 0.7.
convert_length(L, C, very_short) :-
  L >= 100,
  L < 120,
  C is 0.3.
convert_length(L, C, very_short) :-
  L >= 120,
  C is 0.
convert_length(L, C, short) :-
  L < 80,
  C is 0.
convert_length(L, C, short) :-
  L >= 80,
  L < 100,
  C is 0.3.
convert_length(L, C, short) :-
  L >= 100,
  L < 120,
  C is 0.7.
convert_length(L, C, short) :-
  L >= 120,
  L < 500,
  C is 1.
convert_length(L, C, short) :-
  L >= 500,
  L < 1000,
  C is 1 - 0.000001 * (L - 500) ^ 2.
convert_length(L, C, short) :-
  L >= 1000,
  C is 0.
convert_length(L, C, medium) :-
  L < 500,
  C is 0.
convert_length(L, C, medium) :-
  L >= 500,
  L < 1000,
  C is 0.000001 * (L - 500) ^ 2.
convert_length(L, C, medium) :-
  L >= 1000,
  L < 1300,
  C is 1.
convert_length(L, C, medium) :-
  L >= 1300,
  L < 1500,
  C is 1 - log(L / 1300).
convert_length(L, C, medium) :-
  L >= 1500,
  L < 3000,
  C is 0.5.
convert_length(L, C, medium) :-
  L >= 3000,
  C is 0.
convert_length(L, C, long) :-
  L < 1300,
  C is 0.
convert_length(L, C, long) :-
  L >= 1300,
  L < 1500,
  C is log(L / 1300).
convert_length(L, C, long) :-
  L >= 1500,
  L < 3000,
  C is 0.5.
convert_length(L, C, long) :-
  L >= 3000,
  C is 1.

convert_uphill(U, C, flat) :-
  U < 500,
  C is 1.
convert_uphill(U, C, flat) :-
  U >= 500,
  U < 1000,
  C is 0.5.
convert_uphill(U, C, flat) :-
  U >= 1000,
  C is 0.
convert_uphill(U, C, hilly) :-
  U < 500,
  C is 0.
convert_uphill(U, C, hilly) :-
  U >= 500,
  U < 1000,
  C is 0.5.
convert_uphill(U, C, hilly) :-
  U >= 1000,
  U < 30000,
  C is 1 - (1 / log(3)) * log(U / 10000).
convert_uphill(U, C, hilly) :-
  U >= 30000,
  C is 0.
convert_uphill(U, C, mountainous) :-
  U < 1000,
  C is 0.
convert_uphill(U, C, mountainous) :-
  U >= 1000,
  U < 30000,
  C is (1 / log(3)) * log(U / 10000).
convert_uphill(U, C, mountainous) :-
  U >= 30000,
  U < 40000,
  C is 1 - 1 / ((exp(3))*(e - 1)) * exp(0.0001 * U) + 1 / (e - 1).
convert_uphill(U, C, mountainous) :-
  U >= 40000,
  C is 0.
convert_uphill(U, C, very_mountainous) :-
  U < 30000,
  C is 0.
convert_uphill(U, C, very_mountainous) :-
  U >= 30000,
  U < 40000,
  C is 1/((exp(3)) * (e - 1)) * exp(0.0001 * U) - 1 / (e - 1).
convert_uphill(U, C, very_mountainous) :-
  U >= 40000,
  C is 1.

ask(A, N, V) :- ask(A, N, V, []).
ask(_, N, V, Hist) :-
	write(N),
	write('? (yes or no) '),
	get_user(V, Hist).

menuask(Name, AskValue, Menu) :- menuask(_, Name, AskValue, Menu).
menuask(Attribute, Name, AskValue, Menu) :- menuask(Attribute, Name, AskValue, Menu, []).
menuask(_, Name, AskValue, Menu, Hist) :-
	nl, write('What is the value for '), write(Name), write('?'), nl,
	display_menu(Menu),
	write('Enter the number of choice: '),
	get_user(Num, Hist), nl,
	pick_menu(Num, AskValue, Menu).

display_menu(Menu) :-
	disp_menu(1, Menu),
  !.

disp_menu(_, []).
disp_menu(N, [Item | Rest]) :-
	write(N), write('  : '), write(Item), nl,
	NN is N + 1,
	disp_menu(NN, Rest).

pick_menu(N, Val, Menu) :-
	integer(N),
	pic_menu(1, N, Val, Menu),
  !.
pick_menu(Val, Val, _).

pic_menu(_, _, none_of_the_above, []).
pic_menu(N, N, Item, [Item | _]).
pic_menu(Ctr, N, Val, [_ | Rest]) :-
	NextCtr is Ctr + 1,
	pic_menu(NextCtr, N, Val, Rest).

get_user(X, Hist) :-
	repeat,
	write('> '),
	read(X),
  process_ans(X, Hist),
  !.

process_ans(why, Hist) :-
	write_list(4, Hist), !, fail.
process_ans(_, _).
  
write_list(_, []).
write_list(N, [H | T]) :-
	tab(N), write(H), nl,
	write_list(N, T).

round(X,Y,D) :- Z is X * 10^D, round(Z, ZA), Y is ZA / 10^D.

what_length_cat(X) :- menuask(length_cat,'length category',X,[very_short,short,medium,long]).
what_uphill_cat(X) :- menuask(uphill_cat,'uphill category',X,[flat,hilly,mountainous,very_mountainous]).
what_level(X) :- menuask(level,'level',X,[1,2,3,4,5]).
if_historic(X) :- ask(historic,'historic',X).
what_attractions(X) :- menuask(attractions,'attractions',X,[none,antiquity,middle_ages,new_era]).
if_castles(X) :- ask(castles,'castles',X).
what_region(X) :- menuask(region,'region',X,[atlantic, loire_valley, masif_central, paris, mediterranean, provence, haute_savoie, pyrenees, brittany, alpes, jura]).
if_goes_abroad(X) :- ask(goes_abroad,'goes_abroad',X).
if_santiago(X) :- ask(santiago,'santiago',X).

multivalued(length_cat).
multivalued(uphill_cat).
multivalued(level).
multivalued(attractions).
multivalued(region).

fuzzy(rlength).
fuzzy(uphill).
fuzzy(length_cat).
fuzzy(uphill_cat).
binary(level).
binary(historic).
binary(attractions).
binary(castles).
binary(region).
binary(goes_abroad).
binary(santiago).

mutually_exclusive(attractions, none, attractions, antiquity).
mutually_exclusive(attractions, none, attractions, middle_ages).
mutually_exclusive(attractions, none, attractions, new_era).
mutually_exclusive(attractions, none, castles, yes).
mutually_exclusive(attractions, none, historic, yes).
mutually_exclusive(attractions, none, region, loire_valley).
mutually_exclusive(attractions, none, region, brittany).
mutually_exclusive(goes_abroad, no, santiago, yes).
mutually_exclusive(region, alpes, castles, yes).

mutually_exclusive(length_cat, very_short, length_cat, short).
mutually_exclusive(length_cat, very_short, length_cat, medium).
mutually_exclusive(length_cat, very_short, length_cat, long).
mutually_exclusive(length_cat, short, length_cat, medium).
mutually_exclusive(length_cat, short, length_cat, long).
mutually_exclusive(length_cat, medium, length_cat, long).

mutually_exclusive(uphill_cat, flat, uphill_cat, hilly).
mutually_exclusive(uphill_cat, flat, uphill_cat, mountainous).
mutually_exclusive(uphill_cat, flat, uphill_cat, very_mountainous).
mutually_exclusive(uphill_cat, hilly, uphill_cat, mountainous).
mutually_exclusive(uphill_cat, hilly, uphill_cat, very_mountainous).
mutually_exclusive(uphill_cat, mountainous, uphill_cat, very_mountainous).

mutually_exclusive(level, 1, level, 2).
mutually_exclusive(level, 1, level, 3).
mutually_exclusive(level, 1, level, 4).
mutually_exclusive(level, 1, level, 5).
mutually_exclusive(level, 2, level, 3).
mutually_exclusive(level, 2, level, 4).
mutually_exclusive(level, 2, level, 5).
mutually_exclusive(level, 3, level, 4).
mutually_exclusive(level, 3, level, 5).
mutually_exclusive(level, 4, level, 5).
