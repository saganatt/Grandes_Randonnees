:- dynamic fask/2, answer/2, answer/3.
:- dynamic length_cat/3.
:- dynamic uphill_cat/3.

main :-
	help,
  consult('routes_kb_utils.pl'),
	style_check(-discontiguous),
	consult('routes_kb.pl'),
	style_check(+discontiguous),
  prep_fuzzy,
	repeat,
	write('> '),
	read(X),
	do(X),
  help,
	X == quit.

do(help) :- help, !.
do(solve) :- solve, !.
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
  write('solve. - start reasoning'), nl,
  write('quit. - exit the program'), nl,
  write('<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<'), nl.

prep_fuzzy :-
  findall([X, Y], rlength(X, Y), Rlengths),
  findall([X, Y], uphill(X, Y), Ruphills),
  convert_lengths(Rlengths),
  convert_uphills(Ruphills).

convert_lengths([First|Rest]) :-
  [Rname|[L]] = First,
  convert_length(L, Cvshort, very_short),
  convert_length(L, Cshort, short),
  convert_length(L, Cmedium, medium),
  convert_length(L, Clong, long),
  assert((length_cat(Rname, Cvshort, very_short))),
  assert((length_cat(Rname, Cshort, short))),
  assert((length_cat(Rname, Cmedium, medium))),
  assert((length_cat(Rname, Clong, long))),
  convert_lengths(Rest).
convert_lengths([]).

convert_uphills([First|Rest]) :-
  [Rname|[U]] = First,
  convert_uphill(U, Cflat, flat),
  convert_uphill(U, Chilly, hilly),
  convert_uphill(U, Cmountainous, mountainous),
  convert_uphill(U, Cvmountainous, very_mountainous),
  assert((uphill_cat(Rname, Cflat, flat))),
  assert((uphill_cat(Rname, Chilly, hilly))),
  assert((uphill_cat(Rname, Cmountainous, mountainous))),
  assert((uphill_cat(Rname, Cvmountainous, very_mountainous))),
  convert_uphills(Rest).
convert_uphills([]).

solve :-
	find_routes(Routes),
  write('==========================================================='), nl,
	writeln('Your best matching routes are:'), nl,
	write_results(Routes),
  write('==========================================================='), nl,
	retractall(answer(_,_)), !.

write_results([]) :- nl, write('No more matching routes'), nl.
write_results([[Rname, C]|Rest]):-
	format('~w   ~w', [Rname, C]), nl,
	write_results(Rest).

find_routes(Rnames):- 
	findall([Rname, C], check_route(Rname, C), List),
	sort(2, >=, List, Rnames).

check_route(Rname, Cfinal):- 
	route(Rname),
  bagof([C, Y], Rname^length_cat(Rname, C, Y), Rlengths),
  bagof([C, Y], Rname^uphill_cat(Rname, C, Y), Ruphills),
  bagof(Y, Rname^level(Rname, Y), Rlevels),
  historic(Rname, Rhistoric),
  bagof(Y, Rname^attractions(Rname, Y), Rattractions),
  castles(Rname, Rcastle),
  bagof(Y, Rname^region(Rname, Y), Rregions),
  goes_abroad(Rname, Rabroad),
  santiago(Rname, Rsantiago),
  Attributes = [[length_cat, Rlengths], [uphill_cat, Ruphills], [level, Rlevels], [historic, Rhistoric], [attractions, Rattractions], [castles,Rcastle], [region, Rregions], [goes_abroad, Rabroad], [santiago, Rsantiago]],
	check_attributes(Rname, Attributes, 1.0, Cexact),
	round(Cexact, Cfinal, 2).

check_attributes(Rname, [[Attr, Value| _] | Rest], C, Cfinal) :-
  check_attribute(Attr, Value, Result),
  Newc is min(C, Result),
  Newc >= 0.5,
  check_attributes(Rname, Rest, Newc, Cfinal).
check_attributes(_, [], C, Cfinal) :- Cfinal = C.

check_attribute(Attr, yes, Result) :-
  not(multivalued(Attr)),
	get_answer(Attr, Ans),
  Result is Ans, !.
check_attribute(Attr, no, Result) :-
  not(multivalued(Attr)),
	get_answer(Attr, Ans),
  Result is 1 - Ans, !.
check_attribute(Attr, Values, Result) :-
  multivalued(Attr),
  get_answer(Attr, Ans),
  find_ans(Attr, Values, Ans, Ccont),
  Result is Ccont, !.

find_ans(Attr, Values, Ans, Ccont) :-
  binary(Attr),
  member(Ans, Values),
  Ccont = 1.
find_ans(Attr, Values, Ans, Ccont) :-
  binary(Attr),
  not(member(Ans, Values)),
  Ccont = 0.
find_ans(Attr, Values, Ans, Ccont) :-
  fuzzy(Attr),
  bagof(C, member([C, Ans], Values), Certs),
  C = 1,
  combine_certs(Certs, C, Ccont).

combine_certs([First|Rest], C, Ccont) :-
  Newc is min(C, First),
  combine_certs(Rest, Newc, Ccont).
combine_certs([], C, Ccont) :- Ccont is C.

get_answer(Attribute, Answer) :-
  not(multivalued(Attribute)),
  get_num_ans(Attribute, Answer), !.
get_answer(Attribute, Answer) :-
  not(multivalued(Attribute)),
	fask(Attribute, _),
  get_num_ans(Attribute, Answer), !.
get_answer(Attribute, Answer) :-
  multivalued(Attribute),
	answer(Attribute, Answer, 1), !.
get_answer(Attribute, Answer) :-
  multivalued(Attribute),
	fask(Attribute, Answer),
	answer(Attribute, Answer, 1), !.

get_num_ans(Attr, Res) :- answer(Attr, 0), Res = 0.
get_num_ans(Attr, Res) :- answer(Attr, 1), Res = 1.

fask(Attribute) :- fask(Attribute, _).
fask(length_cat, Rlength_cat) :-
  what_length_cat(Rlength_cat),
  remember(length_cat, Rlength_cat).
fask(uphill_cat, Ruphill_cat) :-
  what_uphill_cat(Ruphill_cat),
  remember(uphill_cat, Ruphill_cat).
fask(level, Rlevel) :-
  what_level(Rlevel),
  remember(level, Rlevel).
fask(historic, Rhistoric) :-
  if_historic(Rhistoric),
  remember(historic, Rhistoric).
fask(attractions, Rattractions) :-
  what_attractions(Rattractions),
  remember(attractions, Rattractions).
fask(castles, Rcastles) :-
  if_castles(Rcastles),
  remember(castles, Rcastles).
fask(region, Rregion) :-
  what_region(Rregion),
  remember(region, Rregion).
fask(goes_abroad, Rabroad) :-
  if_goes_abroad(Rabroad),
  remember(goes_abroad, Rabroad).
fask(santiago, Rsantiago) :-
  if_santiago(Rsantiago),
  remember(santiago, Rsantiago).

remember(Attribute, yes) :- 
	not(multivalued(Attribute)),
	assert(answer(Attribute, 1)),
  exclude_no_fail(Attribute, yes).
remember(Attribute, no) :- 
	not(multivalued(Attribute)),
	assert(answer(Attribute, 0)),
  exclude_no_fail(Attribute, no).
remember(Attribute, Answer) :-
	multivalued(Attribute),
	assert(answer(Attribute, Answer, 1)),
  exclude_no_fail(Attribute, Answer).

exclude_no_fail(Attribute, Answer) :-
	bagof([X, Y], Attribute^Answer^mutually_exclusive(Attribute, Answer, X, Y), Excluded),
	bagof([X, Y], Attribute^Answer^mutually_exclusive(X, Y, Attribute, Answer), Excludedr),
  get_exclusives(Excluded),
  get_exclusives(Excludedr).
exclude_no_fail(Attribute, Answer) :-
	bagof([X, Y], Attribute^Answer^mutually_exclusive(Attribute, Answer, X, Y), Excluded),
  get_exclusives(Excluded).
exclude_no_fail(Attribute, Answer) :-
	bagof([X, Y], Attribute^Answer^mutually_exclusive(X, Y, Attribute, Answer), Excludedr),
  get_exclusives(Excludedr).
exclude_no_fail(_, _).

get_exclusives([]).
get_exclusives([Attribute|Rest]) :-
  get_exclusive(Attribute),
  get_exclusives(Rest).

get_exclusive([Attribute, no]) :- answer(Attribute, 0).
get_exclusive([Attribute, yes]) :- answer(Attribute, 1).
get_exclusive([Attribute, no]) :- assert(answer(Attribute, 1)).
get_exclusive([Attribute, yes]) :- assert(answer(Attribute, 0)).
get_exclusive([Attribute, Ans]) :- multivalued(Attribute), answer(Attribute, Ans, 0).
get_exclusive([Attribute, Ans]) :- multivalued(Attribute), assert(answer(Attribute, Ans, 0)).

:- main.
