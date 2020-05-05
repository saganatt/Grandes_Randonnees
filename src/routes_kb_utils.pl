% Functions to manipulate / convert values from the database

convert_length(L, C, very_short) :-
  L < 80,
  C is 1.
convert_length(L, C, very_short) :-
  L >= 80
  L < 100,
  C is 0.7.
convert_length(L, C, very_short) :-
  L >= 100
  L < 120,
  C is 0.3.
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
  U >= 500
  U < 1000,
  C is 0.5.
convert_uphill(U, C, hilly) :-
  U >= 500
  U < 1000,
  C is 0.5.
convert_uphill(U, C, hilly) :-
  U >= 1000,
  U < 30000,
  C is 1 - (1 / log(3)) * log(U / 10000).
convert_uphill(U, C, mountainous) :-
  U >= 1000,
  U < 30000,
  C is (1 / log(3)) * log(U / 10000).
convert_uphill(U, C, mountainous) :-
  U >= 30000,
  U < 40000,
  C is 1 - 1 / ((exp(3))*(e - 1)) * exp(0.0001 * U) + 1 / (e - 1).
convert_uphill(U, C, very_mountainous) :-
  U >= 30000,
  U < 40000,
  C is 1/((exp(3)) * (e - 1)) * exp(0.0001 * U) - 1 / (e - 1).
convert_uphill(U, C, very_mountainous) :-
  U >= 40000
  C is 1.

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

fuzzy(length).
fuzzy(uphill).
fuzzy(level).
binary(historic).
binary(attractions).
binary(castles).
binary(region).
binary(goes_abroad).
binary(santiago).

mutually_exclusive(attractions, none, castles, yes).
mutually_exclusive(attractions, none, historic, yes).
mutually_exclusive(attractions, none, region, loire_valley).
mutually_exclusive(attractions, none, region, brittany).
mutually_exclusive(goes_abroad, no, santiago, yes).
mutually_exclusive(region, alpes, castles, yes).
