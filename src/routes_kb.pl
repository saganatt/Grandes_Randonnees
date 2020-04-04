% ROUTES - a sample route identification system for use with the
% Native shell.
%
% top_goal where Native starts the inference.

top_goal(X) :- route(X).

% Loire valley
route(gr3) :-
  %	length(1273),
  % uphill(15350),
  length_cat(medium),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(atlantic),
  goes_abroad(no),
  santiago(no).
route(gr3) :-
  %	length(1273),
  % uphill(15350),
  length_cat(medium),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(loire_valley),
  goes_abroad(no),
  santiago(no).
route(gr3) :-
  %	length(1273),
  % uphill(15350),
  length_cat(medium),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(masif_central),
  goes_abroad(no),
  santiago(no).

% Atlantic - Mediterranean via Masif Central
route(gr4) :-
  %length(1528),
  %uphill(36324),
  length_cat(long),
  uphill_cat(mountainous),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(atlantic),
  goes_abroad(no),
  santiago(no).
route(gr4) :-
  %length(1528),
  %uphill(36324),
  length_cat(long),
  uphill_cat(mountainous),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(masif_central),
  goes_abroad(no),
  santiago(no).
route(gr4) :-
  %length(1528),
  %uphill(36324),
  length_cat(long),
  uphill_cat(mountainous),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(provence),
  goes_abroad(no),
  santiago(no).
route(gr4) :-
  %length(1528),
  %uphill(36324),
  length_cat(long),
  uphill_cat(mountainous),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(mediterranean),
  goes_abroad(no),
  santiago(no).

% Luxembourg - Nice
route(gr5) :-
  % length(1497),
  % uphill(57795),
  length_cat(medium),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(jura),
  goes_abroad(yes),
  santiago(no).
route(gr5) :-
  % length(1497),
  % uphill(57795),
  length_cat(medium),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(haute_savoie),
  goes_abroad(yes),
  santiago(no).
route(gr5) :-
  % length(1497),
  % uphill(57795),
  length_cat(medium),
  uphill_cat(very_mountainous),
  level(5),
  historic(no),
  attractions(none),
  castles(no),
  region(alpes),
  goes_abroad(yes),
  santiago(no).
route(gr5) :-
  % length(1497),
  % uphill(57795),
  length_cat(medium),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(mediterranean),
  goes_abroad(yes),
  santiago(no).
route(gr5) :-
  % length(1497),
  % uphill(57795),
  length_cat(medium),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(antiquity),
  castles(yes),
  region(mediterranean),
  goes_abroad(yes),
  santiago(no).

% Jura - Saint-Tropez
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(jura),
  goes_abroad(no),
  santiago(no).
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(haute_savoie),
  goes_abroad(no),
  santiago(no).
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(middle_ages),
  castles(yes),
  region(haute_savoie),
  goes_abroad(no),
  santiago(no).
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(alpes),
  goes_abroad(no),
  santiago(no).
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(provence),
  goes_abroad(no),
  santiago(no).
route(gr9) :-
  % length(958),
  % uphill(38524),
  length_cat(short),
  uphill_cat(mountainous),
  level(4),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(mediterranean),
  goes_abroad(no),
  santiago(no).

% Atlantic - Mediterranean via Pyrenees
route(gr10) :-
  % length(917),
  % uphill(55928),
  length_cat(short),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(atlantic),
  goes_abroad(yes),
  santiago(no).
route(gr10) :-
  % length(917),
  % uphill(55928),
  length_cat(short),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(pyrenees),
  goes_abroad(yes),
  santiago(no).
route(gr10) :-
  % length(917),
  % uphill(55928),
  length_cat(short),
  uphill_cat(very_mountainous),
  level(5),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(mediterranean),
  goes_abroad(yes),
  santiago(no).

% Brittany
route(gr34) :-
  % length(1987),
  % uphill(23369),
  length_cat(long),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(atlantic),
  goes_abroad(no),
  santiago(no).
route(gr34) :-
  % length(1987),
  % uphill(23369),
  length_cat(long),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(brittany),
  goes_abroad(no),
  santiago(no).
route(gr34) :-
  % length(1987),
  % uphill(23369),
  length_cat(long),
  uphill_cat(hilly),
  level(2),
  historic(yes),
  attractions(middle_ages),
  castles(yes),
  region(brittany),
  goes_abroad(no),
  santiago(no).

% Santiago de Compostela pilgrim trail
route(gr65) :-
  % length(773),
  % uphill(15369),
  length_cat(short),
  uphill_cat(hilly),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(masif_central),
  goes_abroad(yes),
  santiago(yes).
route(gr65) :-
  % length(773),
  % uphill(15369),
  length_cat(short),
  uphill_cat(hilly),
  level(3),
  historic(yes),
  attractions(middle_ages),
  castles(yes),
  region(masif_central),
  goes_abroad(yes),
  santiago(yes).
route(gr65) :-
  % length(773),
  % uphill(15369),
  length_cat(short),
  uphill_cat(hilly),
  level(3),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(pyrenees),
  goes_abroad(yes),
  santiago(yes).
route(gr65) :-
  % length(773),
  % uphill(15369),
  length_cat(short),
  uphill_cat(hilly),
  level(3),
  historic(yes),
  attractions(middle_ages),
  castles(yes),
  region(pyrenees),
  goes_abroad(yes),
  santiago(yes).

% Paris city trail
route(gr2024) :-
  % length(50),
  % uphill(677),
  length_cat(very_short),
  uphill_cat(flat),
  level(1),
  historic(yes),
  attractions(new_era),
  castles(yes),
  region(paris),
  goes_abroad(no),
  santiago(no).

convert_length(L, very_short) :-
  L < 100,
  !.
convert_length(L, short) :-
  L >= 100,
  L < 1000,
  !.
convert_length(L, medium) :-
  L >= 1000,
  L < 1500,
  !.
convert_length(L, long) :-
  L >= 1500.

convert_uphill(U, flat) :-
  U < 1000,
  !.
convert_uphill(U, hilly) :-
  U >= 1000,
  U < 30000,
  !.
convert_uphill(U, mountainous) :-
  U >= 30000,
  U < 50000,
  !.
convert_uphill(U, very_mountainous) :-
  U >= 50000.

length_cat(X) :- menuask(length_cat,'length category',X,[very_short,short,medium,long]).
uphill_cat(X) :- menuask(uphill_cat,'uphill category',X,[flat,hilly,mountainous,very_mountainous]).
level(X) :- menuask(level,'level',X,[1,2,3,4,5]).
historic(X) :- ask(historic,'historic',X).
attractions(X) :- menuask(attractions,'attractions',X,[none,antiquity,middle_ages,new_era]).
castles(X) :- ask(castles,'castles',X).
region(X) :- menuask(region,'region',X,[atlantic, loire_valley, masif_central, paris, mediterranean, provence, haute_savoie, pyrenees, brittany, alpes, jura]).
goes_abroad(X) :- ask(goes_abroad,'goes_abroad',X).
santiago(X) :- ask(santiago,'santiago',X).

multivalued(length_cat).
multivalued(uphill_cat).
multivalued(level).
multivalued(attractions).
multivalued(region).
