:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> integer(L), "\n", lines(Ls).
lines([L]) --> integer(L).
lines([]) --> [].

partone(Numbers, Result):-
 member(X, Numbers),
 member(Y, Numbers),
 2020 is X + Y,
 Result is X * Y.

parttwo(Numbers, Result):-
 member(X, Numbers),
 member(Y, Numbers),
 member(Z, Numbers),
 2020 is X + Y + Z,
 Result is X * Y * Z.

:-
 phrase_from_file(lines(Numbers), 'input.txt'),
 partone(Numbers, ProductA),
 format("~d\n", ProductA),
 parttwo(Numbers, ProductB),
 format("~d\n", ProductB),
 halt.