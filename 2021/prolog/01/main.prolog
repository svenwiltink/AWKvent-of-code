:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> integer(L), "\n", lines(Ls).
lines([L]) --> integer(L).
lines([]) --> [].

partone([X, Y], Answer):-
    Y > X,
    Answer is 1.

partone([_, _], Answer):-
    Answer is 0.

partone([X, Y| T], Answer):-
    Y > X,
    partone([Y|T], Rest),
    Answer is Rest + 1.

partone([_, Y| T], Answer):-
    partone([Y|T], Answer).

:-
 phrase_from_file(lines(Numbers), 'input.txt'),
 partone(Numbers, ProductA),
 format("~d\n", ProductA),
 halt.