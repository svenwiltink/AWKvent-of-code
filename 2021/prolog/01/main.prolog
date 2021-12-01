:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> integer(L), "\n", lines(Ls).
lines([L]) --> integer(L).
lines([]) --> [].

partone([X, Y], Answer):-
    Y > X,
    Answer is 1.

partone([_, _], 0).

partone([X, Y| T], Answer):-
    Y > X,
    partone([Y|T], Rest),
    Answer is Rest + 1.

partone([_, Y| T], Answer):-
    partone([Y|T], Answer).

parttwo([_, _, _], _, 0).
parttwo([A,B,C|T], Prev, Answer):-
    Sum = A + B + C,
    Sum > Prev,
    parttwo([B,C|T], Sum, Rest),
    Answer is Rest + 1.

parttwo([A,B,C|T], _, Rest):-
    Sum = A + B + C,
    parttwo([B,C|T], Sum, Rest).

:-
 phrase_from_file(lines(Numbers), 'input.txt'),
 partone(Numbers, ProductA),
 format("~d\n", ProductA),
 parttwo(Numbers, 0, ProductB),
 format("~d\n", ProductB),
 halt.