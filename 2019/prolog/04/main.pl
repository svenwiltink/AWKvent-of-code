#! /usr/bin/env swipl

p1passwordCount(C):-
    numlist(271973, 785961, Numbers),
    p1checkPasswords(Numbers, C).

p1checkPasswords([], 0).

p1checkPasswords([P], Valid):-
    p1checkPassword(P, Valid).

p1checkPasswords([P|T], Valid):-
    p1checkPassword(P, IsValid),
    p1checkPasswords(T, OtherValid),
    Valid is IsValid + OtherValid.

alwaysIncreasing([]).

alwaysIncreasing([_|[]]).

alwaysIncreasing([A,B|R]):-
    A =< B,
    alwaysIncreasing([B|R]).

hasDouble([A,B|_]):-
    A = B.

hasDouble([_|R]):-
    hasDouble(R).

p1checkPassword(N, IsValid):-
    number_codes(N, Chars),
    (   (
        alwaysIncreasing(Chars),
        hasDouble(Chars)
        )
    -> IsValid is 1
    ;  IsValid is 0
    ).

:-
    p1passwordCount(C),
    format("part 1: ~p", C),
    halt.