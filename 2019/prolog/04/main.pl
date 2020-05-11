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

% bit of code duplication to brighten the day
p2passwordCount(C):-
    numlist(271973, 785961, Numbers),
    p2checkPasswords(Numbers, C).

p2checkPasswords([], 0).

p2checkPasswords([P], Valid):-
    p2checkPassword(P, Valid).

p2checkPasswords([P|T], Valid):-
    p2checkPassword(P, IsValid),
    p2checkPasswords(T, OtherValid),
    Valid is IsValid + OtherValid.

alwaysIncreasing([]).

alwaysIncreasing([_|[]]).

alwaysIncreasing([A,B|R]):-
    A =< B,
    alwaysIncreasing([B|R]).

hasDouble([A,B|_], A):-
    A = B.

hasDouble([_|R], D):-
    hasDouble(R, D).

hasTriple([A,B,C|_], A):-
    A = B,
    B = C.

hasTriple([_|R], T):-
    hasTriple(R, T).

p1checkPassword(N, IsValid):-
    number_codes(N, Chars),
    (   (
        alwaysIncreasing(Chars),
        hasDouble(Chars, _)
        )
    -> IsValid is 1
    ;  IsValid is 0
    ).

p2checkPassword(N, IsValid):-
    number_codes(N, Chars),
    (   (
        alwaysIncreasing(Chars),
        hasDouble(Chars, D),
        \+ hasTriple(Chars, D)
        )
    -> IsValid is 1
    ;  IsValid is 0
    ).

:-
    p1passwordCount(C),
    format("part 1: ~p\n", C),
    p2passwordCount(C2),
    format("part 2: ~p\n", C2),
    halt.