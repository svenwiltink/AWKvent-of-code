:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> string(L), "\n\n", lines(Ls).
lines([L]) --> string(L).
lines([]) --> [].

sections([S|Sr]) --> block(S), blank, sections(Sr).
sections(S) --> block(S).
sections([]) --> [].

block((Key, Value)) --> string(K), ":", string(V), {atom_codes(Key, K), atom_codes(Value, V)}.

validPassports([P|Ps], C):-
    validPassport(P, V),
    validPassports(Ps, Cn),
    C is Cn + V.

validPassports([P], C):-
    validPassport(P, C).

validPassports([_], 0).
validPassports([], 0).

validPassport(P, 1):-
    phrase(sections(S), P, []),
    countSections(S, N),
    (
        N == 8;
        N == 7, \+ countryPresent(S)
    ).

validPassport(_, 0).

countryPresent([(Key, _)|T]):-
    Key == cid ;
    countryPresent(T).

countryPresent((Key, _)):-
    Key == cid.

countSections([_|T], C):-
    countSections(T, Co),
    C is Co + 1.

countSections((_,_), 1).
countSections([], 0).

:-
    phrase_from_file(lines(L), "input.txt"),
    validPassports(L, C),
    format("~d", C).