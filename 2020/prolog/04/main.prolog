:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> string(L), "\n\n", lines(Ls).
lines([L]) --> string(L).
lines([]) --> [].

sections([S|Sr]) --> block(S), blank, sections(Sr).
sections(S) --> block(S).
sections([]) --> [].

block((Key, Value)) --> string(K), ":", string(Value), {atom_codes(Key, K)}.

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

realValidPassports(P, C):-
    include(realValidPassport, P, V),
    length(V, C).

realValidPassport(P):-
    validPassport(P, 1),
    phrase(sections(S), P, []), !,
    validSections(S).

validSections([(Key,Value)|T]):- validSection(Key, Value), validSections(T).
validSections((Key, Value)):- validSection(Key, Value).
validSections([]).

validSection(byr, Value):- atom_codes(Num, Value), atom_number(Num, N), N >= 1920, N =< 2002.
validSection(iyr, Value):- atom_codes(Num, Value), atom_number(Num, N), N >= 2010, N =< 2020.
validSection(eyr, Value):- atom_codes(Num, Value), atom_number(Num, N), N >= 2020, N =< 2030.
validSection(hgt, Value):- validHeight(Value).
validSection(hcl, Value):- validHairColour(Value).
validSection(ecl, Value):- atom_codes(Colour, Value), validEyeColour(Colour).
validSection(pid, Value):- validPassportId(Value).
validSection(cid, _).

passportId(Pid) --> {length(Pid, 9)}, digits(Pid).
validPassportId(P):- phrase(passportId(_), P, []).

validEyeColour(amb).
validEyeColour(blu).
validEyeColour(brn).
validEyeColour(gry).
validEyeColour(grn).
validEyeColour(hzl).
validEyeColour(oth).

hairColour(Hex) --> "#", xdigits(Hex), {length(Hex, 6)}.

validHairColour(Colour):- 
    phrase(hairColour(_), Colour, []).

height((Amount, Unit)) --> integer(Amount), string(U), {atom_codes(Unit, U)}.

validHeight(H):- 
    phrase(height((Amount, Unit)), H, []),
    validHeight(Unit, Amount).

validHeight(cm, A):- A >= 150, A =< 193.
validHeight(in, A):- A >= 59, A =< 76.

:-
    phrase_from_file(lines(L), "input.txt"),
    validPassports(L, C),
    format("~d\n", C),
    realValidPassports(L, C2),
    format("~d\n", C2),
    halt.