#! /usr/bin/env swipl
readInput(Input):-
    setup_call_cleanup(
        open("input.txt", read, Stream),
        readData(Stream, Input),
        close(Stream)
    ).

readData(Stream, Input):-
    read_string(Stream, "\n", "\r", End, String),
    (
    (End == -1, String == "")
    ->  Input = []
    ;   Input = [String|Tail],
        readData(Stream, Tail)
    ).

p1CalculateMass([], 0).

p1CalculateMass([H|T], Mass):-
    number_string(Hn, H),
    Hm is floor(Hn/3) - 2,
    p1CalculateMass(T, N),
    Mass is Hm + N.

% if the Mass is <= 0 we don't need extra fuel
calculateExtraMass(M, E):-
    M =< 8,
    E is 0.

calculateExtraMass(M, E):-
    ExtraFuel is floor(M/3) - 2,
    calculateExtraMass(ExtraFuel, ExtraFuelFuel),
    E is ExtraFuel + ExtraFuelFuel.


p2CalculateMass([], 0).

p2CalculateMass([H|T], Mass):-
    number_string(Hn, H),
    Hm is floor(Hn/3) - 2,
    calculateExtraMass(Hm, Hmextra),
    p2CalculateMass(T, N),
    Mass is Hm + Hmextra + N.

:-
    readInput(Input),
    p1CalculateMass(Input, Mass),
    format("P1 Total mass: ~d\n", Mass),
    p2CalculateMass(Input, P2Mass),
    format("P2 Total mass: ~d\n", P2Mass),
    halt.