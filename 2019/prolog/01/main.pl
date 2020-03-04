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

calculateMass([], 0).

calculateMass([H|T], Mass):-
    number_string(Hn, H),
    Hm is floor(Hn/3) - 2,
    calculateMass(T, N),
    Mass is Hm + N.

:-
    readInput(Input),
    calculateMass(Input, Mass),
    format("Total mass: ~d\n", Mass),
    halt.