#! /usr/bin/env swipl

:- use_module(library(assoc)).

readIntcode(Input):-
    setup_call_cleanup(
        open("input.txt", read, Stream),
        readData(Stream, Input, 0),
        close(Stream)
    ).

readData(Stream, Input, Index):-
    read_string(Stream, ",", "\n", End, String),
    (
    (End == -1, String == "")
    ->  empty_assoc(Input)
    ;   
        number_string(N, String),
        NIndex is Index + 1,
        readData(Stream, Tail, NIndex),
        put_assoc(Index, Tail, N, Input)
    ).

runIntCode(IntCode, PC, 1):-
    
    format("running opcode ~d\n", 1),

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    get_assoc(Pos1, IntCode, N1),
    get_assoc(Pos2, IntCode, N2),

    Number is N1 + N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn).

runIntCode(IntCode, PC, 2):-
    
    format("running opcode ~d\n", 2),
    
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    get_assoc(Pos1, IntCode, N1),
    get_assoc(Pos2, IntCode, N2),

    Number is N1 * N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn).

runIntCode(IntCode, _, 99):-
    get_assoc(0, IntCode, Value),
    format("Found value ~d", Value).

runIntCode(IntCode, PC):-
    get_assoc(PC, IntCode, OpCode),
    format("Found opcode ~d\n", OpCode),
    runIntCode(IntCode, PC, OpCode).
    
:-
    readIntcode(IntCode),
    put_assoc(1, IntCode, 12, IntCode2),
    put_assoc(2, IntCode2, 1, IntCode3),
    runIntCode(IntCode3, 0).