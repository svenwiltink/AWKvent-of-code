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

runIntCode(MachineIn, MachineOut, 1):-

    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    IntCode = MachineIn.intCode,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    get_assoc(Pos1, IntCode, N1),
    get_assoc(Pos2, IntCode, N2),

    Number is N1 + N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, 2):-

    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    IntCode = MachineIn.intCode,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    get_assoc(Pos1, IntCode, N1),
    get_assoc(Pos2, IntCode, N2),

    Number is N1 * N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, 99):-
    get_assoc(0, MachineIn.intCode, Value),
    MachineOut = MachineIn.put([output=Value]).

runIntCode(MachineIn, MachineOut):-
    get_assoc(MachineIn.pc, MachineIn.intCode, OpCode),
    runIntCode(MachineIn, MachineOut, OpCode).
    
runIntCodeWithReplacement(NumberOne, NumberTwo, IntCode, Value):-
    put_assoc(1, IntCode, NumberOne, IntCode2),
    put_assoc(2, IntCode2, NumberTwo, IntCode3),

    Machine = machine{intCode: IntCode3, pc: 0, output: []},

    runIntCode(Machine, MachineOut),
    Value is MachineOut.output.

:-
    readIntcode(IntCode),
    runIntCodeWithReplacement(12, 1, IntCode, Output),
    format("Part 1: ~d\n", Output).

:-
    readIntcode(IntCode),
    between(0, 100, A),
    between(0, 100, B),
    runIntCodeWithReplacement(A, B, IntCode, 19690720),

    format("Part 2: ~d~d\n", [A, B]),
    halt.