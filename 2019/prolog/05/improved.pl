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

getParameter(IntCode, Modes, Parameter, Address, Value):-
    Mode is mod(floor(Modes/10**Parameter), 10),
    getParameterMode(IntCode, Mode, Address, Value).

% positional mode
getParameterMode(IntCode, 0, Address, Value):-
    get_assoc(Address, IntCode, Value).

% literal mode
getParameterMode(_, 1, Address, Value):-
    Value is Address.

runIntCode(MachineIn, MachineOut, Mode, 1):-
    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,
    IntCode = MachineIn.intCode,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    Number is N1 + N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 2):-
    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    IntCode = MachineIn.intCode,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    Number is N1 * N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, _, 3):-
    PC is MachineIn.pc,
    [I|Inputs] = MachineIn.inputs,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),

    put_assoc(Pos1, IntCode, I, NewIntCode),

    PCn is PC + 2,

    NewMachine = MachineIn.put([intCode=NewIntCode, inputs=Inputs, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 4):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),
    getParameter(IntCode, Mode, 0, Pos1, N1),

    N is N1,
    format("Output: ~p\n", N),

    PCn is PC + 2,

    NewMachine = MachineIn.put([pc=PCn, output=N]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, _, 99):-
    MachineOut = MachineIn.put([halted=true]).

runIntCode(MachineIn, MachineOut):-
    get_assoc(MachineIn.pc, MachineIn.intCode, OpCodeData),
    OpCode is mod(OpCodeData, 100),
    Mode is floor(OpCodeData / 100),

    format("Mode ~p, OpCode ~p\n", [Mode, OpCode]),

    runIntCode(MachineIn, MachineOut, Mode, OpCode).

:-
    readIntcode(IntCode),
    Machine = machine{intCode: IntCode, pc: 0, inputs: [1], output: [], halted: false},
    runIntCode(Machine, MachineOut),
    format("Part 1: ~p\n", MachineOut.output),
    halt.