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

getParameter(Machine, Modes, Parameter, Address, Value):-
    Mode is mod(floor(Modes/10**Parameter), 10),
    getParameterMode(Machine, Mode, Address, Value).
    %format("Detected mode ~p for parameter ~p value ~p\n", [Mode, Parameter, Value]).


% positional mode
getParameterMode(Machine, 0, Address, Value):-
    getIntCodeValue(Address, Machine.intCode, Value).

% literal mode
getParameterMode(_, 1, Address, Value):-
    Value is Address.

% relative mode
getParameterMode(Machine, 2, Address, Value):-
    ShiftedAddress is Machine.rc + Address,
    %format("Using location ~p for relative mode\n", [ShiftedAddress]),
    getIntCodeValue(ShiftedAddress, Machine.intCode, Value).

getStorageLocation(Machine, Modes, Parameter, Address, Location):-
    Mode is mod(floor(Modes/10**Parameter), 10),
    getStorageMode(Machine, Mode, Address, Location).

getStorageMode(_, 0, Address, Location):-
    Location is Address.

getStorageMode(Machine, 2, Address, Location):-
    Location is Address + Machine.rc.

getIntCodeValue(Location, IntCode, Output):-
    (get_assoc(Location, IntCode, PossibleValue)
        ->  Output is PossibleValue
        ;   Output is 0
    ).
    %format("read value at location ~p value ~p\n", [Location, Output]).

runIntCode(MachineIn, MachineOut, Mode, 1):-
    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,
    IntCode = MachineIn.intCode,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),
    getIntCodeValue(L3, IntCode, Pos3),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),
    getStorageLocation(MachineIn, Mode, 2, Pos3, N3),

    Number is N1 + N2,
    PCn is PC + 4,

    %format("writing ~p to ~p", [Number, N3]),
    put_assoc(N3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 2):-
    PC is MachineIn.pc,
    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    IntCode = MachineIn.intCode,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),
    getIntCodeValue(L3, IntCode, Pos3),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),
    getStorageLocation(MachineIn, Mode, 2, Pos3, N3),

    Number is N1 * N2,
    PCn is PC + 4,

    %format("writing ~p to ~p", [Number, N3]),
    put_assoc(N3, IntCode, Number, NewIntCode),

    NewMachine = MachineIn.put([intCode=NewIntCode, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 3):-
    PC is MachineIn.pc,
    [I|Inputs] = MachineIn.inputs,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,

    getIntCodeValue(L1, IntCode, Pos1),
    %format("PC ~p Mode ~p RC ~p L1 ~p Pos1 ~p\n", [PC, Mode, MachineIn.rc, L1, Pos1]),

    getStorageLocation(MachineIn, Mode, 0, Pos1, N1),

    %format("writing ~p to location ~p\n", [I, N1]),
    put_assoc(N1, IntCode, I, NewIntCode),

    PCn is PC + 2,

    NewMachine = MachineIn.put([intCode=NewIntCode, inputs=Inputs, pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 4):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,

    getIntCodeValue(L1, IntCode, Pos1),
    getParameter(MachineIn, Mode, 0, Pos1, N1),

    N is N1,

    PCn is PC + 2,

    ( MachineIn.pauseOnOutput
    ->  (
            MachineOut = MachineIn.put([pc=PCn, output=[N]])
        )
    ;   (
            append(MachineIn.output, [N], NewOutput),
            NewMachine = MachineIn.put([pc=PCn, output=NewOutput]),
            runIntCode(NewMachine, MachineOut)
        )
    ).

runIntCode(MachineIn, MachineOut, Mode, 5):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,
    L2 is PC + 2,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),

    (N1 > 0
    -> PCn is N2
    ; PCn is PC + 3
    ),

    NewMachine = MachineIn.put([pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 6):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,
    L2 is PC + 2,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),

    (N1 = 0
    -> PCn is N2
    ; PCn is PC + 3
    ),

    NewMachine = MachineIn.put([pc=PCn]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 7):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),
    getIntCodeValue(L3, IntCode, Pos3),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),
    getStorageLocation(MachineIn, Mode, 2, Pos3, N3),

    (N1 < N2
    -> Number is 1
    ; Number is 0
    ),

    %format("writing ~p to location ~p\n", [Number, N3]),
    put_assoc(N3, IntCode, Number, NewIntCode),

    PCn is PC + 4,

    NewMachine = MachineIn.put([pc=PCn, intCode=NewIntCode]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 8):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    getIntCodeValue(L1, IntCode, Pos1),
    getIntCodeValue(L2, IntCode, Pos2),
    getIntCodeValue(L3, IntCode, Pos3),

    getParameter(MachineIn, Mode, 0, Pos1, N1),
    getParameter(MachineIn, Mode, 1, Pos2, N2),
    getStorageLocation(MachineIn, Mode, 2, Pos3, N3),

    (N1 = N2
    -> Number is 1
    ; Number is 0
    ),

    %format("writing ~p to location ~p\n", [Number, N3]),
    put_assoc(N3, IntCode, Number, NewIntCode),

    PCn is PC + 4,

    NewMachine = MachineIn.put([pc=PCn, intCode=NewIntCode]),

    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, Mode, 9):-
    PC is MachineIn.pc,
    IntCode = MachineIn.intCode,

    L1 is PC + 1,

    getIntCodeValue(L1, IntCode, Pos1),
    getParameter(MachineIn, Mode, 0, Pos1, N1),

    PCn is PC + 2,
    RCn is N1 + MachineIn.rc,

    NewMachine = MachineIn.put([pc=PCn, rc=RCn]),
    runIntCode(NewMachine, MachineOut).

runIntCode(MachineIn, MachineOut, _, 99):-
    MachineOut = MachineIn.put([halted=true]).

runIntCode(MachineIn, MachineOut):-
    get_assoc(MachineIn.pc, MachineIn.intCode, OpCodeData),
    OpCode is mod(OpCodeData, 100),
    Mode is floor(OpCodeData / 100),
    %format("\n\nRunning opcode ~p with mode ~p inputs ~p RC ~p PC ~p\n", [OpCode, Mode, MachineIn.inputs, MachineIn.rc, MachineIn.pc]),
    runIntCode(MachineIn, MachineOut, Mode, OpCode).