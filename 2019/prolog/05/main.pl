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
%    format("modes ~p parameter ~p\n", [Modes, Parameter]),
    Mode is mod(floor(Modes/10**Parameter), 10),
%    format("Mode ~p\n", Mode),
    getParameterMode(IntCode, Mode, Address, Value).

% positional mode
getParameterMode(IntCode, 0, Address, Value):-
%    format("using positional mode\n"),
    get_assoc(Address, IntCode, Value).

getParameterMode(_, 1, Address, Value):-
%    format("using immidiate mode\n"),
    Value is Address.

runIntCode(IntCode, PC, Mode, Inputs, Output, 1):-

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    Number is N1 + N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn, Inputs, Output).

runIntCode(IntCode, PC, Mode, Inputs, Output, 2):-

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    Number is N1 * N2,
    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn, Inputs, Output).

runIntCode(IntCode, PC, _, [I|Inputs], Output, 3):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),

    put_assoc(Pos1, IntCode, I, NewIntCode),

    PCn is PC + 2,

    runIntCode(NewIntCode, PCn, Inputs, Output).

runIntCode(IntCode, PC, Mode, Inputs, [N|Output], 4):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),
    getParameter(IntCode, Mode, 0, Pos1, N1),

    N is N1,
    format("Output: ~p\n", N),

    PCn is PC + 2,

    runIntCode(IntCode, PCn, Inputs, Output).

runIntCode(_, _, _, _, _, 99).

runIntCode(IntCode, PC, Inputs, Output):-
    get_assoc(PC, IntCode, OpCodeData),
    OpCode is mod(OpCodeData, 100),
    Mode is floor(OpCodeData / 100),
%    format("Original ~p Opcode ~p Mode ~p\n", [OpCodeData, OpCode, Mode]),
    runIntCode(IntCode, PC, Mode, Inputs, Output, OpCode).

runIntCodeWithReplacement(A, B, IntCode, Inputs, Value):-
    put_assoc(1, IntCode, A, IntCode2),
    put_assoc(2, IntCode2, B, IntCode3),
    runIntCode(IntCode3, 0, Inputs, Output),
    Value is Output.

:-
    readIntcode(IntCode),
    runIntCode(IntCode, 0, [1], _),
    halt