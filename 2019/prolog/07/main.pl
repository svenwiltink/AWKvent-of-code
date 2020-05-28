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

getParameterMode(_, 1, Address, Value):-
    Value is Address.


returnOnPrint.

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 1):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 2):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, _, [I|Inputs], Output, NPC, NIntCode, 3):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),

    put_assoc(Pos1, IntCode, I, NewIntCode),

    PCn is PC + 2,

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, Mode, Inputs, [N|Output], NPC, NIntCode, 4):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),
    getParameter(IntCode, Mode, 0, Pos1, N1),

    N is N1,
    format("Output: ~p\n", N),

    PCn is PC + 2,

    ( returnOnPrint
        -> (NPC is PCn, copy_term(IntCode, NIntCode))
        ;  runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode)
    ).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 5):-
    L1 is PC + 1,
    L2 is PC + 2,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    (N1 > 0
    -> PCn is N2
    ; PCn is PC + 3
    ),

    runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 6):-

    L1 is PC + 1,
    L2 is PC + 2,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    (N1 = 0
    -> PCn is N2
    ; PCn is PC + 3
    ),

    runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 7):-

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    (N1 < N2
    -> Number is 1
    ; Number is 0
    ),

    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, 8):-

    L1 is PC + 1,
    L2 is PC + 2,
    L3 is PC + 3,

    get_assoc(L1, IntCode, Pos1),
    get_assoc(L2, IntCode, Pos2),
    get_assoc(L3, IntCode, Pos3),

    getParameter(IntCode, Mode, 0, Pos1, N1),
    getParameter(IntCode, Mode, 1, Pos2, N2),

    (N1 = N2
    -> Number is 1
    ; Number is 0
    ),

    PCn is PC + 4,

    put_assoc(Pos3, IntCode, Number, NewIntCode),

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode).

runIntCode(IntCode, PC, _, _, _, NPC, NIntCode, 99):-
    NPC is PC,
    copy_term(IntCode, NIntCode).

runIntCode(IntCode, PC, Inputs, Output, NPC, NIntCode):-
    get_assoc(PC, IntCode, OpCodeData),
    OpCode is mod(OpCodeData, 100),
    Mode is floor(OpCodeData / 100),
    runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, OpCode).

runIntCodeWithReplacement(A, B, IntCode, Inputs, Value):-
    put_assoc(1, IntCode, A, IntCode2),
    put_assoc(2, IntCode2, B, IntCode3),
    runIntCode(IntCode3, 0, Inputs, Output),
    Value is Output.

:-
    format("Part 1\n"),
    readIntcode(IntCode),
    runIntCode(IntCode, 0, [3,0], _, NPC, NIntCode),
    format("End PC ~p\nStart Intcode\n~p End intcode\n~p", [NPC, IntCode, NIntCode]).