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

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 1):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 2):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, _, [I|Inputs], Output, NPC, NIntCode, NInputs, Halted, 3):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),

    put_assoc(Pos1, IntCode, I, NewIntCode),

    PCn is PC + 2,

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, Mode, Inputs, [N|Output], NPC, NIntCode, NInputs, Halted, 4):-
    L1 is PC + 1,

    get_assoc(L1, IntCode, Pos1),
    getParameter(IntCode, Mode, 0, Pos1, N1),

    N is N1,

%    format("Output ~p\n", N),

    PCn is PC + 2,

    ( returnOnPrint
        -> (
            NPC is PCn,
            copy_term(IntCode, NIntCode),
            copy_term(NInputs, Inputs),
            Halted is 0,
            Output = []
            )
        ;  runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted)
    ).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 5):-
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

    runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 6):-

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

    runIntCode(IntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 7):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, 8):-

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

    runIntCode(NewIntCode, PCn, Inputs, Output, NPC, NIntCode, NInputs, Halted).

runIntCode(IntCode, PC, _, Inputs, _, NPC, NIntCode, NInputs, Halted, 99):-
    NPC is PC,
    copy_term(Inputs, NInputs),
    Halted is 1,
    copy_term(IntCode, NIntCode).

runIntCode(IntCode, PC, Inputs, Output, NPC, NIntCode, NInputs, Halted):-
    get_assoc(PC, IntCode, OpCodeData),
    OpCode is mod(OpCodeData, 100),
    Mode is floor(OpCodeData / 100),
    runIntCode(IntCode, PC, Mode, Inputs, Output, NPC, NIntCode, NInputs, Halted, OpCode).

runIntCodeWithReplacement(A, B, IntCode, Inputs, Value):-
    put_assoc(1, IntCode, A, IntCode2),
    put_assoc(2, IntCode2, B, IntCode3),
    runIntCode(IntCode3, 0, Inputs, Output),
    Value is Output.


runAmps([PhaseA, PhaseB, PhaseC, PhaseD, PhaseE], OutputE):-
    readIntcode(IntCode),
    copy_term(IntCode, AmpA),
    copy_term(IntCode, AmpB),
    copy_term(IntCode, AmpC),
    copy_term(IntCode, AmpD),
    copy_term(IntCode, AmpE),

    runIntCode(AmpA, 0, [PhaseA,0], [OutputA], _, _, _, _),
    runIntCode(AmpB, 0, [PhaseB,OutputA], [OutputB], _, _, _, _),
    runIntCode(AmpC, 0, [PhaseC,OutputB], [OutputC], _, _, _, _),
    runIntCode(AmpD, 0, [PhaseD,OutputC], [OutputD], _, _, _, _),
    runIntCode(AmpE, 0, [PhaseE,OutputD], [OutputE], _, _, _, _).

findBestOutput(BestSetting, BestOutput):-
    permutation([0,1,2,3,4], BestSetting),
    runAmps(BestSetting, BestOutput),

    \+ (
        permutation([0,1,2,3,4], OtherSetting),
        runAmps(OtherSetting, OtherOutput),
        OtherOutput > BestOutput
    ).

startFeedbackAmps([PhaseA, PhaseB, PhaseC, PhaseD, PhaseE], Output):-
        readIntcode(IntCode),
        copy_term(IntCode, AmpA),
        copy_term(IntCode, AmpB),
        copy_term(IntCode, AmpC),
        copy_term(IntCode, AmpD),
        copy_term(IntCode, AmpE),

        runFeedbackAmps([
            amp("A", AmpA, 0, [PhaseA,0]),
            amp("B", AmpB, 0, [PhaseB]),
            amp("C", AmpC, 0, [PhaseC]),
            amp("D", AmpD, 0, [PhaseD]),
            amp("E", AmpE, 0,  [PhaseE])],
            Output
        ).

findBestOutputPart2(BestOutput):-
    permutation([5,6,7,8,9], BestSetting),
    format("trying permutation ~p ~p ~p ~p ~p\n", BestSetting),
    startFeedbackAmps(BestSetting, [BestOutput]),
    format("Output ~p\n", [BestOutput]),

    \+ (
        permutation([5,6,7,8,9], OtherSetting),
        format("trying other permutation ~p ~p ~p ~p ~p\n", OtherSetting),
        startFeedbackAmps(OtherSetting, [OtherOutput]),
        format("Otheroutput ~p\n", [OtherOutput]),

        OtherOutput > BestOutput
    ).

amp(_, _, _, _).

runFeedbackAmps(Amps, LastOutput):-
    runFeedbackAmps(Amps, [], LastOutput).

runFeedbackAmps([amp(N1, C1, P1, I1), amp(N2, C2, P2, I2), A3, A4, A5], Previous, LastOutput):-
    runIntCode(C1, P1, I1, Output, NPC, NIntCode, NInputs, Halted),

    (Halted = 1
    ->  Output = [],
        copy_term(Previous, LastOutput)
    ;   append(I2, Output, NI2),
        runFeedbackAmps([amp(N2, C2, P2, NI2), A3, A4, A5, amp(N1, NIntCode, NPC, NInputs)], Output, LastOutput)
    ).

%:-
%    findBestOutput(_, Output),
%    format("Part 1 ~p", Output),
%    halt.

:-
    debug,
    findBestOutputPart2(Output),
    format("Part 2 ~p", Output).