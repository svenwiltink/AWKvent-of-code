#! /usr/bin/env swipl

:- [intcode].

runAmps([PhaseA, PhaseB, PhaseC, PhaseD, PhaseE], Output):-
    readIntcode(IntCode),

    AmpA = machine{intCode: IntCode, pc: 0, inputs: [PhaseA, 0], output: [], halted: false, pauseOnOutput: true},
    runIntCode(AmpA, AmpAOut),

    AmpB = machine{intCode: IntCode, pc: 0, inputs: [PhaseB|AmpAOut.output], output: [], halted: false, pauseOnOutput: true},
    runIntCode(AmpB, AmpBOut),

    AmpC = machine{intCode: IntCode, pc: 0, inputs: [PhaseC|AmpBOut.output], output: [], halted: false, pauseOnOutput: true},
    runIntCode(AmpC, AmpCOut),

    AmpD = machine{intCode: IntCode, pc: 0, inputs: [PhaseD|AmpCOut.output], output: [], halted: false, pauseOnOutput: true},
    runIntCode(AmpD, AmpDOut),

    AmpE = machine{intCode: IntCode, pc: 0, inputs: [PhaseE|AmpDOut.output], output: [], halted: false, pauseOnOutput: true},
    runIntCode(AmpE, AmpEOut),
    Output is AmpEOut.output.

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
        runFeedbackAmps([
            machine{name: a, intCode: IntCode, pc: 0, inputs: [PhaseA,0], output: [], halted: false, pauseOnOutput: true},
            machine{name: b, intCode: IntCode, pc: 0, inputs: [PhaseB], output: [], halted: false, pauseOnOutput: true},
            machine{name: c, intCode: IntCode, pc: 0, inputs: [PhaseC], output: [], halted: false, pauseOnOutput: true},
            machine{name: d, intCode: IntCode, pc: 0, inputs: [PhaseD], output: [], halted: false, pauseOnOutput: true},
            machine{name: e, intCode: IntCode, pc: 0, inputs: [PhaseE], output: [], halted: false, pauseOnOutput: true}],
            Output
        ).

findBestOutputPart2(BestOutput):-
    permutation([5,6,7,8,9], BestSetting),
    startFeedbackAmps(BestSetting, [BestOutput]),

    \+ (
        permutation([5,6,7,8,9], OtherSetting),
        startFeedbackAmps(OtherSetting, [OtherOutput]),
        OtherOutput > BestOutput
    ).

runFeedbackAmps([A1, A2, A3, A4, A5], Output):-
    runIntCode(A1, A1Out),
    (A1Out.halted
    ->
        Output = A5.output
    ;
        last(A1Out.output, Last),
        append(A2.inputs, [Last], A2Input),
        A2N = A2.put([inputs: A2Input]),
        runFeedbackAmps([A2N, A3, A4, A5, A1Out], Output)
    ).

:-
    findBestOutput(_, Output),
    format("Part 1 ~p\n", Output),
    findBestOutputPart2(Output2),
    format("Part 2 ~p\n", [Output2]),
    halt.