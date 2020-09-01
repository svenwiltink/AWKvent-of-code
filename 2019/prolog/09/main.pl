#! /usr/bin/env swipl

:- [intcode].

:-
    readIntcode(IntCode),
    Machine = machine{intCode: IntCode, pc: 0, rc: 0, inputs: [1], output: [], halted: false, pauseOnOutput: false},
    runIntCode(Machine, MachineOut),
    format("Part one ~p\n", MachineOut.output),
    Machine2 = machine{intCode: IntCode, pc: 0, rc: 0, inputs: [2], output: [], halted: false, pauseOnOutput: false},
    runIntCode(Machine2, MachineOut2),
    format("Part two ~p", MachineOut2.output),
    halt.