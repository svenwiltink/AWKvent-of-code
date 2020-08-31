#! /usr/bin/env swipl

:- [intcode].

:-
    readIntcode(IntCode),
    Machine = machine{intCode: IntCode, pc: 0, inputs: [1], output: [], halted: false},
    runIntCode(Machine, MachineOut),
    format("Part 1: ~p\n", MachineOut.output).

:-
    readIntcode(IntCode),
    Machine = machine{intCode: IntCode, pc: 0, inputs: [5], output: [], halted: false},
    runIntCode(Machine, MachineOut),
    format("Part 2: ~p\n", MachineOut.output),
    halt.