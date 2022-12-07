:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

pairs([pair(L, R)]) --> pair(L, R).
pairs([pair(L,R) |Rs]) --> pair(L,R), "\n", pairs(Rs).



:-
    phrase_from_file(pairs(A), 'input.txt'),
    include(pair_contains, A, P1Out),
    include(pair_overlaps, A, P2Out),
    format("~p\n", L1),
    format("~p\n", L2),
    halt.

