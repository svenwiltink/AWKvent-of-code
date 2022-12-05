:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

pairs([pair(L)]) --> pair(L).
pairs([pair(L) |Rs]) --> pair(L), "\n", pairs(Rs).

pair((plot(P1L, P1R), plot(P2L, P2R))) --> plot(P1L, P1R), ",", plot(P2L, P2R).

plot(L, R) --> number(L),  "-", number(R).


pair_contains(pair((A, B))):- plot_contains(A, B).
pair_contains(pair((A, B))):- plot_contains(B, A).

plot_contains(plot(ALeft, ARight), plot(BLeft, BRight)) :-
    BLeft =< ALeft,
    BRight >= ARight.

:-
    phrase_from_file(pairs(A), 'input.txt'),
    include(pair_contains, A, Out),
    length(Out, L),
    format("~p\n", L).

