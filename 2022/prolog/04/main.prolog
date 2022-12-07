:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

pairs([pair(L, R)]) --> pair(L, R).
pairs([pair(L,R) |Rs]) --> pair(L,R), "\n", pairs(Rs).

pair(plot(P1L, P1R), plot(P2L, P2R)) --> plot(P1L, P1R), ",", plot(P2L, P2R).

plot(L, R) --> number(L),  "-", number(R).

pair_contains(pair(A, B)):- plot_contains(A, B).
pair_contains(pair(A, B)):- plot_contains(B, A).

plot_contains(plot(ALeft, ARight), plot(BLeft, BRight)) :-
    BLeft =< ALeft,
    BRight >= ARight.

pair_overlaps(pair(A, B)):- not(plot_outside(A, B)), not(plot_outside(B, A)).

plot_outside(plot(L1, R1), plot(L2, R2)):- 
    R2 < L1 ;
    L2 > R1.
    
:-
    phrase_from_file(pairs(A), 'input.txt'),
    include(pair_contains, A, P1Out),
    include(pair_overlaps, A, P2Out),
    length(P1Out, L1),
    length(P2Out, L2),
    format("~p\n", L1),
    format("~p\n", L2),
    halt.

