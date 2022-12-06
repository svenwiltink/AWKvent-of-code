:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

numbers([R]) --> nonblank(R).
numbers([R|Rs]) --> nonblank(R), numbers(Rs).

marker(Index, Length, [H|Tail], Result):-
    length(Sub, Length),
    append(Sub, _, [H|Tail]),
    all_distinct(Sub),
    Result is Index + Length.

marker(Index, Length, [_|Tail], Result):-
    NIndex is Index + 1,
    marker(NIndex, Length, Tail, Result).

:-
    phrase_from_file(numbers(A), 'input.txt'),
    marker(0, 4, A, P1),
    marker(0, 14, A, P2),
    format("~p\n", P1),
    format("~p\n", P2),
    halt.

