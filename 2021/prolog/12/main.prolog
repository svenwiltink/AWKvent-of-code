:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

edges([edge(S, D)|Rs]) --> edge(S, D), "\n", edges(Rs).
edges([edge(S, D)]) --> edge(S, D).

edge(Source, Destination) --> location(Source), "-", location(Destination).

location(L) --> string(Codes), {atom_codes(L, Codes)}.

:-
    phrase_from_file(edges(A), 'input.txt'),
    forall(member(Fact, A), assertz(Fact)).

connected(A, B):-
    edge(A, B) ; edge(B, A).

find_route(Source, Destination, _, Route, [Destination|Route]):-
    connected(Source, Destination).

find_route(Source, Destination, Walked, Route, NewRoute):-
    connected(Source, H),
    \+ member(H, Walked),
    
    string_chars(H, [FirstChar|_]),
    (char_type(FirstChar, lower) ->
        NewWalked = [H|Walked]
    ;   NewWalked = Walked
    ),

    find_route(H, Destination, NewWalked, [H|Route], NewRoute).

:-
    bagof(R, find_route(start,end, [start,end], [start], R), Routes),
    length(Routes, L),
    format("~p\n", L),
    halt.