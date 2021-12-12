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

find_route(Source, Destination, _, Route, _, [Destination|Route]):-
    connected(Source, Destination).

find_route(Source, Destination, Walked, Route, DoubleCaveAllowed, NewRoute):-
    connected(Source, H),
    \+ member(H, Walked),
    
    string_chars(H, [FirstChar|_]),
    (char_type(FirstChar, lower) ->
        (DoubleCaveAllowed ->
            try_revisit_small_cave(Try),
            (Try ->
                % double cave
                NewDoubleCaveAllowed = false,
                NewWalked = Walked
            ;   % skip double caving
                NewWalked = [H|Walked],
                NewDoubleCaveAllowed = DoubleCaveAllowed
            )
        ;   % Double caving not allowed
            NewWalked = [H|Walked],
            NewDoubleCaveAllowed = false
        )
    ;   NewWalked = Walked,
        NewDoubleCaveAllowed = DoubleCaveAllowed
    ),

    find_route(H, Destination, NewWalked, [H|Route], NewDoubleCaveAllowed, NewRoute).

find_route(Start, End, AllowDoubleCaving, Route):-
    find_route(Start, End, [Start,End], [Start], AllowDoubleCaving, R),
    reverse(Route, R).

try_revisit_small_cave(true).
try_revisit_small_cave(false).

:-
    bagof(R, find_route(start,end, false, R), Routes),
    length(Routes, L),
    format("~p\n", L),
    bagof(DC, distinct(find_route(start,end, true, DC)), DoubleCaveRoutes),
    length(DoubleCaveRoutes, LP2),
    format("~p\n", LP2),
    halt.
