:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines(Locations, Y) --> line(L, Y), {Yn is Y + 1}, lines(L2, Yn), { append(L, L2, Locations) }.
lines(Locations, Y) --> line(Locations, Y).
lines([]) --> [].

line(L, Y) --> {X is 0}, locations(L, X, Y), "\n".

locations([location(X,Y,T)|Rs], X, Y) --> location(X, Y, T), {Xn is X + 1}, locations(Rs, Xn, Y).
locations([location(X,Y,T)|[]], X, Y) --> location(X, Y, T).
locations([], _, _) --> [].

location(_, _, tree) --> tree.
location(_, _, space) --> space.

space --> ".".
tree --> "#".


treesOnSlope(Y, DX, DY, C):-
    X is (floor(Y * DX / DY)) mod 31,
    location(X, Y, tree),
    Yn is Y + 1,
    treesOnSlope(Yn, DX, DY, Cn),
    ((B is Y mod DY, B == 0)
    -> C is Cn + 1
    ; C is Cn
    ).

treesOnSlope(Y, DX, DY, C):-
    X is (floor(Y * DX / DY)) mod 31,
    location(X, Y, space),
    Yn is Y + 1,
    treesOnSlope(Yn, DX, DY, C).

treesOnSlope(_, _, _, 0).

:-
    phrase_from_file(lines(Trees, 0), 'input.txt'),
    forall(member(Fact, Trees), assertz(Fact)),
    treesOnSlope(0, 1, 1, SL1),
    treesOnSlope(0, 3, 1, SL2),
    treesOnSlope(0, 5, 1, SL3),
    treesOnSlope(0, 7, 1, SL4),
    treesOnSlope(0, 1, 2, SL5),
    format("~d\n", SL2),
    P2 is SL1 * SL2 * SL3 * SL4 * SL5,
    format("~d\n", P2),
    halt.