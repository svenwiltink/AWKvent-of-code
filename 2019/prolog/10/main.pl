use_module(library(pio)).

lines(Locations, Y) --> line(L, Y), {Yn is Y + 1}, lines(L2, Yn), { append(L, L2, Locations) }.
lines(Locations, Y) --> line(Locations, Y).
lines([]) --> [].

line(L, Y) --> {X is 0}, locations(L, X, Y), "\n".

locations([location(X,Y,T)|Rs], X, Y) --> location(X, Y, T), {Xn is X + 1}, locations(Rs, Xn, Y).
locations([location(X,Y,T)|[]], X, Y) --> location(X, Y, T).
locations([], _, _) --> [].

location(_, _, asteroid) --> asteroid.
location(_, _, space) --> space.

space --> ".".
asteroid --> "#".

:-
    phrase_from_file(lines(Asteroids, 0), 'input.txt'),
    forall(member(Fact, Asteroids), assertz(Fact)).

polarCoord(X1, Y1, X2, Y2, Angle, Distance):-
    Dx is X2 - X1,
    Dy is Y2 - Y1,
    Angle is atan2(Dy, Dx),
    Distance is Dx**2 + Dy**2.

los(X1, Y1, X2, Y2):-
    location(X2, Y2, asteroid),
    polarCoord(X1, Y1, X2, Y2, Angle, Distance),

    \+ (
        location(X3, Y3, asteroid),
        polarCoord(X1, Y1, X3, Y3, Angle2, Distance2),
        Angle = Angle2,
        Distance2 < Distance
    ).

losCount(X, Y, C):-
    location(X, Y, asteroid),
    aggregate_all(count, los(X, Y, _, _), C).

findBest([l(X, Y, C)|Rs], l(Cbx, Cby, Cbc), Best):-
    (C > Cbc
        ->  Nx is X,   Ny is Y,   Nc is C
        ;   Nx is Cbx, Ny is Cby, Nc is Cbc
    ),
    findBest(Rs, l(Nx, Ny, Nc), Best).

findBest([l(X, Y, C)|[]], l(Cbx, Cby, Cbc), Best):-
        (C > Cbc
            ->  Nx is X,   Ny is Y,   Nc is C
            ;   Nx is Cbx, Ny is Cby, Nc is Cbc
        ),
        Best = l(Nx, Ny, Nc).

:-
    findall(l(X, Y, C), losCount(X, Y, C), B),
    findBest(B, l(0,0,0), l(Xb, Yb, Cb)),
    format("Part 1: X ~p Y ~p C ~p", [Xb, Yb, Cb]),
    halt.