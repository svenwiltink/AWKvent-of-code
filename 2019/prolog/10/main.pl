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