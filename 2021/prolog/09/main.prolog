:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines(Locations, Y) --> line(L, Y), {Yn is Y + 1}, lines(L2, Yn), { append(L, L2, Locations) }.
lines(Locations, Y) --> line(Locations, Y).
lines([]) --> [].

line(L, Y) --> {X is 0}, locations(L, X, Y), "\n".

locations([location(X,Y,T)|Rs], X, Y) --> location(X, Y, T), {Xn is X + 1}, locations(Rs, Xn, Y).
locations([location(X,Y,T)|[]], X, Y) --> location(X, Y, T).
locations([], _, _) --> [].

location(_, _, D) --> digit(A), {number_string(D, [A])}.

neighbour(X,Y,location(XN, Y, Z)):- XN is X + 1, location(XN, Y, Z).
neighbour(X,Y,location(XN, Y, Z)):- XN is X - 1, location(XN, Y, Z).
neighbour(X,Y,location(X, YN, Z)):- YN is Y + 1, location(X, YN, Z).
neighbour(X,Y,location(X, YN, Z)):- YN is Y - 1, location(X, YN, Z).

lowneighbour(location(X, Y, Z)):-
    neighbour(X,Y,location(_, _,NZ)),
    NZ =< Z,!.

islowpoint(X):- not(lowneighbour(X)).

highneighbour(Walked, location(X,Y,Z), location(XN, YN, ZN)):-
    neighbour(X, Y, location(XN, YN, ZN)),
    \+ member(location(XN, YN, ZN), Walked),
    ZN > Z,
    ZN < 9.

basin(CurPoint, (Walked,CurSize), ([CurPoint|CompleteWalked], NewSize)):- 
    ( (bagof(N, highneighbour([CurPoint|Walked], CurPoint, N), Neighbours)) ->

        length(Neighbours, CurrentNeighbourSize),
        append(Walked,Neighbours,NeighbourWalked),
        foldl(basin, Neighbours, ([CurPoint|NeighbourWalked],CurrentNeighbourSize), (OtherWalked,NeighbourSize)),
        CompleteWalked = OtherWalked

    ;   NeighbourSize is 0,
        CompleteWalked = Walked
    ),
    NewSize is CurSize + NeighbourSize.

basin(Point, Size):-
    basin(Point, ([],0), (_, S)), Size is S + 1.

score(location(_,_,Z), Cur, Result):- Result is Cur + Z + 1.

:-
    phrase_from_file(lines(Lavatubes, 0), 'input.txt'),
    forall(member(Fact, Lavatubes), assertz(Fact)),

    include(islowpoint, Lavatubes, LowPoints),
    foldl(score, LowPoints, 0, Sum),
    format("~p\n", Sum),

    maplist(basin, LowPoints, BasinSizes),
    sort(0, @>=, BasinSizes, [A,B,C|_]),
    Answer is A * B * C,
    format("~p\n", [Answer]),
    halt.