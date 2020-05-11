#! /usr/bin/env swipl
readSeries():-
    assert(coord(0, 0, 0)),
    setup_call_cleanup(
        open("input.txt", read, Stream),
        readData(Stream, 0, 0, 0),
        close(Stream)
    ).

readData(Stream, SeriesName, X, Y):-
    read_string(Stream, ",\n", "\n", Sep, String),
    readLine(Stream, SeriesName, String, Sep, X, Y).

% empty line
readLine(_, _, "", -1, _, _).

% Last value in file
readLine(_, SeriesName, String, -1, X, Y):-
        parseUnit(String, Direction, Amount),
        transformCoords(Direction, Amount, X, Y, NewX, NewY),
        createEdge(SeriesName, X, Y, NewX, NewY),
        insertCoords(SeriesName, X, Y, NewX, NewY).

% We've hit a newline. This value still belongs to the old series
readLine(Stream, SeriesName, String, 10, X, Y):-
        parseUnit(String, Direction, Amount),
        transformCoords(Direction, Amount, X, Y, NewX, NewY),
        createEdge(SeriesName, X, Y, NewX, NewY),
        insertCoords(SeriesName, X, Y, NewX, NewY),
        NSeriesName is SeriesName + 1,
        assert(coord(NSeriesName, 0, 0)),
        readData(Stream, NSeriesName, 0, 0).

% just an ordinary value
readLine(Stream, SeriesName, String, _, X, Y):-
        parseUnit(String, Direction, Amount),
        transformCoords(Direction, Amount, X, Y, NewX, NewY),
        createEdge(SeriesName, X, Y, NewX, NewY),
        insertCoords(SeriesName, X, Y, NewX, NewY),
        readData(Stream, SeriesName, NewX, NewY).

% Transform String to Direction and Amount
parseUnit(Input, Direction, Amount):-
    sub_string(Input, 0, 1, After, Direction),
    sub_string(Input, 1, After, _, AmountString),
    number_string(Amount, AmountString).


createEdge(SeriesName, X, Y, NewX, NewY):-
    D is abs(NewX - X) + abs(NewY - Y),
    assert(edge(SeriesName, X, Y, NewX, NewY, D)).

% The coords are the same. We can stop stepping now
insertCoords(Series, X, Y, X, Y):-
    assert(coord(Series, X, Y)).

% The X coordinates are the same. We only need to step Y now.
insertCoords(Series, X, Y1, X, Y2):-
    assert(coord(Series, X, Y1)),
    ( (Y1 =< Y2)
    -> NewY is Y1 + 1
    ;  NewY is Y1 - 1
    ),
    insertCoords(Series, X, NewY, X, Y2).

% The Y coordinates are the same. We only need to step X now.
insertCoords(Series, X1, Y, X2, Y):-
    assert(coord(Series, X1, Y)),
    ( (X1 =< X2)
    -> NewX is X1 + 1
    ;  NewX is X1 - 1
    ),
    insertCoords(Series, NewX, Y, X2, Y).

% coord transform functions
transformCoords("U", Amount, X, Y, X, NewY):-
    NewY is Y + Amount.

transformCoords("D", Amount, X, Y, X, NewY):-
    NewY is Y - Amount.

transformCoords("L", Amount, X, Y, NewX, Y):-
    NewX is X - Amount.

transformCoords("R", Amount, X, Y, NewX, Y):-
    NewX is X + Amount.

mDistance(X, Y, D):-
    D is abs(X) + abs(Y).

crossing(X, Y, D):-
    coord(0, X, Y), coord(1, X, Y),
    X \= 0, Y \= 0,
    mDistance(X, Y, D).

shortest(crossing(X, Y, D)):-
 crossing(X, Y, D),
 \+ (crossing(X2, Y2, D2), X \= X2, Y \= Y2, D2 < D).


crossingBounds(MinX, MaxX, MinY, MaxY, X, Y):-
    crossing(X, Y, _),
    X >= MinX,
    X =< MaxX,
    Y >= MinY,
    Y =< MaxY.

% we found an edge that the crossing is on.
distanceToCrossing(Series, CurrentX, CurrentY, CrossingX, CrossingY, Distance):-
    edge(Series, CurrentX, CurrentY, NewX, NewY, _),
    crossingBounds(
        min(CurrentX, NewX),
        max(CurrentX, NewX),
        min(CurrentY, NewY),
        max(CurrentY, NewY),
        CrossingX, CrossingY),
    Distance is abs(CrossingX - CurrentX) + abs(CrossingY - CurrentY).

% Keep walking the path until we found an edge that the crossing is on
distanceToCrossing(Series, CurrentX, CurrentY, CrossingX, CrossingY, Distance):-
    edge(Series, CurrentX, CurrentY, NewX, NewY, D),
    distanceToCrossing(Series, NewX, NewY, CrossingX, CrossingY, OtherDistance),
    Distance is D + OtherDistance.


combinedDistance(CrossingX, CrossingY, D):-
    distanceToCrossing(0, 0, 0, CrossingX, CrossingY, D1),
    distanceToCrossing(1, 0, 0, CrossingX, CrossingY, D2),
    D is D1 + D2.

closestCrossing(X, Y, D):-
    crossing(X, Y, _),
    combinedDistance(X, Y, D),
    \+ (
        crossing(X2, Y2, _),
        combinedDistance(X2, Y2, D2),
        D2 < D
    ).

:-
    readSeries,
    shortest(crossing(X, Y, D)),
    format("Part 1: X ~p Y ~p Distance: ~p\n", [X, Y, D]),
    closestCrossing(X2, Y2, D2),
    format("Part 2: X ~p Y ~p Distance: ~p\n", [X2, Y2, D2]),
    halt.