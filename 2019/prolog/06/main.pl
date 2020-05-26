use_module(library(pio)).

edges([edge(S, D)|Rs]) --> edge(S, D), "\n", edges(Rs).
edges([edge(S, D)|[]]) --> edge(S, D).
edges([]) --> [].

edge(Source, Destination) --> location(Source), ")", location(Destination).

location([]) --> [].
location([L|Ls]) --> [L], location(Ls).

:-
    phrase_from_file(edges(A), 'input.txt'),
    forall(member(Fact, A), assertz(Fact)).


% from Source try to find an edge. If there is one, try to walk it. Once we have
% Done that add it to the Walked list. Repeat until everything has been traversed
find_orbits(Source, Walked, Depth, Orbits):-
    edge(Source, Destination),
    \+ member(Destination, Walked),

    % go deeper
    NDepth is Depth + 1,
    find_orbits(Destination, [Destination|Walked], NDepth, NOrbits),

    % check if there are more edges from this source node
    find_orbits(Source, [Destination|Walked], Depth, BOrbits),
    Orbits is NOrbits + BOrbits.

% If there is no edge we are done walking this path
find_orbits(_, _, Depth, Orbits):-
    Orbits is Depth.

:-
    string_codes("COM", S),
    find_orbits(S, [], 0, Orbits),
    format("Part 1: ~p\n", Orbits),
    halt.