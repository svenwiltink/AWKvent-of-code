use_module(library(pio)).

layers(L, [layer(L, P)|Rs]) --> layer(L, P), layers(L, Rs).
layers(L, [layer(L, P)|[]]) --> layer(L, P).
layers(_, []) --> [].


layer(0, []) --> [].
layer(L, [P|Ps]) --> [P], {L1 is L - 1}, layer(L1, Ps).

get_number_count([], _, 0).
get_number_count([L|Lr], Number, Count):-
    get_number_count(Lr, Number, Remaining),
    (L = Number
    ->  Count is Remaining + 1
    ;   Count is Remaining
    ).

get_lowest_zero_layer(Ls, Best):-
    char_code("0", Number),
    member(layer(_, Best), Ls),
    get_number_count(Best, Number, BestCount),

    \+ (
        member(layer(_, Other), Ls),
        get_number_count(Other, Number, OtherCount),
        OtherCount < BestCount
    ).

:-
    char_code("1", Number1),
    char_code("2", Number2),

    phrase_from_file(layers(150, Ls), 'input.txt'),
    get_lowest_zero_layer(Ls, Layer),

    get_number_count(Layer, Number1, N1),
    get_number_count(Layer, Number2, N2),

    Answer is N1 * N2,

    format("part 1: ~p", Answer).