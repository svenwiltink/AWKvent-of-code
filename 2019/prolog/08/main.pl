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

% when there are no more layers to apply the last output is the
% final image
apply_layers([], Output, Output).

apply_layers([layer(_, CurrentLayer)| Rl], LastFrame, Output):-
    apply_layer(CurrentLayer, LastFrame, NextFrame),
    apply_layers(Rl, NextFrame, Output).

% If the last frame is empty the current frame is the output
apply_layer(Output, [], Output).

apply_layer([CurrentPixel|Rp], [LastFramePixel|RLp], [NewPixel|OtherOutput]):-
    (LastFramePixel = 50
    ->  NewPixel is CurrentPixel
    ;   NewPixel is LastFramePixel
    ),
    apply_layer(Rp, RLp, OtherOutput).

print_image(_, _, []).
print_image(Width, Index, [Pixel|Rp]):-
    NIndex is Index + 1,
    PrintNewline is mod(Index, Width),

    (Pixel = 49
    ->  format("1")
    ;   format(" ")),

    (PrintNewline is 0
    ->  format("\n")
    ;   format("")),
    print_image(Width, NIndex, Rp).
:-
    char_code("1", Number1),
    char_code("2", Number2),

    phrase_from_file(layers(150, Ls), 'input.txt'),
    get_lowest_zero_layer(Ls, Layer),

    get_number_count(Layer, Number1, N1),
    get_number_count(Layer, Number2, N2),

    Answer is N1 * N2,

    format("part 1: ~p\n", Answer).

:-
    format("Part 2: \n"),
    phrase_from_file(layers(150, Ls), 'input.txt'),
    apply_layers(Ls, [], Output),
    print_image(25, 1, Output).
