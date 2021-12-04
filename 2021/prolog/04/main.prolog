:- use_module(library(pio)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(apply)).

input(Nums, Boards) --> lines(Nums), boards(Boards).
lines([L|Ls]) --> integer(L), ",", lines(Ls).
lines([L]) --> integer(L), "\n".

boards([B|Bs]) --> "\n", board(B), boards(Bs).
boards([B]) --> "\n", board(B).

board(Board) --> {length(Board, 5)}, rows(Board).

rows([R|Rs]) --> {length(R, 5)}, row(R), "\n", rows(Rs).
rows([R]) --> {length(R, 5)}, row(R), "\n".

row([A|T]) --> blanks, integer(A), row(T).
row([A]) --> blanks, integer(A).

winningrows([R|T]):- rowwinning(R) ; winningrows(T).
winningrows([R]):- rowwinning(R).

rowwinning([-1,-1,-1,-1,-1]).

winning(R):- winningrows(R).
winning(R):- transpose(R, T),  winningrows(T).

board_playnumber([R|T], Number, [Result|NewTail]):- 
    row_playnumber(R, Number, Result), 
    board_playnumber(T, Number, NewTail).

board_playnumber([R], Number, [Result]):- row_playnumber(R, Number, Result).

row_playnumber(Row, Number, Result):- select(Number, Row, -1, Result).
row_playnumber(Row, _, Row).

play_boards([Board|T], Number, [NewBoard|NewTail]):- 
    board_playnumber(Board, Number, NewBoard), 
    play_boards(T, Number, NewTail).

play_boards([Board], Number, [NewBoard]):- 
    board_playnumber(Board, Number, NewBoard).

winner([Board|_], Board):- winning(Board).
winner([_|T], Board):- winner(T, Board).
winner([Board], Board):- winning(Board).

play_until_win([Number|Numbers], Boards, WinningBoard, FinalNumber):- 
    play_boards(Boards, Number, NewBoards),

    (winner(NewBoards, Board)
    ->  WinningBoard = Board,
        FinalNumber = Number
    ; 
         play_until_win(Numbers, NewBoards, WinningBoard, FinalNumber)
    ).

box_value(-1, A, A).
box_value(A, B, C):- plus(A, B, C).

row_sum(Row, Previous, Result):- foldl(box_value, Row, 0, Value), Result is Previous + Value.

board_sum(Board, Sum):- foldl(row_sum, Board, 0, Sum).

play_until_last([Number|Numbers], Boards, WinningBoard, FinalNumber):-
    play_boards(Boards, Number, NewBoards),
    exclude(winning, NewBoards, LosingBoards),
    length(LosingBoards, Losers),

    ((Losers is 0) ->
            member(WinningBoard, NewBoards),
            FinalNumber = Number
        ;
            play_until_last(Numbers, LosingBoards, WinningBoard, FinalNumber)
    ).


:-
 phrase_from_file(input(Numbers, Boards), 'input.txt'),
 play_until_win(Numbers, Boards, Board, FinalNumber),
 board_sum(Board, Sum),
 Answer is Sum * FinalNumber,
 format("~p\n", Answer),
 play_until_last(Numbers, Boards, LastWinner, LastNumber),
 board_sum(LastWinner, P2Sum),
 Answer2 is P2Sum * LastNumber,
 format("~p\n", Answer2),
 halt.
