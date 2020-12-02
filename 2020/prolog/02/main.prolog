:- use_module(library(pio)).
:- use_module(library(dcg/basics)).

lines([L|Ls]) --> password(L), "\n", lines(Ls).
lines([L]) --> password(L).
lines([]) --> [].

password((Low, High, Char, Pass)) --> integer(Low), "-", integer(High), blanks, string([Char]), ": ", string(Pass).

countChar([C|Cs], C, D):-
  countChar(Cs, C, Do),
  D is Do + 1.

countChar([_|Cs], C, Do):-
  countChar(Cs, C, Do).

countChar([C], C, 1).
countChar([_], _, 0).
countChar([], _, 0).

validPasswordOne((Low, High, Char, Pass), 1):-
  countChar(Pass, Char, Count),
  Count >= Low,
  Count =< High.

validPasswordOne(_, 0).

validPasswordsOne([P|Ps], C):-
  validPasswordOne(P, V),
  validPasswordsOne(Ps, Vo),
  C is V + Vo.

validPasswordsOne([], 0).

validPasswordTwo((Low, High, Char, Pass), 1):-
   nth1(Low, Pass, Char),
   \+ nth1(High, Pass, Char).

validPasswordTwo((Low, High, Char, Pass), 1):-
   \+ nth1(Low, Pass, Char),
   nth1(High, Pass, Char).

validPasswordTwo(_, 0).

validPasswordsTwo([P|Ps], C):-
  validPasswordTwo(P, V),
  validPasswordsTwo(Ps, Vo),
  C is V + Vo.

validPasswordsTwo([], 0).

:-
  phrase_from_file(lines(Passwords), 'input.txt'),
  validPasswordsOne(Passwords, Count),
  format("~d\n", Count),
  validPasswordsTwo(Passwords, Count2),
  format("~d\n", Count2),
  halt.