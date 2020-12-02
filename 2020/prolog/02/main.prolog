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

validPassword((Low, High, Char, Pass), 1):-
  countChar(Pass, Char, Count),
  Count >= Low,
  Count =< High.

validPassword(_, 0).

validPasswords([P|Ps], C):-
  validPassword(P, V),
  validPasswords(Ps, Vo),
  C is V + Vo.

validPasswords([], 0).
:-
  phrase_from_file(lines(Passwords), 'input.txt'),
  validPasswords(Passwords, Count),
  format("~d\n", Count),
  halt.