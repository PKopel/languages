:- module(utils,[clear_facts/0, positive/2, negative/2, remember/3]).

:- dynamic([xpositive/2, xnegative/2]).


positive(X, Y) :-
	xpositive(X, Y), !.

positive(X, Y) :-
	not(xnegative(X, Y)),
	not(xpositive(X, _)),
	ask(X, Y, yes_).


negative(X, Y) :-
	xnegative(X, Y), !.

negative(X, Y) :-
	not(xpositive(X, Y)),
	not(xnegative(X, _)),
	ask(X, Y, no_).


ask(X, Y, yes_) :-
	!, write(X), write(' '), write(Y), write(' ? (y/N)\n'),
	readln([Reply]),
	remember(X, Y, Reply), 
	answer(Reply, yes_).

ask(X, Y, no_) :-
	!, write(X), write(' '), write(Y), write(' ? (y/N)\n'),
	readln([Reply]),
	remember(X, Y, Reply),
	answer(Reply, no_).    


answer(Reply, yes_):-
	sub_string(Reply, 0, _, _, 'y').

answer(_, no_).


remember(X, Y, Reply) :-
	answer(Reply, yes_),
	assertz(xpositive(X, Y)).

remember(X, Y, Reply) :-
	answer(Reply, no_),
	assertz(xnegative(X, Y)).


clear_facts :-
	write('\n\nPress enter to finish\n'),
	retractall(xpositive(_, _)),
	retractall(xnegative(_, _)),
	readln(_).