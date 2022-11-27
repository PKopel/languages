:- module(main,[execute/0]).

:- use_module(utils).
:- use_module(languages).

execute :-
	language_is(X), !,
	write('The best language might be '), write(X), nl,
	clear_facts.

execute :-
	write('\nI cannot guess '),
	write('which language would work best.\n\n'), clear_facts.