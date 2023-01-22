:- module(main,[execute/0]).

:- use_module(utils).
:- use_module(languages).

respond([]):-
	write("I cannot guess which language would work best."), nl.

respond([H|T]) :-
	write("I could not find language matching requirements"), nl,
	write("The best matches were: "), write([H|T]), nl.

respond(Lang) :-
	write("The best language might be "), write(Lang), nl.


execute :-
	language_is(X), !,
	respond(X),
	clear_facts.
