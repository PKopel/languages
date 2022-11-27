:- module(languages,[execute/0]).

:- dynamic([xpositive/2, xnegative/2]).

language_is(python) :-
	(environment(pc) ; environment(cloud) ; environment(embedded) ; environment(cross_platform)),
	(system_type(cli) ; system_type(backend) ; system_type(machine_learning)),
	memory("moderately important"),
	purpose("proof of concept"),
	typing("dynamic typing"),
	gc(needed).
	
language_is(java) :-
	(environment(pc) ; environment(cloud) ; environment(mobile) ; environment(cross_platform)),
	(system_type(desktop) ; system_type(backend)),
	memory("moderately important"),
	purpose("production environment"),
	typing("static typing"),
	gc(needed).
	
language_is(cpp) :-
	(environment(pc) ; environment(cloud) ; environment(mobile) ; environment(embedded)),
	(system_type(desktop) ; system_type(cli) ; system_type(backend)),
	memory("very important"),
	purpose("production environment"),
	typing("static typing"),
	gc(not_needed).
	
language_is(typescript) :-
	(environment(pc) ; environment(cloud) ; environment(cross_platform)),
	(system_type(web) ; system_type(cli) ; system_type(backend)),
	memory("moderately important"),
	purpose("production environment"),
	typing("static typing"),
	gc(needed).
	
language_is(golang) :-
	(environment(pc) ; environment(cloud)),
	(system_type(cli) ; system_type(backend)),
	memory("very important"),
	purpose("proof of concept"),
	typing("static typing"),
	gc(needed).
	
language_is(haskell) :-
	(environment(pc) ; environment(cloud)),
	(system_type(cli) ; system_type(backend)),
	memory("not important"),
	purpose("proof of concept"),
	typing("static typing"),
	gc(needed).

environment(pc) :-
	positive("is environment", pc).

environment(mobile) :-
	positive("is environment", mobile).

environment(cloud) :-
	positive("is environment", cloud).

environment(embedded) :-
	positive("is environment", embedded).

environment(cross_platform) :-
	positive("is environment", cross_platform).

system_type(desktop) :-
	positive("is system type", desktop).

system_type(web) :-
	positive("is system type", web).

system_type(cli) :-
	positive("is system type", cli).

system_type(backend) :-
	positive("is system type", backend).

system_type(machine_learning) :-
	positive("is system type", machine_learning).

memory("very important") :-
	positive("is lower memory usage", "very important");
	memory("moderately important").

memory("moderately important") :-
	positive("is lower memory usage", "moderately important");
	memory("not important").

memory("not important") :-
	positive("is lower memory usage", "not important").

purpose("proof of concept") :-
	positive("is the purpose of program", "proof of concept").

purpose("production environment") :-
	positive("is the purpose of program", "production environment").

typing("static typing") :-
	positive("do you prefer", "static typing").

typing("dynamic typing") :-
	negative("do you prefer", "static typing").

gc(needed) :-
	positive("is garbage collecting", needed).

gc(not_needed) :-
	positive("is garbage collecting", needed);
	negative("is garbage collecting", needed).

positive(X, Y) :-
	xpositive(X, Y), !.

positive(X, Y) :-
	not(xnegative(X, Y)),
	ask(X, Y, yes_).

negative(X, Y) :-
	xnegative(X, Y), !.

negative(X, Y) :-
	not(xpositive(X, Y)),
	ask(X, Y, no_).

ask(X, Y, yes_) :-
	!, write(X), write(' '), write(Y), write(' ? (y/n)\n'),
	readln([Replay]),
	remember(X, Y, Replay), 
	answer(Replay, yes_).


ask(X, Y, no_) :-
	!, write(X), write(' '), write(Y), write(' ? (y/n)\n'),
	readln([Replay]),
	remember(X, Y, Replay),
	answer(Replay, no_).    

answer(Replay, yes_):-
	sub_string(Replay, 0, _, _, 'y').

answer(Replay, no_):-
	sub_string(Replay, 0, _, _, 'n').

remember(X, Y, Replay) :-
	answer(Replay, yes_),
	assertz(xpositive(X, Y)).

remember(X, Y, Replay) :-
	answer(Replay, no_),
	assertz(xnegative(X, Y)).

clear_facts :-
	write('\n\nPress enter to finish\n'),
	retractall(xpositive(_, _)),
	retractall(xnegative(_, _)),
	readln(_).

execute :-
	language_is(X), !,
	write('The best language might be '), write(X), nl,
	clear_facts.

execute :-
	write('\nI cannot guess '),
	write('which language would work best.\n\n'), clear_facts.


	