:- module(languages,[language_is/1]).

:- use_module(utils).

language_is('Python') :-
	system_type('machine learning'),
	(environment(cloud) ; environment('cross platform')).
language_is('Python') :-
	system_type(backend),
	(environment(pc) ; environment(cloud) ; environment(embedded) ; environment('cross platform')),
	language_characteristics('Python').

language_is('TypeScript') :-
	(system_type('web app') ; system_type(backend)),
	(environment(pc) ; environment(cloud) ; environment('cross platform')),
	language_characteristics('TypeScript').
	
language_is('Java') :-
	system_type('machine learning'),
	environment(mobile).
language_is('Java') :-
	system_type('GUI app'),
	(environment(pc) ; environment(mobile) ; environment('cross platform')),
	language_characteristics('Java').
language_is('Java') :-
	system_type(backend),
	(environment(pc) ; environment(cloud) ; environment('cross platform')),
	language_characteristics('Java').
	
language_is('C++') :-
	system_type('machine learning'),
	environment(embedded).
language_is('C++') :-
	system_type('GUI app'),
	(environment(pc) ; environment(mobile) ; environment(embedded)),
	language_characteristics('C++').
language_is('C++') :-
	system_type(backend),
	(environment(pc) ; environment(cloud) ; environment(embedded)),
	language_characteristics('C++').

language_is(Lang) :-
	(system_type(cli) ; system_type(backend)),
	(environment(pc) ; environment(cloud)),
	(Lang = 'Python' ; Lang = 'TypeScript' ; Lang = haskell ; Lang = golang),
	language_characteristics(Lang).


language_characteristics('Python'):-
	memory("moderately important"),
	speed("not important"),
	purpose("proof of concept"),
	typing("dynamic typing"),
	gc(needed).

language_characteristics('Java'):-
	memory("moderately important"),
	speed("moderately important"),
	purpose("production environment"),
	typing("static typing"),
	gc(needed).

language_characteristics('C++'):-
	memory("very important"),
	speed("very important"),
	purpose("production environment"),
	typing("static typing"),
	gc(not_needed).

language_characteristics('TypeScript'):-
	memory("moderately important"),
	speed("not important"),
	purpose("production environment"),
	typing("static typing"),
	gc(needed).

language_characteristics(golang):-
	memory("very important"),
	speed("moderately important"),
	typing("static typing"),
	gc(needed).

language_characteristics(haskell):-
	memory("not important"),
	speed("moderately important"),
	purpose("proof of concept"),
	typing("static typing"),
	gc(needed).


system_type('GUI app') :-
	positive("is system type", 'GUI app'),
	(environment(pc) ; environment(mobile)). % GUI apps doesn't make sense enywhere else

system_type('web app') :-
	positive("is system type", 'web app'),
	(environment(pc) ; environment(mobile) ; environment('cross platform')). % web apps doesn't make sense enywhere else

system_type(cli) :-
	positive("is system type", cli),
	remember("is environment", pc, "y"). % CLIs doesn't make sense enywhere else

system_type(backend) :-
	positive("is system type", backend),
	(environment(cloud) ; environment(embedded)).

system_type('machine learning') :-
	positive("is system type", 'machine learning').


environment(pc) :-
	positive("is environment", pc).

environment(mobile) :-
	positive("is environment", mobile).

environment(cloud) :-
	positive("is environment", cloud).

environment(embedded) :-
	positive("is environment", embedded).

environment('cross platform') :-
	positive("is environment", 'cross platform').


memory("very important") :-
	positive("is lower memory usage", "very important").
memory("very important") :-
	memory("moderately important").

memory("moderately important") :-
	positive("is lower memory usage", "moderately important").
memory("moderately important") :-
	memory("not important").

memory("not important") :-
	positive("is lower memory usage", "not important").


speed("very important") :-
	positive("is speed", "very important").
speed("very important") :-
	speed("moderately important").

speed("moderately important") :-
	positive("is speed", "moderately important").
speed("moderately important") :-
	speed("not important").

speed("not important") :-
	positive("is speed", "not important").


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


	