:- module(languages,[language_is/1]).

:- use_module(symptoms).

language_is(Langs) :-
	system_type('machine learning'),
	(environment(cloud) ; environment('cross platform')),
	Options = ['Python'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('machine learning'),
	environment(mobile),
	Options = ['Java'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('machine learning'),
	environment(embedded),
	Options = ['C++'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type(backend),
	(environment(pc) ; environment(cloud)),
	Options = ['TypeScript', 'Python', 'Java', 'C++', haskell, golang],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type(backend),
	environment(embedded),
	Options = ['C++', 'Python'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type(backend),
	environment('cross platform'),
	Options = ['TypeScript', 'Python', 'Java'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('web app'),
	(environment(pc) ; environment('cross platform')),
	Options = ['TypeScript'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('GUI app'),
	(environment(pc) ; environment(mobile)),
	Options = ['Java', 'C++'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('GUI app'),
	environment('cross platform'),
	Options = ['Java'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type('GUI app'),
	environment(embedded),
	Options = ['C++'],
	(check_options(Options, Langs) ; Langs = Options).

language_is(Langs) :-
	system_type(cli),
	environment(pc),
	Options = ['Python', 'TypeScript', haskell, golang],
	(check_options(Options, Langs) ; Langs = Options).

language_is([]).


check_options(Options, Lang):-
	member(Lang, Options),
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


	