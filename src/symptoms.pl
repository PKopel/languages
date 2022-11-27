:- module(symptoms,[system_type/1, environment/1, memory/1, speed/1, purpose/1, typing/1, gc/1]).

:- use_module(utils).


system_type('GUI app') :-
	positive("is system type", 'GUI app').

system_type('web app') :-
	positive("is system type", 'web app').

system_type(cli) :-
	positive("is system type", cli).

system_type(backend) :-
	positive("is system type", backend).

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