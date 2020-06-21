:- use_module(library(clpfd)).
:- use_module(library(lists)).

friend(alice, bob).
friend(bob, carol).
friend(carol, daniel).
friend(carol, eve).
friend(frank, daniel).
friend(frank, eve).

friends(A,B) :- friend(A,B); friend(B,A).

%I have attempted all modes, please check them all.

%Is there a relation Rel from S to T whose path is shown by list L of length N
%path(+Rel, ?S, ?T, ?P, ?N)
path(Rel,S,T,L,N) :- pathHelper(Rel,S,T,L,N,[]).

pathHelper(Rel,S,T,[S,T],N,L2) :- N #= 2,call(Rel,S,A), A = T, \+member(A,L2).
pathHelper(Rel,S,T,[S|L],N,L2) :- call(Rel,S,A), \+member(A,L2), pathHelper(Rel,A,T,L,N - 1,[S|L2]).


