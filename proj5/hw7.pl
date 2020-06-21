:- use_module(library(clpfd)).

%dfa(++FA)
%True when FA is deterministic, containing no null moves or ambiguous transitions.
dfa([state(_,Y,_)|[]]) :- noNullMoves(Y).
dfa([state(_,Y,_),X|L]) :- noNullMoves(Y), dfa([X|L]).

noNullMoves([read(_,_)|[]]).
noNullMoves([read(_,_),X|L]) :- noNullMoves([X|L]).

%fa_state(+Name, ++FA, -State)
fa_state(Name,[state(Name,Y,Accept)|[]],state(Name,Y,Accept)).
fa_state(Name,[state(Name,Y,Accept),X|L],state(Name,Y,Accept)) :- fa_state(Name,[X|L],state(Name,Y,Accept)).

%next_state(++State, ++Input, -Name, -Leftover)
next_state(state(Name,
