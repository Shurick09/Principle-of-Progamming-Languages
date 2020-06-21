:- use_module(library(clpfd)).

%tree(?Tree) : Checks if Tree is a tree
tree(nil).
tree(b(Left,_,Right)) :- tree(Left), tree(Right).

%mirror(+Tree1, ?Tree2) Tree1 and Tree2 should be mirror images of each other
mirror(nil,nil).
mirror(b(Left,Here,Right),b(Left2,Here,Right2)) :- mirror(Left,Right2), mirror(Right,Left2).

%echo(+L1, ?L2) True if L1 and L2 are lists, and L2 contains each element in L1 twice in a row.
echo([],[]).
echo([X|L1],[X,X|L2]) :- echo(L1,L2).  

%unecho(+L1, ?L2) True if L1 and L2 are lists, and any consecutive repetitions in L1 are single elements in L2.
unecho([],[]).
unecho([X],[X]).
unecho([X,X|L1],[X|L2]) :- unechoHelper([X|L1],[X|L2]).
unecho([X,Y|L1],[X|L2]) :- X #\= Y, unecho([Y|L1],L2).

unechoHelper([],[]).
unechoHelper([X],[X]).
unechoHelper([X,X|L1],[X|L2]) :- unechoHelper([X|L1],L2).
unechoHelper([X,Y|L1],[X|L2]) :- unecho([Y|L1],L2).

%slist(?List) List is a non-decreasing list of integers
slist([]).
slist([_]).
slist([X,Y|L1]) :- X #=< Y, slist([Y|L1]).

%sselect(?Item, ?List0, ?List1) Inserts item into List0 to create List1
sselect(X,[Y],[Y,X]) :- Y #=< X.
sselect(X,[Y],[X,Y]) :- X #=< Y.
sselect(X,[Y,Z|L1],[Y|L2]) :- Y #=< Z,X #> Y,sselect(X,[Z|L1],L2).
sselect(X,[Y,Z|L1],[X|L2]) :- Y #=< Z, X #=< Y, [Y,Z|L1] = L2.
