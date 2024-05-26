:- use_module(library(clpfd)).

test(X, Y) :-
    between(-1,1, Diff),
    (
        nonvar(X), var(Y), Y is X + Diff
    ;   nonvar(Y), X is Y + Diff
    ).

hladky([_]).
hladky([A,B|Xs]) :- 
    nonvar(A),
    test(A,B),
    hladky([B|Xs]).
hladky([A,B|Xs]) :-
    var(A),
    hladky([B|Xs]),
    test(A,B).


hladka([Xs|Xss]) :-
    hladky(Xs),
    transpose([Xs|Xss], XssT),
    hladka_(XssT),
    hladka_(Xss).

hladka_([]).
hladka_([Xs|Xss]) :-
    hladky(Xs),
    hladka_(Xss).

