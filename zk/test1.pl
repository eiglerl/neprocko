:- use_module(library(clpfd)).

test(X, Y) :-
    between(-1,1,N),
    (
        nonvar(Y), var(X),
        X is Y + N;
        nonvar(X), var(Y),
        Y is X + N;
        nonvar(X), nonvar(Y),
        Y is X + N,!
    ).


% find_num([], _) :- fail.
% find_num([A|_], A) :- nonvar(A).
% find_num([A|As], Num) :- var(A), find_num(As, Num).

% hladky(As) :-
%     find_num(As, Num),
%     length(As, Len),
%     Min is Num - Len - 1,
%     Max is Num + Len + 1,
%     maplist(between(Min, Max), As),
%     hladky_(As).

% hladky_([]).
% hladky_([_]).
% hladky_([X,Y|Xs]) :-
%     test(X,Y),
%     hladky_([Y|Xs]).

% hladka([Xs|Xss]) :-
%     hladky(Xs),
%     transpose([Xs|Xss], XssT),
%     hladka_(XssT),
%     hladka_([Xs|Xss]).


% hladka_([]).
% hladka_([R|Xss]) :-
%     hladky_(R),
%     hladka_(Xss).


hladky2([_]).
hladky2([A,B|As]) :-
    (nonvar(A) ; nonvar(B)),
    test(A,B),
    hladky2([B|As]).
hladky2([A,B|As]) :-
    var(A), var(B),
    hladky2([B|As]),
    test(A,B).


hladka2([Xs|Xss]) :-
    hladky2(Xs),
    transpose([Xs|Xss], XssT),
    hladka2_(XssT),
    hladka2_([Xs|Xss]).

hladka2_([]).
hladka2_([Xs|Xss]) :-
    hladky2(Xs),
    hladka2_(Xss).
