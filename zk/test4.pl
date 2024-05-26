% % A cifry(+Ns, ?N)
% % cifry([1,2,3], N). -> N = 123
% % cifry([1,2,3], 99). -> false
% cifry(Ns, Num) :-
%     cifry_(Ns, Num, _).

% cifry_([N], N, 1).
% cifry_([N|Ns], NewNum, NewOrder) :-
%     cifry_(Ns, Num, Order),
%     NewOrder is Order * 10,
%     NewNum is Num + NewOrder * N.

% % B
% % cifry0(+Ns, ?N)
% cifry0([N|Ns], Num) :-
%     length([N|Ns], M),
%     (
%         M < 2
%     ;   M >= 2,
%         N \= 0
%     ),
%     cifry([N|Ns], Num).

% % C
% % gen(+-Xs)
% vals([0,1,2,3,4,5,6,7,8,9]).
% gen(Xs) :-
%     vals(Vals),
%     gen_(Xs, Vals).

% gen_([], _).
% gen_([X|Xs], Vals) :-
%     select(X, Vals, NewVals),
%     gen_(Xs, NewVals).

% % D
% % alg(+-XS, +-Ys, +-Zs).
% alg(Xs, Ys, Zs) :-
%     maplist(between(0, 9),Xs),
%     maplist(between(0, 9),Ys),
%     maplist(between(0, 9),Zs),

%     cifry0(Xs, NX),
%     cifry0(Ys, NY),
%     cifry0(Zs, NZ),

%     NZ is NX + NY.

% % E
% % alg([[T,R,I], [K,R,A,T], [T,R,I], [D,E,V,E,T]]).
% numbers([], Acc, Acc).
% numbers([Xs|Xss], Numbers, Acc) :-
%     cifry0(Xs, Cifry),
%     numbers(Xss, [Cifry|Numbers], Acc).


% alg(Xss) :-
%     length(Xss, L),
%     L >= 3,

%     maplist(maplist(between(0,9)), Xss),
%     numbers(Xss, [], [Result|Cifry]),
%     sum_list(Cifry, Result).
    





cifry(Ns, N) :-
    cifry_(Ns, N, _).

cifry_([N], N, 1) :- !.
cifry_([N|Ns], NewNum, NewOrder) :-
    cifry_(Ns, Num, Order),
    NewOrder is Order * 10,
    NewNum is Num + N * NewOrder.

cifry0([N|Ns], Num) :-
    length([N|Ns], Len),
    (
        Len < 2
    ;   Len >= 2,
        N \= 0
    ),
    cifry([N|Ns], Num).

vals([0,1,2,3,4,5,6,7,8,9]).

gen(Xs) :-
    vals(V),
    gen_(Xs, V).

gen_([],_).
gen_([X|Xs], Possible) :-
    var(X),
    select(X, Possible, NewPossible),
    gen_(Xs, NewPossible).
gen_([X|Xs], Possible) :-
    nonvar(X),
    gen_(Xs, Possible).

gen2([],Left,Left).
gen2([X|Xs], Possible, Left) :-
    var(X),
    select(X, Possible, NewPossible),
    gen2(Xs, NewPossible, Left).
gen2([X|Xs], Possible, Left) :-
    nonvar(X),
    gen2(Xs, Possible, Left).

gen__([],_).
gen__([Xs|Xss], Possible) :-
    gen2(Xs, Possible, Left),
    gen__(Xss, Left).


alg(Xs, Ys, Zs) :-
    gen(Xs),
    gen(Ys),
    gen(Zs),

    cifry0(Xs, NX),
    cifry0(Ys, NY),
    cifry0(Zs, NZ),

    NZ is NY + NX.

alg2(Xss) :-
    length(Xss, Len),
    Len >= 3,
    reverse(Xss, RXss),

    maplist(gen, Xss),
    maplist(cifry0, RXss, [Sum|Numbers]),

    sum_list(Numbers, Num),
    Sum is Num.
    


    