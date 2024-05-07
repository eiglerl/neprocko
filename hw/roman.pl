:- use_module(library(clpfd)).

table(T) :- T =[
    900 : [c, m], 1000 : [m],
    400 : [c, d], 500 : [d],
    90 : [x, c], 100 : [c],
    40 : [x, l], 50 : [l],
    9 : [i, x], 10 : [x],
    4 : [i, v], 5 : [v],
    1 : [i]
    ].

% roman(N, R) :- table(T), member(N:R, T).
% roman(N, List) :-
%     table(T),
%     member(Number:R, T),
%     Number #=< N,
%     N #< 4000,
%     append(R, L, List),
%     Less #= N - Number,
%     roman(Less, L).

