% nesplnitelnost
edge(a,b).
edge(b,c).

% correct:
% path(X, X).
% path(X, Y) :- edge(X, Z), path(Z, Y).

% almost correct but tries the longest paths first
% path(X, Y) :- edge(X, Z), path(Z, Y).
% path(X, X).

% incorrect, finds the solution, then infinite recursion:
% path(X, X).
% path(X, Y) :- path(Z, Y), edge(X, Z).

% even worse, does not find the solution, then infinite recursion
path(X, Y) :- path(Z, Y), edge(X, Z).
path(X, X).


% perm(0, _, []).
% perm(N, Xs, [Item|R]) :- member(Item, Xs), delete(Xs, Item, Ys), M is N - 1, comb(M, Ys, R).
comb(0, _, []).
comb(N, Xs, [Item|R]) :- N > 0, append(_, [Item | Ys], Xs), M is N - 1, comb(M, Ys, R).
