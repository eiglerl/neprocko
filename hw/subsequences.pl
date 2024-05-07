% Predicate sublist(L, M) is true if M is a sublist of L,
% i.e. the elements of M appear contiguously somewhere inside L.
% A sublist must always contain at least one element.

sublist(List, Sublist) :- 
    % Find a prefix
    append(Prefix, _, List),
    % Whose suffix is the given Sublist
    append(_, Sublist, Prefix),
    % Make sure is contains at least one element.
    Sublist \= [].

% Predicate subseq(L, M) that is true if M is a subsequence of L,
% i.e. the elements of M appear in the same order inside L,
% but not necessarily contiguously.
subseq(_, []).
subseq([X|Xs], [X|Ys]) :-
    subseq(Xs, Ys).
subseq([_|Xs], [Y|Ys]) :-
    subseq(Xs, [Y|Ys]).


% Predicate disjoint(L, M, N) that is true if M and N are disjoint subsequences of L.
% This means that M and N must be subsequences (as defined above)
% and that M and N together must contain all the elements of L.
% For simplicity, you may assume that all elements of L are distinct.
disjoint([], [], []).
disjoint([L|Ls], [L|Ms], Ns) :-
    disjoint(Ls, Ms, Ns).
disjoint([L|Ls], Ms, [L|Ns]) :-
    disjoint(Ls, Ms, Ns).
