% incr(_, A, B) :- B is A + 1.
% len(List, R) :- foldl(incr, List, 0, R).


% incr(R, _, A, B) :- B is A + 1, B =< R.
% len(List, R) :- foldl(incr(R), List, 0, R).

% :- us_module(library(clpfd)).
% incr(R, _, A, B) :- B is A + 1, B #=< R.
% len(List, R) :- foldl(incr(R), List, 0, R).



lenRev(L, List) :- length(List, L).

genMatrix(M, N, Out) :-
    length(Out, M),
    % maplist(lenRev(N), Out).
    length(Row, N),
    maplist(same_length(Row), Out).



split_one([A|B], A, B).
split_all(M, Col, Rest) :- maplist(split_one, M, Col, Rest).

transpose_matrix(M, []) :- maplist(=([]), M), !.
transpose_matrix(M, [Col|R]) :- split_all(M, Col, Rest), transpose_matrix(Rest, R).
