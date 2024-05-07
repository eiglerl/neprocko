rotate(L, M) :-
    % Make sure L is bound (so that calling 'append(Start, End, L)' makes sense)
    nonvar(L),
    append(Start, End, L),
    append(End, Start, M);

    % Make sure M is bound
    nonvar(M),
    append(Start, End, M),
    append(End, Start, L).
