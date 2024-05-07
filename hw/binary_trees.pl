maptree(_, nil).
maptree(P, b(L, V, R)) :-
    % Call predicate P for the current node
    call(P, V),

    % Continue recursively
    maptree(P, L),
    maptree(P, R).

size(nil, 0, -1).
size(b(L, _, R), N, H) :-
    % Initialize the number of nodes in both left and right subtree to 0 <= N{L,R} <= N
    between(0, N, NL),
    % between(0, N, NR),
    % Initialize heights in both left and right subtree to -1 <= H{L,R} <= H
    between(-1, H, HL),
    between(-1, H, HR),

    % Make sure the number of nodes and height is correct (plus one for the current node)
    % N is NL + NR + 1,
    NR is N - NL - 1,
    H is max(HL, HR) + 1,
    % Call recursively
    size(L, NL, HL),
    size(R, NR, HR).