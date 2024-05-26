% SUBSEQUENCES
sublist([],[]).
sublist(L, M) :-
    append(Prefix, _, L),
    append(_, M, Prefix),
    M \= [].

subseq(_, []).
subseq([M|Ls], [M|Ms]) :- subseq(Ls, Ms).
subseq([L|Ls], [M|Ms]) :- subseq(Ls, [M|Ms]), L \= M.

disjoint([], [], []).
disjoint([L|Ls], [L|Ms], N) :- disjoint(Ls, Ms, N).
disjoint([L|Ls], M, [L|Ns]) :- disjoint(Ls, M, Ns).

% MINES
check(0, [o,o,o]).
check(1, [o,o,x]).
check(1, [o,x,o]).
check(1, [x,o,o]).
check(2, [x,x,o]).
check(2, [x,o,x]).
check(2, [o,x,x]).
check(3, [x,x,x]).


miny(Counts, Mines) :-
    same_length(Counts, Mines),
    append([o], Mines, H),
    append(H, [o], ExtendedMines),

    helper(Counts, ExtendedMines).


helper([], [_,_]).
helper([C|Counts], [M1,M2,M3|ExtendedMines]) :-
    check(C, [M1,M2,M3]),
    helper(Counts, [M2,M3|ExtendedMines]).

% BINARY TREES
maptree(_, nil).
maptree(P, b(L, V, R)) :- 
    call(P, V),
    maptree(P, L),
    maptree(P, R).

size(nil, 0, -1).
size(b(L, _, R), N, H) :-
    N > 0,
    H > -1,
    between(0,N,NL),
    NR is N - NL - 1,

    between(-1,H,HL),
    between(-1,H,HR),
    H is max(HL, HR) + 1,
    size(L, NL, HL),
    size(R, NR, HR).

% JACK-O-LANTERN
flip(out, lit).
flip(lit, out).

flip3(S, T) :-
    same_length(S, T),
    
    (   append(Prefix, Postfix, S), append([A,B,C], Rest, Postfix), append(Prefix, Postfix1, T), append([A1,B1,C1], Rest, Postfix1)
    ;   append([A,B], Rest, S), append(Same, [C], Rest), append([A1,B1], Rest1, T), append(Same, [C1], Rest1)
    ;   append([A], Rest, S), append(Same, [B,C], Rest), append([A1], Rest1, T), append(Same, [B1,C1], Rest1)
    ),
    flip(A,A1),
    flip(B,B1),
    flip(C,C1).



