% cesta(+Graph, +Vertices, -Path)
cesta(G, [V|Vertices], Path) :-
    % bagof(_, cesta_(G, Vertices, [V]), Cs),
    % longest(Cs, Path).
    cesta_(G, Vertices, [V], RevPath),!,
    reverse(RevPath, Path).
    


cesta_(G, Vertices, [P|Path], End) :-
    member(V, Vertices),
    member(P-Edges,G),
    member(V, Edges),
    select(V, Vertices, NewVertices),
    cesta_(G, NewVertices, [V,P|Path], End).

cesta_(_, [], Path, Path).
cesta_(G, [V|_], [P|Path], [P|Path]) :-
    \+ (
        member(P-Edges,G),
        member(V, Edges)
    ).

longest(Xss, L) :-
    longest_(Xss, [], L).

longest_([], Max, Max).
longest_([Xs|Xss], Curr, Max) :-
    length(Xs, N),
    length(Curr, M),

    (
        N > M, !, longest_(Xss, Xs, Max);
        M >= N, !, longest_(Xss, Curr, Max)
    ).


    


% nejcesta()



