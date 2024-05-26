cesta(G, [N|Ns], P) :-
    cesta_(G, Ns, [N], PRev),!,
    reverse(PRev, P).
cesta_(G, Ns, [P|Ps], End) :-
    member(P-Edges, G),
    member(N, Edges),
    select(N, Ns, NewNs),

    cesta_(G, NewNs, [N,P|Ps], End).
cesta_(G, [N|Ns], [P|Ps], [P|Ps]) :-
    \+ (
        member(P-Edges, G),
        member(N,Edges),
        select(N, Ns, _)
    ).


    cesta_(_, [], End, End).

% nejcesta(G, N, P) :-
%     cesta(G, N, AlmostP),
