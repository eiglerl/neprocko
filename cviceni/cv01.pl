% muz(jirka).
% muz(pavel).
% muz(adam).

% zena(marie).
% zena(adela).
% zena(jitka).

% rodic(jirka, marie).
% rodic(jirka, adam).
% rodic(jitka, marie).
% rodic(pavel, jirka).

% tata(X, Y) :- rodic(X, Y), muz(X).

% manzele(jirka, jitka).

% manzeleSym(X, Y) :- manzele(X, Y).
% manzeleSym(X, Y) :- manzele(Y, X).

% bratr(Kdo, Koho) :- 
%     rodic(R, Kdo),
%     rodic(R, Koho),
%     muz(Kdo),
%     Kdo \= Koho.

% % Hadanka




