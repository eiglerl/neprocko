muz(ja).
muz(muj_otec).
muz(muj_syn).
muz(syn_dcery).

zena(vdova).
zena(vdova_dcera).


manzele(ja, vdova).
manzele(vdova_dcera, muj_otec).

manzeleSym(X, Y) :- manzele(X, Y); manzele(Y, X).

% vlastni
bio_rodic(ja, muj_syn).
bio_rodic(vdova, muj_syn).
bio_rodic(vdova, vdova_dcera).
bio_rodic(muj_otec, syn_dcery).
bio_rodic(vdova_dcera, syn_dcery).
bio_rodic(muj_otec, ja).
% nevlastni
% rodic(vdova, muj_syn).
% rodic(ja, vdova_dcera).
% rodic(vdova_dcera, ja).
rodic(Kdo, Koho) :-
    bio_rodic(R, Koho),
    manzeleSym(Kdo, R);
    bio_rodic(Kdo, Koho).

zet(Kdo, Koho) :-
    manzeleSym(Kdo, Y),
    rodic(Koho, Y),
    muz(Kdo).


bratr(Kdo, Koho) :-
    muz(Kdo),
    rodic(R, Kdo),
    rodic(R, Koho),
    Kdo \= Koho.

sestra(Kdo, Koho) :-
    zena(Kdo),
    rodic(R, Kdo),
    rodic(R, Koho),
    Kdo \= Koho.

stryc(Kdo, Koho) :-
    bratr(Kdo, R),
    rodic(R, Koho),
    muz(Kdo),
    Kdo \= Koho.

vnuk(Kdo, Prarodic) :-
    rodic(Prarodic, R),
    rodic(R, Kdo),
    muz(Kdo).

svagr(Kdo, Koho) :-
    bratr(Kdo, P),
    manzeleSym(Koho, P).

svagr(Kdo, Koho) :-
    sestra(P, Koho),
    manzeleSym(Kdo, P).

prarodic(Kdo, Koho) :-
    rodic(Kdo, X),
    rodic(X, Koho).

