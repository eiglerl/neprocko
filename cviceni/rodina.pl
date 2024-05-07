% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%% DATABAZE PRIBUZENSKYCH VZTAHU %%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % predikaty popsane fakty

% % muz(Kdo) :- Kdo je muz.
% muz(adam).
% muz(kain).
% muz(abel).

% % zena(Kdo) :- Kdo je zena.
% zena(eva).

% % rodic(Kdo, Koho) :- Kdo je rodicem Koho.
% rodic(adam,kain).
% rodic(adam,abel).
% rodic(eva,abel).
% rodic(eva,kain).

% % predikaty popsane pravidly

% % otec(Kdo, Koho):- Kdo je otcem Koho.
% otec(Otec,Dite):-rodic(Otec,Dite), muz(Otec).

% % bratr(Kdo, Koho) :- Kdo je bratrem Koho.
% bratr(Bratr,Osoba):- rodic(R,Bratr),
%                      rodic(R,Osoba),
%                      muz(Bratr),
%                      Bratr \= Osoba.

% % clovek(Kdo) :- Kdo je clovek.
% clovek(X) :- zena(X).
% clovek(X) :- muz(X).