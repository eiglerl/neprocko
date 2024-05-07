% solve(Cs, Prolog, Swift, Linux, Mac, Win) :-
%     different(Cs, Prolog, Swift),
%     different(Linux, Mac, Win),
%     bill \= Mac,
%     Win = Cs,
%     bill \= Linux,
%     ada \= Swift,
%     Swift = Mac.

% person(ada).
% person(bill).
% person(steve).

% different(A, B, C) :-
%     person(A), person(B), person(C),
%     A \= B, B \= C, C \= A.


% somewhere(Person, friends(O1, O2, O3)) :-
%     Osoba = O1; Osoba = O2; Osoba = O3.

% solution(R) :-
%     R = friends(person(ada, _, _), person(bill, _, _), person(steve, _, _)),
%     somewhere(person(Mac, _, mac), R), Mac \= bill,
%     somewhere(person(_, cs, win), R),
%     somewhere(person(Linux, _, linux), R), Linux \= bill,
%     somewhere(person(Swift, swift, mac), R), Swift \= ada,
%     somewhere(person(_, prolog, _), R).


nat(0).
nat(s(N)) :- nat(N).

leq(0, Y) :- nat(Y).
leq(s(X), s(Y)) :- leq(X, Y).

add(0, Y, Y) :- nat(Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(0, Y, 0) :- nat(Y).
mult(s(X), Y, R) :-
    mult(X, Y, R2),
    add(R2, Y, R).

subtrack(X, Y, R) :- add(Y, R, X).

% div(X, Y, Q, R) <=> X / Y = Q, X % Y = R
div(X, Y, 0, X) :- leq(s(X), Y).%leq(X, Y), X \= Y.
div(X, X, 1, 0) :- nat(X).

% 5 / 2 = 2, 1 
div(X, Y, s(Q), R) :-
    % leq(Y, X),
    subtrack(X, Y, XminusY),
    div(XminusY, Y, Q, R).

% div(s(X), Y, Q, R) :-




    
    
