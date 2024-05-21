man(david).
man(thomas).

woman(stella).
woman(emma).

person(X) :- man(X);woman(X).

different(A,B,C,D) :-
    A \= B, A \= C, A \= D,
    B \= C, B \= D,
    C \= D.

across(X, Y) :- (man(X), man(Y)) ; (woman(X), woman(Y)).

solve(Dumplings, Pasta, Soup, Trout) :- solveAll(Dumplings, Pasta, Soup, Trout, Cider, Beer, IcedTea, Wine).

solveAll(Dumplings, Pasta, Soup, Trout, Cider, Beer, IcedTea, Wine) :-
    person(Dumplings), person(Pasta), person(Soup), person(Trout), person(Cider), person(Beer), person(IcedTea), person(Wine),
    different(Dumplings, Pasta, Soup, Trout),
    different(Cider, Beer, IcedTea, Wine),

    across(Cider, Trout),
    Dumplings = Beer,
    Soup = Cider,
    across(Pasta, Beer),
    IcedTea \= david,
    Wine = emma,
    Dumplings \= stella.


    % Dumplings = david,
    % Pasta = thomas,
    % Soup = stella,
    % Trout = emma ;
    % false.