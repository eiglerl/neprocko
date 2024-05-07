% Given names
man(thomas).
man(david).

woman(stella).
woman(emma).

% Utility predicate
person(A) :- man(A); woman(A).

different(A, B, C, D) :- 
    person(A), person(B), person(C), person(D),
    A \= B, A \= C, A \= D,
    B \= C, B \= D,
    C \= D.

nextTo(A, B) :-
    man(A), woman(B); woman(A), man(B).

% Men and women sit across each other respectively
across(A, B) :-
    man(A), man(B), A \= B;
    
    woman(A), woman(B), A \= B.

solve(Dumplings, Pasta, Soup, Trout) :- 
    % Make sure the names for each food and drink are respectively different.
    different(Dumplings, Pasta, Soup, Trout),
    different(Beer, Cider, IcedTea, Wine),

    % The person with cider sat across from the person with trout.
    across(Cider, Trout),
    % The dumplings came with beer.
    Dumplings = Beer,
    % The mushroom soup came with cider.
    Soup = Cider,
    % The person with pasta sat across from the person with beer.
    across(Pasta, Beer),
    % David never drinks iced tea.
    david \= IcedTea,
    % Emma only drinks wine.
    emma = Wine,
    % Stella does not like dumplings.
    stella \= Dumplings.
