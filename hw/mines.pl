is_mine(X) :- X = x.
is_not_mine(X) :- X = o.

% Predicate that makes sure the given List contains only 'x' or 'o'
mines_valid([]).
mines_valid([X|Rest]) :- (is_mine(X) ; is_not_mine(X)), mines_valid(Rest).


miny(Pocty, Miny) :-
    % Make sure the length is same
    same_length(Pocty, Miny),
    % Call recursive helper predicate
    miny_rec(Pocty, Miny, 0).

% Predicate to make sure every number has the correct number of mines in its neighbourhood
% miny_rec(List, Miny, Index)

% List of numbers is empty -> trivial
miny_rec([], _, _).
% Make sure the next number 'X' on the index 'Index' has 'X' mines in its neighbourhood
miny_rec([X|Ps], Miny, Index) :-
    % Get valid neighbours
    get_neighbours(Miny, Index, Neighbours),
    mines_valid(Neighbours),

    % Check if number of mines is correct
    count_mines(Neighbours, MinesCount),
    X is MinesCount,

    % Move onto the next one
    Index2 is Index + 1,
    miny_rec(Ps, Miny, Index2).


% Function to get neighbouring mines to a value (given by as as index into a list)
% get_neighbours(List, Index, Neighbours)

% Index = 0 -> return the first two elements
get_neighbours([X, Y | _], 0, [X, Y]).

% Index = Len(List) - 1 -> return last two elements
get_neighbours(List, Index, Neighbours) :-
    length(List, Len),
    Index is Len - 1, 

    append(_, [X, Y], List),
    Neighbours = [X, Y].

% Otherwise find such a sublist (Neighbours) so that the number of elements before the sublist is Index-1
get_neighbours(List, Index, Neighbours) :-
    Index > 0,
    % The element given by the index and the elements before and after 
    length(Neighbours, 3),
    
    append(Prefix, Sufix, List),
    length(Prefix, PrefixLen),
    PrefixLen is Index - 1,
    append(Neighbours, _, Sufix).

% Predicate that counts the number of mines in the given List
incr(X, A, B) :- ((is_mine(X)) -> B is A + 1; B is A).
count_mines(List, C) :-
    foldl(incr, List, 0, C).