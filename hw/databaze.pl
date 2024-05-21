extrakce(Database, Attributes) :-
    getAllAttributes(Database, AllAttributes),
    prepare(AllAttributes, EmptyA),
    helper(Database, AllAttributes, EmptyA, Attributes).



prepare([], []).
prepare([A|AllAttributes], [A-[]|Attributes]) :-
    prepare(AllAttributes, Attributes).


helper_([],_,_).
helper_([D|Database], A, Attributes) :-
    helper_(Database, A, Attributes),

helper([], _, _).
helper(Database, [A|AllAttributes], Attributes) :-

    

% helper([D|Database], AllAttributes, [A|Attributes]) :-
%     getAll(D, AllAttributes, A),
%     helper(Database, AllAttributes, Attributes).

% getAll([],[],[]).
% getAll([A-V|D], [B|AllAttributes], [B-nedef|Attributes]) :-
%     getAll([A-V|D], AllAttributes, Attributes).
% getAll([A-V|D], [A|AllAttributes], [A-V|Attributes]) :-
%     getAll(D, AllAttributes, Attributes).



getAttributes([],[]).
getAttributes([A-D|Rest], [A|Attributes]) :-
    getAttributes(Rest, Attributes).


allAttributes([],[]).
allAttributes([Item|Database], Attributes) :-
    getAttributes(Item, A),
    allAttributes(Database, B),
    append(A,B,Attributes).

getAllAttributes(Database, Attributes) :-
    allAttributes(Database, A),
    sort(A, Attributes).

table1(T) :-
    T = [[barva-oranzova, motor-plyn, pocet_mist-40], 
    [barva-modra, motor-diesel, pocet_kol-6],
    [motor-elektro, pocet_mist-5]].