:- op(550, xfx, ekv).
:- op(500, xfy, imp).
:- op(450, xfy, or).
:- op(400, xfy, and).
:- op(350,  fx, non).


correct(X) :- ground(X), correct_(X).

correct_(X) :- atom(X).
correct_(non F) :- correct_(F).
correct_(X) :- 
    X =.. [Op, L, R],
    member(Op, [ekv, imp, or, and]),
    correct_(L), correct_(R).

sat(F) :-
    correct(F),
    vars(F, Vars),
    genModel(Vars, Model),
    % member(Model, Model),
    eval(F, Model, true), !.


merge(XS, [], XS) :- !.  % Řezem zaručíme, že pro merge([], [], R) nedostaneme dva stejné výsledky.
merge([], YS, YS) :- !.
% merge([X|XS], [X|YS], [X|R]) :-
%     merge(XS, YS, R), !.
% merge([X|XS], [Y|YS], R) :-
%     % X \= Y,
%     ( X @=< Y -> merge(XS, [Y|YS], S), R = [X|S]
%     ; merge([X|XS], YS, S), R = [Y|S]
%     ).
    
merge([X|XS], [Y|YS], R) :-
    % X \= Y,
    ( X @< Y -> merge(XS, [Y|YS], S), R = [X|S]
    ; X @> Y -> merge([X|XS], YS, S), R = [Y|S]
    ; merge(XS, YS, S), R = [X|S]
    ).


vars(X, [X]) :- atom(X), !.
vars(non F, Vars) :- 
    vars(F, Vars), !.
vars(F, Vars) :-
    F =.. [Op, L, R],
    member(Op, [ekv, imp, or, and]),
    vars(L, V1),
    vars(R, V2),
    merge(V1, V2, Vars).

genModel([], []).
genModel([V|Vars], [V:X|Xs]) :-
    member(X, [true, false]),
    genModel(Vars, Xs).

% IN1, IN2, OUT
and(true, true, true).
and(false, _, false).
and(false, false, false).

or(true, _, true).
or(false, true, true).

ekv(true, true, true).
ekv(false, false, true).
ekv(true, false, false).
ekv(false, true, false).

imp(false, _, true).
imp(true, true, true).
imp(true, false, false).

% IN, OUT
non(false, true).
non(true, false).

eval(X, Model, Value) :- atom(X), member(X:Value, Model).
eval(non X, Model, Value) :- 
    eval(X, Model, V),
    non(Value, V).

eval(F, Model, Value) :-
    F =.. [Op, L, R],
    eval(L, Model, Value1),
    eval(R, Model, Value2),
    call(Op, Value1, Value2, V),
    V = Value.

    

