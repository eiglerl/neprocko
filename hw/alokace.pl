% alokace(+Alokovat, +Obsazeno, -Umisteni, -NoveObsazeno)
% alokace([], _, [], []).
% alokace([A|Alokovat], Obsazeno, Umisteni, NoveObsazeno) :-
%     Diff is (To - From),
%     A > Diff,
%     alokace([A|Alokovat], Obsazeno, Umisteni, NoveObsazeno).

% alokace([A|Alokovat], [From-To|Obsazeno], [A-From|Umisteni], [From-To|NoveObsazeno]) :-
%     Diff is (To - From),
%     A =< Diff,
%     alokace(Alokovat, [From-To|Obsazeno], Umisteni, NoveObsazeno).


free([From-Len], [0-From,End-inf]) :- End is From + Len.
free([From-Len|Obsazeno], [0-End,From-X|Free]) :-
    End is From + Len,
    free(Obsazeno, [0-X|Free]).

alokace([], _, Umisteni, NoveObsazeno).
alokace(Alokovat, Obsazeno, Umisteni, NoveObsazeno) :-
    free(Obsazeno, Free),
    alokace_(Alokovat, Free, Umisteni, NoveObsazeno).

alokace_([],_,[],[]).
alokace_([A|Alokovat], Free, [Len-From|Umisteni], NoveObsazeno) :-

    alokace__(A, Free, Len-From),
    alokace_(Alokovat, Free, Umisteni, NoveObsazeno).

alokace__([],[From-To],A-To).
alokace__(A, [From-To|Free], A-From) :-
    Diff is From - To,
    A =< Diff.
alokace__(A, [From-To|Free], Len-From) :-
    Diff is From - To,
    A > Diff,
    alokace__(Alokovat, Free, Len-From).


% test([], []).
% test([(From-To)|Rest], [From|Other]) :-
%     test(Rest, Other).