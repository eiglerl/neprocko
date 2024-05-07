flip(lit, out).
flip(out, lit).

same_but_last_two([_,_],[_,_]).
same_but_last_two([X|Xs],[Y|Ys]) :- X=Y, same_but_last_two(Xs, Ys).

same_all([],[]).
same_all([X|Xs],[Y|Ys]) :- X=Y, same_all(Xs, Ys).

% flip3([A,B,C|S],[AFlipped,BFlipped,CFlipped|T], Remaining) :-
%     Remaining > 0,
%     flip(A,AFlipped),
%     flip(B,BFlipped),
%     flip(C,CFlipped),

%     same_all(S,T).

% flip3([A|S],[B|T], Remaining) :-
%     Remaining > 0,

%     append(S,[A],S2),
%     append(T,[B],T2),
%     NewRemaining is Remaining - 1,
%     flip3(S2,T2,NewRemaining).

% flip3(S,T) :-
%     same_length(S,T),
%     length(S, Len),

%     flip3(S,T,Len).

flip3([A,B]-S2, [AFlipped,BFlipped]-T2, Remaining) :-
    Remaining > 0,

    reverse(S2,[C,D|S2Rev]),
    reverse(T2,[E,F|T2Rev]),

    flip3([A,B,C,D]-S2Rev, [AFlipped,BFlipped,E,F]-T2Rev, Remaining).

flip3([A,B,C|S]-S2, [AFlipped,BFlipped,CFlipped|T]-T2, Remaining) :-
    Remaining > 0,
    flip(A, AFlipped),
    flip(B, BFlipped),
    flip(C, CFlipped),
    S = T,
    S2 = T2.

flip3([A|S]-S2, [B|T]-T2, Remaining) :-
    Remaining > 0,
    NewRemaining is Remaining - 1,
    flip3(S-[A|S2], T-[B|T2], NewRemaining).

flip3(S, T) :-
    same_length(S, T),
    length(S, Len),
    flip3(S-[], T-[], Len).


    
% flip3([A,B|S],[C,D|T]) :-
%     same_length(S,T),

%     append([A,B|S],[A,B], SLonger),
%     append([C,D|T],[C,D], TLonger),
    
%     flip3_([]-SLonger,[]-TLonger).


% flip3_(SRest-[A,B,C|S],TRest-[AFlipped,BFlipped,CFlipped|T]) :-
%     flip(A,AFlipped),
%     flip(B,BFlipped),
%     flip(C,CFlipped),

%     (same_all(S,T), same_but_last_two(SRest,TRest) ; \+ same_all(S,T), same_but_last_two(S,T), same_all(SRest,TRest)).
    % same_all(SRest,TRest).

flip3_(SRest-[A|S],TRest-[B|T]):-
    flip3_([A|SRest]-S,[B|TRest]-T).


end(T) :- maplist(=(out), T).
    

pumpkin(Initial, Path) :-
    bfs([[Initial]],[],RevPath),!,
    % bfs([[Initial]],RevPath),!,
    reverse(RevPath, Path).
    
contains([Current|_], State) :-
    member(State, Current), !.
contains([_|Queue], State) :-
    contains(Queue, State).

% bfs([[State|Prev]|_], _, [State|Prev]) :- end(State).
% bfs([[State|Prev]|Queue], Visited, Result) :-
%     findall([NewState,State|Prev], (flip3(State,NewState), \+ contains([[State|Prev]|Queue], NewState)), NewPaths),
%     append(Queue,NewPaths,NewQueue),
%     bfs(NewQueue,[State|Visited],Result).

bfs([[State|Prev]|_], _, [State|Prev]) :- end(State).
bfs([[State|Prev]|Queue], Visited, Result) :-
    findall([NewState,State|Prev], (flip3(State,NewState), \+ contains([[State|Prev]|Queue], NewState)), NewPaths),
    append(Visited, NewPaths, NewVisited),
    sort(NewVisited, SortedVisited),
    append(Queue,NewPaths,NewQueue),
    bfs(NewQueue,SortedVisited,Result).

bfs([[State|Prev]|_], [State|Prev]) :- end(State).
bfs([[State|Prev]|Queue], Result) :-
    findall([NewState,State|Prev], (flip3(State,NewState), \+ contains([[State|Prev]|Queue], NewState)), NewPaths),
    append(Queue,NewPaths,NewQueue),
    bfs(NewQueue,Result).
