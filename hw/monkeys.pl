valid_position(P) :- member(P,[a,b,c,d]).

go(P) :- valid_position(P).

go([M1,B1,B2,Ban,Stacked,false,HasBan],[M2,B1,B2,Ban,Stacked,false,HasBan]) :- valid_position(M2), M1 \= M2.

stack([M,M,M,Ban,false,false,HasBan],[M,M,M,Ban,true,false,HasBan]).
unstack([M,M,M,Ban,true,false,HasBan],[M,M,M,Ban,false,false,HasBan]).

push(box2, [M,M,M,Ban,true,false,HasBan],[B,B,B,Ban,true,false,HasBan]) :- valid_position(B), B \= M.
push(box1, [M,M,B2,Ban,false,false,HasBan],[B1,B1,B2,Ban,false,false,HasBan]) :- valid_position(B1), B1 \= M.
push(box2, [M,B1,M,Ban,false,false,HasBan],[B2,B1,B2,Ban,false,false,HasBan]) :- valid_position(B2), B2 \= M.

climb_on_1([M,M,B2,Ban,Stacked,false,HasBan], [M,M,B2,Ban,Stacked,true,HasBan]).
climb_off_1([M,M,B2,Ban,Stacked,true,HasBan], [M,M,B2,Ban,Stacked,false,HasBan]).

grab([M,M,M,M,true,true,false],[M,M,M,M,true,true,true]).


initial_state([a,b,c,d,false,false,false]).
% initial_state([a,a,a,d,false,false,false]).

end([_,_,_,_,_,_,true]). 



find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], go(P), NewState) :-
    NewState = [P,B1,B2,Ban,Stacked,OnBox1,HasBan],
    go([M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).

find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], push(Box, P), NewState) :-
    (
        Box = box1,
        NewState = [P,P,B2,Ban,Stacked,OnBox1,HasBan]
        % push(Box, [M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState)
        ; 
        Box = box2,
        Stacked = false,
        NewState = [P,B1,P,Ban,Stacked,OnBox1,HasBan]
        % push(Box, [M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState)
        ;
        Box = box2,
        Stacked = true,
        NewState = [P,P,P,Ban,Stacked,OnBox1,HasBan]
    ),
    push(Box, [M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).


find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], stack, NewState) :-
    NewState = [M,B1,B2,Ban,true,OnBox1,HasBan],
    stack([M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).

find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], unstack, NewState) :-
    NewState = [M,B1,B2,Ban,false,OnBox1,HasBan],
    unstack([M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).

find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], climb_on_1, NewState) :-
    NewState = [M,B1,B2,Ban,Stacked,true,HasBan],
    climb_on_1([M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).

find_action([M,B1,B2,Ban,Stacked,OnBox1,HasBan], climb_off_1, NewState) :-
    NewState = [M,B1,B2,Ban,Stacked,false,HasBan],
    climb_off_1([M,B1,B2,Ban,Stacked,OnBox1,HasBan], NewState).

find_action([M,B1,B2,Ban,Stacked,OnBox1,false], grab, NewState) :-
    NewState = [M,B1,B2,Ban,Stacked,OnBox1,true],
    grab([M,B1,B2,Ban,Stacked,OnBox1,false], NewState).


solve(P) :-
    initial_state(Initial),
    bfs([[Initial]-[]], [], PRev),
    reverse(PRev, P).


bfs([[State|_]-Output|_], _, Output) :- end(State).
bfs([[State|Prev]-Output|Queue], Visited, Result) :- 
    findall([NewState,State|Prev]-[Action|Output], (find_action(State, Action, NewState), \+ member(NewState, Visited)), NewPaths),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, [State|Visited], Result).



