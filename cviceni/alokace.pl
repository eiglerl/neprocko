% firstFit(Start, Len, Alloc, Pos, NewAlloc).
firstFit(Start, Len, [], Start, [Start-Len]).
firstFit(Start, Len, [Alloc-AllocLen|Rest], Pos, NewTaken) :-
    Start + Len =< Alloc -> Pos = Start, NewTaken = [Start-Len,Alloc-AllocLen|Rest];
    
    NewStart is Alloc + AllocLen,
    firstFit(NewStart, Len, Rest, Pos, RestTaken), NewTaken = [Alloc-AllocLen|RestTaken].


merge([],[]).
merge([X],[X]).
merge([A-AL, B-BL|Rest], New) :-
    B is A + AL -> L is AL + BL, merge([A-L|Rest], New);
    merge([B-BL|Rest], RestNew), New = [A-AL|RestNew].

alokace([], Taken, [], Taken).
alokace([Req|Reqs], Taken, [Req-Pos|PosRest], NewTaken) :-
    firstFit(0, Req, Taken, Pos, Unmerged),
    merge(Unmerged, Merged),
    alokace(Reqs, Merged, PosRest, NewTaken).