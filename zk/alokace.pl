

firstFit(Req, [], Start, [Start-Req], Start) :- !.
firstFit(Req, [A-L|Allocated], Start, [Start-Req, A-L|Allocated], Start) :-
    Start + Req =< A,!.
    % Loc is Start.


firstFit(Req, [A-L|Allocated], Start, [A-L|Loc], UsedLocation) :-
    Start + Req >= A,
    NewStart is A + L,
    firstFit(Req, Allocated, NewStart, Loc, UsedLocation).

merge([A-L], [A-L]).
merge([A-L1, B-L2|Alloc], Merged) :-
    B is A + L1,
    NewL is L1 + L2,

    merge([A-NewL|Alloc], Merged).
merge([A-L1, B-L2|Alloc], [A-L1|Merged]) :-
    B > A + L1,

    merge([B-L2|Alloc], Merged).




alokace(Req, Taken, Loc, NewTaken) :-
    reverse(Req, RevReq),
    alokace_(RevReq, Taken, RevLoc, NewTaken),
    reverse(RevLoc, Loc).

alokace_([], Taken, [], Taken).
alokace_([Req|Reqs], Taken, [Req-Loc|Locations], Merged) :-
    alokace_(Reqs, Taken, Locations, NewTaken),
    firstFit(Req, NewTaken, 0, Unmerged, Loc),
    merge(Unmerged, Merged).
