getAttr(X-_,X).

getAttrLine(Attr, DBLine, Val) :-
    member(Attr-Val, DBLine) -> true; Val = nedef.

getAttrAll(DB, Attr, Attr-Vals) :- 
    maplist(getAttrLine(Attr), DB, ValsDup), sort(ValsDup, Vals).

extrakce(Database, Result) :- 
    append(Database, Flat),
    maplist(getAttr, Flat, AttrsDup),
    sort(AttrsDup, Attrs),
    maplist(getAttrAll(Database), Attrs, Result).



