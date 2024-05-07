create_words([], Words).
create_words(Lists, Words) :-
    select(X, Lists, Rest),
    select(Y, Rest, RestRest),
    create_words(RestRest, Words)