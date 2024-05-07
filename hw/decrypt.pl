% True if word E decrypts to word D using the keys K1 and K2.
decrypt_word(E, D, K1, K2) :- 
    % Get character codes from words
    string_codes(E, ECodes),
    string_codes(D, DCodes),

    % Check the codes
    check_codes_with_keys(ECodes, DCodes, K1, K2).

% Check whether codes are rotation of each other
check_codes_with_keys([],[], _, _).
check_codes_with_keys([C|Codes1], [D|Codes2], K1, K2) :-
    check_code(C, D, K1),
    % Call recursively with keys swapped 
    check_codes_with_keys(Codes1, Codes2, K2, K1).

% Check if letter D can be 'rotated' to obtain letter C 
check_code(C, D, K) :-
    D is C + K.
check_code(C, D, K) :-
    D is C + K - 26.

% True if words EWords decrypt to DWords (all of which are in the dictionary) using the keys K1 and K2.
decrypt_all([], _, [], _, _).
decrypt_all([EWord|EWords], Dictionary, [DWord|DWords], K1, K2) :-
    % Try a word from the dictionary
    member(DWord, Dictionary),
    % Try decrypting the DWord into EWord
    decrypt_word(DWord, EWord, K1, K2),
    % Recurively continue
    decrypt_all(EWords, Dictionary, DWords, K1, K2).


% Predicate decrypt(+C, +D, -M) that takes an encrypted text C and a list of words D, and produces the original text M.
% Generates M for every possible pair of keys (K1, K2)
decrypt(C, D, M) :-
    % Get list of words
    split_string(C, " ", " ", W),

    % Initialize keys
    between(0, 26, K1),
    between(0, 26, K2),

    % Try decrypting all words using the keys
    decrypt_all(W, D, N, K1, K2),
    % Join the decrypted words
    atomics_to_string(N, " ", M).
