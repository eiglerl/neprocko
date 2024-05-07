% Check if X is a number
isNum(X) :- X = zero; X = one.

% Logic AND
and(X, Y) :- X = one, Y = one.
% Logic XOR
xor(X, Y) :- X = one, Y = zero; X = zero, Y = one.

% Addition of two numbers and carryIn, gives result and carryOut
add(X, Y, CIn, R, COut) :-
	% (X + Y + CIn) % 2 = R 
	% X + Y + CIn >= 2 => COut = one

	% If carryIn is one
    CIn = one, 
    (
    and(X, Y), R = one, COut = one;
    xor(X, Y), R = zero, COut = one;
    X = zero, Y = zero, R = one, COut = zero
    );
	
	% Else if carryIn is zero
    CIn = zero, 
    (
    and(X, Y), R = zero, COut = one;
    xor(X, Y), R = one, COut = zero;
    X = zero, Y = zero, R = zero, COut = zero
    ).

% Addition of two 4-bit numbers
add(X3, X2, X1, X0, Y3, Y2, Y1, Y0, Z4, Z3, Z2, Z1, Z0) :-
    isNum(X3), isNum(X2), isNum(X1), isNum(X0),
    isNum(Y3), isNum(Y2), isNum(Y1), isNum(Y0),
    isNum(Z4), isNum(Z3), isNum(Z2), isNum(Z1), isNum(Z0),

    add(X0, Y0, zero, Z0, C0),
    add(X1, Y1, C0, Z1, C1),
    add(X2, Y2, C1, Z2, C2),
    add(X3, Y3, C2, Z3, C3),
    Z4 = C3.