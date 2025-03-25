% This file provides a simple, text-based user interface to prove theorems and print the result.
% Requires the file 'derivation_engine.pro' to first be consulted.

% This is the top-level predicate for proving a theorem and pretty-printing. Calls 
% the derive predicate (in derivation_engine.pro) to generate the proof.
% Predicate form: print_proof(LHS, RHS, MaxSteps)
print_proof(LHS, RHS, MaxSteps):-
	derive(LHS, RHS, MaxSteps, P),
	exp_to_string(LHS, L),
	exp_to_string(RHS, R),
	write("THEOREM: "),
	write(L),
	write(" = "),
	write(R),
	write("\n"),
	writeln("PROOF:"),
	print_derivation(P),
	writeln("---"),
	writeln("QED"),
	writeln("").

% This is a utility predicate to pretty-print a derivation
% Predicate form: print_derivation(Derivation)
print_derivation([]).
print_derivation([A|B]):-
	exp_to_string(A,S),
	writeln(S),
	print_derivation(B).
	
% This predicate converts an list-based expression to a string, adding parentheses where needed.
% Predicate form: exp_to_string(Exp, String)
exp_to_string(X,Y):-
	not(is_list(X)),
	elts_to_string(X,Y).
exp_to_string(X,Y):-
	is_list(X),
	elts_to_string(X,Z),
	normalize_space(string(A), Z),
	string_concat("(",A,B),
	string_concat(B,")",Y).

% A utilty predicate to convert a list of expressions to a string (without outer parentheses).
% Predicate form: elts_to_string(ExpressionList, String)
elts_to_string(X,X):- string(X).
elts_to_string(X,Y):- 
	not(is_list(X)),
	term_string(X,Y).
elts_to_string([], "").
elts_to_string([X|Y],Z):-
	not(is_list(X)),
	elts_to_string(X,A),
	elts_to_string(Y,B),
	string_concat(A," ",C),
	string_concat(C,B,Z).
elts_to_string([X|Y],Z):-
	is_list(X),
	exp_to_string(X,A),
	elts_to_string(Y,B),
	string_concat(A," ",C),
	string_concat(C,B,Z).
