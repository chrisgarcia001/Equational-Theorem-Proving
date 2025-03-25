% This file contains predicates for math expression simplification.
% It is only partially implemented and can perform only additive simplification.

% This predicate performs top-level simplification.
% Predicate Form: simplify(Expression, SimplifiedExpression).
% *NOTE: This predicate is NOT complete - the product expression
%        simplification is incomplete, making this procedure only
%        partially implemented at this point. However, it is complete
%        enough to allow proving of many trigonometric identities.
simplify(SumExp, SimpExp):-
	flatten_sum_exp(SumExp, FlatExp),
	maplist(prod_simplify, FlatExp, ProdSimplified),
	sum_simplify(ProdSimplified, SumSimpExp),
	remove_superfluous_paren(SumSimpExp, SimpExp),
	!.

% This predicate removes unnecessary parentheses.
% Predicate Form: remove_superfluous_paren(OrignalExp, SimplifiedExp)
remove_superfluous_paren(X,X):-
	is_list(X),
	length(X,L),
	>(L,1),
	!.
remove_superfluous_paren([X],Y):-
	remove_superfluous_paren(X,Y),
	!.

% This predicate determines whether an expression is a product expression.
% Predicate Form: is_prod_exp(Exp)
is_prod_exp([X]) :- not(member(X, [+,-,/,*,^])).
is_prod_exp([A, MultOp, B|C]):-
	not(member(A, [+,-,/,*,^])),
	not(member(B, [+,-,/,*,^])),
	member(MultOp, [*, /]),
	is_prod_exp([B|C]),
	!.

% This performs a limited product simplification.
% Predicate Form: prod_simplify(ProductExpression, SimplifiedExpression).
% *NOTE: This predicate is NOT complete - only simplifies 
%        simple product expressions consisting of all numbers.
prod_simplify([], []).
prod_simplify(-[A,*,B], -C):-
	prod_simplify([A,*,B], C),
	!.
prod_simplify([A,*,B], C):-
	number(A),
	number(B),
	C is A * B,
	!.
prod_simplify([A,*,B], [A,*,B]):-
	not(number(A)),
	!.
prod_simplify([A,*,B], [A,*,B]):-
	not(number(B)),
	!.
prod_simplify(X, X):- !.

% This predicate negates a sum expression.
% Predicate Form: negate_sum_exp(InitialExp, NegatedExp)
negate_sum_exp([], []).
negate_sum_exp([AddOp|SumExp], [AddOp|NegSumExp]):-
	member(AddOp, [+,-]),
	negate_sum_exp(SumExp, NegSumExp),
	!.
negate_sum_exp([-A|B], [A|C]):-
	not(member(A, [+,-,/,*,^])),
	negate_sum_exp(B, C),
	!.
negate_sum_exp([A|B], [-A|C]):-
	not(is_sum_exp(A)),
	not(member(A, [+,-,/,*,^])),
	negate_sum_exp(B, C),
	!.
negate_sum_exp([A|B], [An|C]):-
	is_sum_exp(A),
	negate_sum_exp(A, An),
	negate_sum_exp(B, C),
	!.

% This predicate flattens a sum expression (for simplification).
% Predicate Form: flatten_sum_exp(SumExp, FlattenedSumExp)
flatten_sum_exp([], []).
flatten_sum_exp([+|A], [+|B]):- flatten_sum_exp(A, B), !.
flatten_sum_exp([-,A|B], [+,-A|C]):- 
	not(is_sum_exp(A)),
	flatten_sum_exp(B,C),
	!.
flatten_sum_exp([-,A|B], D):- 
	is_sum_exp(A),
	negate_sum_exp(A, An),
	flatten_sum_exp(An, Af),
	flatten_sum_exp(B,C),
	append([+|Af], C, D),
	!.
flatten_sum_exp([-A|B], [-A|C]):- 
	not(is_sum_exp(A)),
	flatten_sum_exp(B, C), 
	!.
flatten_sum_exp([-A|B], D):- 
	is_sum_exp(A),
	negate_sum_exp(A, An),
	flatten_sum_exp(An, Af),
	flatten_sum_exp(B, C),
	append(Af, C, D),
	!.
flatten_sum_exp([A|B], [A|C]):- 
	not(is_sum_exp(A)),
	flatten_sum_exp(B, C), 
	!.
flatten_sum_exp([A|B], D):- 
	is_sum_exp(A),
	flatten_sum_exp(A, Af),
	flatten_sum_exp(B, C),
	append(Af, C, D),
	!.

% This predicate tests whether an expression is a sum expression
% Predicate Form: is_sum_exp(Exp)
is_sum_exp([X]) :- not(member(X, [+,-,/,*,^])).
is_sum_exp([A, AddOp, B|C]):-
	not(member(A, [+,-,/,*,^])),
	not(member(B, [+,-,/,*,^])),
	member(AddOp, [+, -]),
	is_sum_exp([B|C]),
	!.

% This predicate simplifies sum expressions
% Predicate Form: sum_simplify(SumExp, SimplifiedExp)
sum_simplify(SumExp, SimplifiedExp):-
	ht_new(H),
	accumulate_sum_terms(SumExp, 1, H, HN),
	ht_keys(HN, K),
	reverse(K, KK),
	pos_terms(KK, HN, P),
	neg_terms(KK, HN, N),
	combine_sum_terms(P, N, SimplifiedExp),
	!.

% This predicate combines sum terms (positive and negative) into a single expression
% Predicte Form: combine_sum_terms(PosTerms, NegTerms, Exp)
combine_sum_terms(Pos, [], All):- interleave(Pos, +, All), !.
combine_sum_terms([], [[N,*,T]|Neg], All):- 
	interleave([[-N,*,T]|Neg], -, All), 
	!.
combine_sum_terms([], [NT|Neg], All):- 
	interleave([-NT|Neg], -, All), 
	!.
combine_sum_terms(Pos, Neg, All):-
	interleave(Pos, +, P),
	interleave(Neg, -, N),
	append(P, [-], X),
	append(X, N, All),
	!.    

% This predicate takes a list of elements and separator and produces a
% new list consisting of the separator between all original elements.
% Predicate Form: interleave(List, Sep, NewList)
interleave([],_,[]).
interleave([X],_,[X]).
interleave([X|Y], Sep, [X,Sep|Z]):-
	interleave(Y,Sep,Z), !.
	
% This predicate gets the absolute value of a cached sum term in a hashtable.
% Predicate Form: abs_sum_term(HashTable, Term, AbsTerm)
abs_sum_term(H, numeric_val, AV):- ht_get(H, numeric_val, V), AV is abs(V), !.
abs_sum_term(H, T, T):- ht_get(H, T, 1), !.
abs_sum_term(H, T, T):- ht_get(H, T, -1), !.
abs_sum_term(H, T, [AV,*,T]):- ht_get(H, T, V), AV is abs(V), !.

% This predicate gets the positive (i.e. added) terms stored in a hashtable.
% Predicate Form: pos_terms(Exp, Hashtable, PosTerms)
pos_terms([], H, []).
pos_terms([X|Y], H, [T|Z]):-
	ht_get(H,X,V),
	>(V,0),
	abs_sum_term(H, X, T),
	pos_terms(Y,H,Z).
pos_terms([X|Y], H, Z):-
	ht_get(H,X,V),
	=<(V,0),
	pos_terms(Y,H,Z).

% This predicate gets the negative (i.e. subtracted) terms stored in a hashtable.
% Predicate Form: neg_terms(Exp, Hashtable, NegTerms)
neg_terms([], H, []).
neg_terms([X|Y], H, [T|Z]):-
	ht_get(H,X,V),
	<(V,0),
	abs_sum_term(H, X, T),
	neg_terms(Y,H,Z).
neg_terms([X|Y], H, Z):-
	ht_get(H,X,V),
	>=(V,0),
	neg_terms(Y,H,Z).

% This expression constructs a sum expression from terms cached in a hashtable
% Predicate Form: sum_exp_from_table(TermList, HashTable, SumExp)
sum_exp_from_table([], H, []).
sum_exp_from_table([X|Y], H, [X,+|Z]):-
	ht_get(H,X,V),
	=(V,1),
	!.

% This predicate gets the multiple for a valid sum term
% Predicate Form: sum_term_multiple(Term, TermForm, Multiple).
% Example: sum_term_multiple([2*,cos(x)], cos(x), Y)). ==> Y = 2
sum_term_multiple([A,*,X], X, A):- number(A), not(number(X)), !.
sum_term_multiple([X,*,A], X, A):- number(A), not(number(X)), !.
sum_term_multiple(-[A,*,X], X, -A):- number(A), not(number(X)), !.
sum_term_multiple(-[X,*,A], X, -A):- number(A), not(number(X)), !.
sum_term_multiple(-X, numeric_val, -X):- number(X), !.
sum_term_multiple(X, numeric_val, X):- number(X), !.
sum_term_multiple([X,*,Y], numeric_val, Z):- 
	number(X), 
	number(Y),
	Z is X * Y,
	!.
sum_term_multiple(-(-X), X, 1):- !.
sum_term_multiple(-X, X, -1):- !.
sum_term_multiple(X, X, 1):- !.

% This predicate updates a sum term's multiple in a hashtable - used in the accumulation of terms
% Predicate Form: sum_term_table_update(SumTerm, Sign, Hashtable)
sum_term_table_update(X, Sign, H):-
	sum_term_multiple(X,T,V1),
	not(ht_get(H,T,_)),
	V2 is V1 * Sign,
	ht_put(H,T,V2).
sum_term_table_update(X, Sign, H):-
	sum_term_multiple(X,T,V1),
	ht_get(H,T,V2),
	VN is V2 + (Sign * V1),
	ht_put(H,T,VN).

% This predicate is used to accumulate sums of each unique term in a sum expression into a hash table.
% Predicate Form: accumulate_sum_terms(SumTermList, InitHashTable, EndHashTable).
% Example: accumulate_sum_terms([cos(x),+,[2,*,[sin(x),^,2]],+,4,+,9,-,cos(x)], H, HN)
%          => H has pairs [numeric_const-13, cos(x)-0, [sin(x), ^, 2]-2].
accumulate_sum_terms(X,H,HN):-
	accumulate_sum_terms(X,1,H,HN).
accumulate_sum_terms([], _, H, H).
accumulate_sum_terms([+|Y],_,H,HN):-
	accumulate_sum_terms(Y,1,H,HN),
	!.
accumulate_sum_terms([-|Y],_,H,HN):-
	accumulate_sum_terms(Y,-1,H,HN),
	!.
accumulate_sum_terms([X|Y],S,H,HN):-
	sum_term_table_update(X, S, H),
	accumulate_sum_terms(Y,1,H,HN),
	!.
