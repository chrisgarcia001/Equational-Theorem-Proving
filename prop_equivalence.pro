% This contains rewrite rules and related predicates for 
% propositional equivalence theorem proving.

% This predicate is used to designate logic operators.
% Predicate Form: operator(Op)
operator(->).
operator(and).
operator(or).
operator(~).

% The eqv is a predicate to represent two-way rewrite rules, where
% any side can be replaced with the other.
% Predicate Form: eqv(Side1, Side2, RuleName)
eqv([A,->,B], [[~,A], or, B], "= (Definition of ->)").

% The conditions on DeMorgan's laws are to ensure we don't needlessly add
% negations while flipping back and forth between and/or.
% Three or fewer negations for an expression is always sufficient.
eqv([~,[A, and, B]], [[~,A], or, [~,B]], "= (DeMorgan)"):- 
	not(=(A,[~,[~,[~|_]]])),
	not(=(B,[~,[~,[~|_]]])).
eqv([~,[A, or, B]], [[~,A], and, [~,B]], "= (DeMorgan)"):- 
	not(=(A,[~,[~,[~|_]]])),
	not(=(B,[~,[~,[~|_]]])).

eqv([A, or, B], [B, or, A], "= (Commutative)").
eqv([A, and, B], [B, and, A], "= (Commutative)").

eqv([A, or, [B, or, C]], [[A, or, B], or, C], "= (Associative)").
eqv([A, and, [B, and, C]], [[A, and, B], and, C], "= (Associative)").

eqv([A, or, [B, and, C]], [[A, or, B], and, [A, or, C]], "= (Distributive)").
eqv([A, and, [B, or, C]], [[A, and, B], or, [A, and, C]], "= (Distributive)").

% The rr predicate is shorthand for a single left-to-right rewrite rule.
% Predicate Form: rr(LHS, RHS, RuleName)
rr([A, or, [B, and, [~,B]]], A, "= (Domination)").
rr([A, and, [B, or, [~,B]]], A, "= (Domination)").
rr([A, or, [A, and, B]], A, "= (Absorption)").
rr([A, and, [A, or, B]], A, "= (Absorption)").

rr([A, or, A], A, "= (Idempotent)").
rr([A, and, A], A, "= (Idempotent)").

rr([A, or, true], true, "= (Domination)").
rr([A, or, [~,A]], true, "= (Excluded Middle)").
rr([A, or, false], A, "= (Identity)").
rr([A, and, true], A, "= (Identity)").

rr([A, and, false], false, "= (Domination)").
rr([A, and, [~,A]], true, "= (Non-Contradiction)").

rr([~,true], false,"(= By Definition)").
rr([~,false], true,"(= By Definition)").

% Incorporate the bi-directional eqv rewrite rules under the 
% uni-directional rr rule.
rr(X,Y,R):- eqv(X,Y,R).
rr(X,Y,R):- eqv(Y,X,R).

% The double-negation rule is written as three rules to ensure we 
% don't endlessly increase the negations. Three or fewer negations 
% for an expression is always sufficient.
rr(A, [~,[~,A]], "= (Double Negation)"):- atomic(A), not(operator(A)).
rr([X|Y], [~,[~,[X,[Y|Z]]]], "= Double Negation"):- not(=(X,~)), not(=(Y,~)).
rr([~,[~,A]], A, "= (Double Negation)").

% Define a single top-level rewrite rule that uses rr and ensures only
% valid expressions. This is used directly by the reasoning engine.
rewrite_rule(X,Y,R):- rr(X,Y,R),term_variables([X,Y],L), not(member([], L)).
