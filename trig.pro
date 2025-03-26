% This file contains predicates for algebraic and trigonometric manipulations.
% It requires the file math_util.pro to first be consulted. 

% The rr predicate is a shorthand one-way rewrite rule
% Predicate Form: rr(LHS, RHS, RuleName)
rr([X,*,[1,/,X]], 1, "= (Simplify)").
rr([X,/,X], 1, "= (Simplify)").
rr([1,*,X], X, "= (Simplify)").
rr([X,*,1], X, "= (Simplify)").
rr([X,/,1], X, "= (Simplify)").
rr([X,+,0], X, "= (Simplify)").
rr([0,+,X], X, "= (Simplify)").
rr([X,-,X], 0, "= (Simplify)").
rr([-X,+,X],0, "= (Simplify)").
rr([[X,^,2],/,X], X, "= (Division)").
rr([A,+,B], [B,+,A], "= (Commutative)").
rr([A,*,B], [B,*,A], "= (Commutative)").
rr([[A,/,B],*,[B,/,C]], [A,/,C], "= (Cancel and Multiply)").
rr([[A,/,B],*,[C,/,A]], [C,/,B], "= (Cancel and Multiply)").
rr([[A,/,B],*,[B,/,A]], 1, "= (Simplify)").
rr([[A,/,B],*,B], A, "= (Simplify)").
rr([B,*,[A,/,B]], A, "=  (Simplify)").
rr([[1,/,A],*,[1,/,B]], [1,/,[A,*,B]], "= (Fraction Multiply)").
rr([A,/,[B,/,C]], [[A,*,C],/,B], "= (Fraction Divide)").
rr([A,/,[A,*,B]], [1,/,B], "= (Cancel)").
rr([B,/,[A,*,B]], [1,/,A], "= (Cancel)").
rr([[A,+,B],*,[C,+,D]], [[A,*,C],+,[A,*,D],+,[B,*,C],-,[B,*,D]], "= (FOIL)").
rr([[A,+,B],*,[C,-,D]], [[A,*,C],-,[A,*,D],+,[B,*,C],-,[B,*,D]], "= (FOIL)").
rr([[A,-,B],*,[C,+,D]], [[A,*,C],+,[A,*,D],-,[B,*,C],-,[B,*,D]], "= (FOIL)").
rr([[A,-,B],*,[C,-,D]], [[A,*,C],-,[A,*,D],-,[B,*,C],+,[B,*,D]], "= (FOIL)").

rr(X,Y,R):- eqv(X,Y,R).
rr(X,Y,R):- eqv(Y,X,R).

% The eqv predicate is a shorthand bi-directional rewrite rule.
% Predicate Form: eqv(Side1, Side2, RuleName)
eqv([A,*,[B,/,C]], [[A,*,B],/,C], "= (Fraction Multiply)").
eqv([[A,/,B],*,[C,/,D]], [[A,*,C],/,[B,*,D]], "= (Fraction Multiply)").
eqv([A,*,[B,+,C]], [[A,*,B],+,[A,*,C]], "= (Distribute)").
eqv([A,*,[B,-,C]], [[A,*,B],-,[A,*,C]], "= (Distribute)").
eqv([[B,+,C],*,A], [[B,*,A],+,[C,*,A]], "= (Distribute)").
eqv([[B,-,C],*,A], [[B,*,A],-,[C,*,A]], "= (Distribute)").
eqv([[A,+,B],/,C], [[A,/,C],+,[B,/,C]], "= (Fraction Equivalence)"). 
eqv([[A,-,B],/,C], [[A,/,C],-,[B,/,C]], "= (Fraction Equivalence)"). 
eqv([[A,*,B],/,C], [[A,/,C],*,B], "= (Fraction Equivalence)").

eqv(tan(x), [sin(x),/,cos(x)], "= (Def. TAN)").
eqv(csc(x), [1,/,sin(x)], "= (Def. CSC).").
eqv(sec(x), [1,/,cos(x)], "= (Def. SEC).").
eqv(cot(x), [1,/,tan(x)], "= (Def. COT).").
eqv(cot(x), [cos(x),/,sin(x)], "= (Alt. Def. COT).").

eqv([csc(x),^,2], [1,/,[sin(x),^,2]], "= (Def. CSC).").
eqv([sec(x),^,2], [1,/,[cos(x),^,2]], "= (Def. SEC).").
eqv([cot(x),^,2], [1,/,[tan(x),^,2]], "= (Def. COT).").
eqv([cot(x),^,2], [[cos(x),^,2],/,[sin(x),^,2]], "= (Def. CSC).").

eqv([sin(x),^,2], [1,/,[csc(x),^,2]], "= (Inverted CSC).").
eqv([cos(x),^,2], [1,/,[sec(x),^,2]], "= (Inverted SEC).").

eqv([[sin(x),^,2],+,[cos(x),^,2]], 1, "= (Pythag. Ident.)").
eqv([sin(x),^,2], [1,-,[cos(x),^,2]], "= (Since sin(x)^2 + cos(x)^2 = 1)").
eqv([cos(x),^,2], [1,-,[sin(x),^,2]], "= (Since sin(x)^2 + cos(x)^2 = 1)").

eqv([sec(x),^,2], [1,+,[tan(x),^,2]], "= (Pythag. Ident.)").
eqv([tan(x),^,2], [[sec(x),^,2],-,1], "= (Since sec(x)^2 = tan(x)^2 + 1)").

eqv([csc(x),^,2], [1,+,[cot(x),^,2]], "= (Pythag. Ident.)").
eqv([cot(x),^,2], [[csc(x),^,2],-,1], "= (Since csc(x)^2 = cot(x)^2 + 1)").

eqv([X,/,csc(x)], [X,*,sin(x)], "= (By Def. of CSC)").
eqv([X,/,sec(x)], [X,*,cos(x)], "= (By Def. of SEC)").

eqv([X,*,X], [X,^,2], "= (Def. Square)").

% This predicate is the top-level rewrite rule predicate used by the 
% reasoning engine. It has two cases:
%   1) Simplify an expresion using the simplifier in math_util.pro
%   2) Rewrite an expression using the rules above
% Predicate Form: rewrite_rule(LHS, RHS, RuleName)
rewrite_rule(X, Z, "= (Simplify)") :-
	is_sum_exp(X),
	length(X, L),
	>(L, 2),
	simplify(X, Z).
rewrite_rule(X,Y,R):- 
	rr(X,Y,R).
