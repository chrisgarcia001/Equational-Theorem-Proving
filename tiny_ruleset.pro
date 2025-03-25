% This exemplifies a few rewrite rules.
% Predicate Form: rewrite_rule(LHS, RHS, Rule_Name)
rewrite_rule([A,-,A], 0, "= (By Definition)").
rewrite_rule([A,+,B], [B,+,A], "= (Commutative)").
rewrite_rule([A,*,B], [B,*,A], "= (Commutative)").

% Next two rules illustrate a shorter way to do things.
rewrite_rule([[A,*,B],Op,[A,*,C]], [A,*,[B,Op,C]], "= (Factor)"):- member(Op, [+,-]).
rewrite_rule([A,*,[B,Op,C], [[A,*,B],Op,[A,*,C]]], "= (Distribute)"):- member(Op, [+,-]).
