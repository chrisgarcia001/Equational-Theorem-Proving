% Term Rewriting Predicates 
% Predicate Form: rewrite(LHS, RHS, RuleName)
rewrite(X,Y,R):- rewrite_rule(X,Y,R).
rewrite([X|Z],[Y|Z],R):- rewrite(X,Y,R).
rewrite([X|Y],[X|Z],R):- rewrite(Y,Z,R).

% Top-Level Derivation Predicate 
% Predicate Form: derive(InitialExp, TargetExp, MaxSteps, Proof)
derive(W,X,Y,Z):- new_tabu_list(Tabu), derive(W,X,Y,Z,Tabu).

% Main Derivation Predicate (Constructs the proof)
% Predicate Form: derive(InitialExp, TargetExp, MaxSteps, Proof, TabuList)
derive(X,X,_,_,_):- !.
derive(X,Y,_,[X,R,Y],Tabu):- not(in_tabu_list(Tabu,X)), rewrite(X,Y,R), !.
derive(X,Y,N,[X,R|D],Tabu):- not(in_tabu_list(Tabu,X)), N>1, M is N-1,
			     rewrite(X,Z,R), add_tabu_list(Tabu,X,NewTabu),
			     derive(Z,Y,M,D,NewTabu), !.

% Tabu List Predicates (Implemented using normal lists for portability - can be faster with other data structures)
new_tabu_list([]).
in_tabu_list(TabuList, X):- member(X, TabuList).
add_tabu_list(TabuList, X, [X|TabuList]).
