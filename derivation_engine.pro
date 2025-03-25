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
			     rewrite(X,Z,R), add_tabu_list(Tabu,X),
			     derive(Z,Y,M,D,Tabu), !.

% Tabu List Predicates (Implemented using SWI Prolog's hashtable data structure)
new_tabu_list(X):- ht_new(X).
in_tabu_list(List, X):- ht_get(List, X, true).
add_tabu_list(List, X):- ht_put(List, X, true).
