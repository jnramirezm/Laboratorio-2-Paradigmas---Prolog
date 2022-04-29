incrementar(L, N):-
        incrementar(L, N, 1).
incrementar([], N, X) :-
    X>N,
    !.
incrementar([X|L], N, X):-
        X =< N,
        X1 is X + 1,
        incrementar(L, N, X1).


ncards(L, N):-
        ncards(L, N, 1,1).
ncards([1|L], N, J, X):-
        J > N ,
        !
ncards([X|L],N,J,X):-
        J =< N:
        X1 is (J*N)+K+1
