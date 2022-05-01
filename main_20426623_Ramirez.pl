% Primera carta
fcard(L, N):-
        fcard(L, N, 1).

fcard([], N, X) :-
    X>N,
    !.

fcard([X|L], N, X):-
        X =< N,
        X1 is X + 1,
        fcard(L, N, X1).


% Primeras N cartas

ncards(N,J,L):-
    	J1 is J+1,
        ncards(N, J, 1, 1, L).

ncards(N, J, K, L, [L]):-
    K > N.

ncards(N, J, K,X,[X|L]):-
        N >= K,
        X1 is (N * J + (K+1)),
        K1 is K + 1,
        ncards(N, J, K1, X1, L).

ncards2(N,L):-
    	ncards(N,1,L1),
        ncards2(N,2,L1,L).

ncards2(N,J,L,[L]):-
        J > N.

ncards2(N,J,L1,[L1|L]):-
        N >= J,
        ncards(N,J,L2),
        J1 is J+1,
        ncards2(N, J1, L2, L).
