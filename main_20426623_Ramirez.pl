% Primera carta
fcard(N, L):-
        fcard(N, 1, L).

fcard(N, X,[X]) :-
    X > N.

fcard(N, X, [X|L]):-
        N >= X,
        X1 is X + 1,
        fcard(N, X1, L).

% Primeras N cartas

ncards(N,J,L):-
        ncards(N, J, 1, 1, L).

ncards(N, _, K, L, [L]):-
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

%nncards

nncards4(N,K,I,J,L):- 
    X1 is I+1,
    nncards4(N,K,I,J,X1,L).

nncards4(N,K,_,_,L,[L]):-
    K > N.

nncards4(N,K,I,J,X1,[X1|L]):-
    N >= K,
    K1 is K+1,
    X2 is (N+2+N*(K-1)+(((I-1)*(K-1)+J-1) mod N)),
    nncards4(N,K1,I,J,X2,L).

nncards3(N,I,J,L):-
	nncards4(N,1,I,J,L1),
	nncards3(N,I,J,L1,L).

nncards3(N,_,J,L,[L]):-
    J >= N.

nncards3(N,I,J,L1,[L1|L]):-
    N >= J,
    J1 is J+1,
    nncards4(N,1,I,J1,L2),
    nncards3(N,I,J1,L2,L).

nncards2(N,I,L):-
    nncards3(N,I,1,L1),
    nncards2(N,I,L1,L).

nncards2(N,I,L,[L]):-
    I >= N.

nncards2(N,I,L1,L):-
    N >= I,
    I1 is I + 1,
    nncards3(N,I1,1,L2),
    append(L1,L2,L3),
    nncards2(N,I1,L3,L).

nncards(N,L):-
    nncards2(N,1,[X|L1]),
    append(L1,X,L).

% Creacion mazo

mazo(N,L):-
    fcard(N, L1),
    ncards2(N,L2),
    append([L1],L2,L3),
    nncards(N,L4),
    append(L3,L4,L).

% Utilizacion maxC
limitarmazo(N,M,LE,L):-
    mazo(N,L1),
    length(L,M),
    append(L2,_,L1),
	melemento(L2,LE,L).

%[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]

% Intercambio de elementos
intercambiarelementos([I|_],L2,X):-
    nth1(I,L2,X).

ie(L1,L2,L):-
    intercambiarelementos(L1,L2,X),
    ieaux(L1,L2,X,L).

ieaux(L1,_,L,[L]):-
    same_length(L1,[L]), !.

ieaux([_|L1],L2,X,[X|L]):-
    intercambiarelementos(L1,L2,E),
    ieaux(L1,L2,E,L).

melemento([P|M],LE,L):-
    ie(P,LE,X),
    mazoelemento(M,LE,X,L).

mazoelemento(L1,_,L,[L]):-
    length(L1,X),
    length([L],X1),
    X2 is X+1,
    X2 = X1.
    
mazoelemento([I|M],L2,X,[X|L]):-
    ie(I,L2,LA),
    mazoelemento(M,L2,LA,L).