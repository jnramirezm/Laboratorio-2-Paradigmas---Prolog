/*-----------------------------------
  _          _      ____      ____          
 | |        / \    | __ )    |___ \      
 | |       / _ \   |  _ \      __) |  
 | |___   / ___ \  | |_) |    / __/    
 |_____| /_/   \_\ |____/    |_____|   

-------------------------------------

 Juan Ramirez Montero
 
 ----------------- TDA CARDSSET Constructor ------------
 
 Dominios:
 N, J, K, X, M, Xn,Xn1:	 				Enteros
 L, L1, Mazo, Carta, LE, Xs, MazoL: 	Estructura Lista
 v: 					 				Atomic
 
 Predicados:
 
 fcard(N, L)						aridad: 2
 ncards2(N, L)						aridad: 2
 nncards(N, L)						aridad: 2
 mazo(N, L)							aridad: 2 
 limitarmazo(Mazo,M,L)				aridad: 3
 intercambiarelementos(L, LE, V)	aridad: 3
 ie(L1,LE,L)						aridad: 3
 melemento(Mazo,LE,L)				aridad: 3
 mazoelemento(Carta,LE,l)			aridad: 3
 myRandom(Xn,Xn1)					aridad: 2
 myShuffle(Xs, Xn1, L)				aridad: 3
 mazoAleatorio(N, Mazo, MazoAl)		aridad: 3

 Metas Primarias

 ,Metas Secundarias
*/

% fcard: Predicado que crea la primera carta del mazo.
% Dominio: Entero, Carta
fcard(N, L):-
        fcard(N, 1, L).

fcard(N, X,[X]) :-
    X > N.

fcard(N, X, [X|L]):-
        N >= X,
        X1 is X + 1,
        fcard(N, X1, L).

% ncards: Predicado que crea solamente una carta.
% Entero, Entero, Carta
ncards(N,J,L):-
        ncards(N, J, 1, 1, L).

ncards(N, _, K, L, [L]):-
    K > N.

ncards(N, J, K,X,[X|L]):-
        N >= K,
        X1 is (N * J + (K+1)),
        K1 is K + 1,
        ncards(N, J, K1, X1, L).

% ncards2: Predicado que utiliza la funcion ncards para crear las N cartas necesarias.
% Entero , Lista de Cartas
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

% nncards4: Predicado que crea solamente una carta de las NN cartas, utiliza la funcion auxiliar nncards4 de aridad 6.
% Dominio: Entero, Entero, Entero, Entero, Carta.
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

% nncards3: Predicado que realiza el segundo ciclo, creando las N cartas necesarias para el mazo.
% Dominio: Entero, Entero, Entero, Lista de Cartas.
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

% nncards2: Predicado que realiza el tercer ciclo, creando las NN cartas necesarias.
% Dominio: Entero, Entero, Lista de Lista de Cartas
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

% nncards: Predicado que utiliza nncards2 para crear la primera carta e iniciar los ciclos para la creacion de las nnCards.
% Dominio: Entero, Lista de Lista de Cartas.
nncards(N,L):-
    nncards2(N,1,[X|L1]),
    append(L1,X,L).

% mazo: Predicado que une las cartas de los predicados para crear el mazo.
% Dominio: Entero, Lista de Cartas (Mazo).
mazo(N,L):-
    fcard(N, L1),
    ncards2(N,L2),
    append([L1],L2,L3),
    nncards(N,L4),
    append(L3,L4,L).

% limitarmaz: Predicacado utilizado para limitar el mazo correspondiente al Entero M recibido.
% Dominio: Mazo, Entero, Mazo actualizado.
limitarmaz(Mazo,M,L):-
    length(L,M),
    append(L,_,Mazo),
    length(L,Y),
    M >= Y.

% limitarmazo: Predicado para el segundo caso, cuando el M ingresado es una variable este entrega el total de cartas del mazo y el M correspodiente a este total.
% Dominio: Mazo, Entero, Mazo actualizado.
limitarmazo(Mazo,M,L):-
    length(L,M),
    append(L,_,Mazo),
    length(L,Y),
    M = Y.

%[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]

% intercambiarelementos: Predicado que entrega el elemento de la Lista L2 dado el indice I.
% Dominio: Carta, Lista Simbolos, Simbolo
intercambiarelementos([I|_],L2,V):-
    nth1(I,L2,V).

% ie: Predicado que intercambia los elementos de la primera lista de la lista.
% Dominio: Carta, Lista Simbolos, Carta actualizada
ie(L1,LE,L):-
    intercambiarelementos(L1,LE,X),
    ieaux(L1,LE,X,L).

ieaux(L1,_,L,[L]):-
    same_length(L1,[L]), !.

ieaux([_|L1],LE,X,[X|L]):-
    intercambiarelementos(L1,LE,E),
    ieaux(L1,LE,E,L).

% melemento: Predicado que actualiza los elementos de una Lista numerica en una Lista actualizada con una Lista dada.
% Dominio: Mazo, Lista de Simbolos, Mazo actualizado con los simbolos.
melemento([P|M],LE,L):-
    ie(P,LE,X),
    mazoelemento(M,LE,X,L).

mazoelemento(L1,_,L,[L]):-
    length(L1,X),
    length([L],X1),
    X2 is X+1,
    X2 = X1.

% mazoelemento: Predicado que utiliza recursividad para llamar a ie el cual actualiza la carta, por lo tanto mazoelemento actualiza todas las cartas.
% Dominio: Mazo, Lista de Simbolos, Simbolo, Lista de cartas actualizada.
mazoelemento([I|M],LE,X,[X|L]):-
    ie(I,LE,LA),
    mazoelemento(M,LE,LA,L).

% myRandom: Predicado que crea una semilla para su futuro uso en myShuffle.
% Dominio: Entero, Seed(Entero).
myRandom(Xn, Xn1):-
AX is 110 * Xn,
AXC is AX + 123,
Xn1 is (AXC mod 226).

% myShuffle: Predicado que actualiza el orden de las cartas dependiendo del entero Xn1 dado, es una funcion pseudorandomizada que utiliza la semilla de myRandom.
% Dominio: Mazo, Entero, Mazo Actualizado
myshuffle(Xs, Xn1, L) :-
   length(Xs, N),
   H is N - N // 2,
   length(Ys, H),
   append(Ys, LSup, Xs),
   subtract(Xs,LSup,LInf),
   append(LSup,LInf,Lista),
   myshuffleAux(Lista,Xn1,0,L),!.

myshuffleAux(Lista4,Xn1,Cont,Lista4):-
    Cont = Xn1,!.

% Cuando el modulo 2 del contador es 0, corta la lista por la mitad y las cambia de lado (Parte Superior pasa a inferior e inferior a superior).
myshuffleAux(Lista,Xn1,Cont,L):-
    Cont =< Xn1,
    X is Cont mod 2,
    X = 0,
	length(Lista, N),
    H is N - N // 2,
    length(Ys, H),
    append(Ys, LSup, Lista),
    subtract(Lista,LSup,LInf),
    append(LSup,LInf,Lista2),
    ContAux is Cont+1,
    myshuffleAux(Lista2,Xn1,ContAux,L).

% Si el mod no es 2, la mitad de la mitad superior de la lista cambia de lugar hacia el medio de la lista.
myshuffleAux(Lista,Xn1,Cont,L):-
    Cont =< Xn1,
    length(Lista, N),
  	H is N - N // 2,
    length(Ys, H),
    append(Ys, LSup, Lista),
    length(LSup, Y),
  	X is Y - Y // 2,
    length(Bs, X),
    append(Bs, LSup1, LSup),
    subtract(LSup,LSup1,LInf1),
    append(LSup1,LInf1,LSup2),
   subtract(Lista,LSup2, L3),
    append(L3,LSup2,Lista4),
    ContAux is Cont+1,
    myshuffleAux(Lista4,Xn1,ContAux,L).

% MazoAleatorio: Predicado que actualiza el Mazo dada la semilla del Random.
% Dominio: Entero, Mazo, Mazo Actualizado.
mazoAleatorio(N,Mazo, MazoAl):-
    myRandom(N, Seed),
    myshuffle(Mazo,Seed,MazoAl),!.

/*  
%cardsSet [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]
------------------ TDA cardsSet -------------------------

/*
 Dominios:
    CS:                                             CardsSet
 	LE,FirstCard,NextCards,P, M,C,Cartas:		Estructura Lista
	NumE,MaxC,Seed,I:								Entero
    CS_STR,S1,S2,S3,X:								String
 
 Predicados:
 	cardsSet(LE,NumE,MaxC,Seed,CS)					aridad 5
	getFirstCard(CS,FirstCard)						aridad 2
    getNextCards(CS,NextCards)						aridad 2
    cardsSetIsDobble(CS)							aridad 1
    	interseccionCartas(C,CS, V)					aridad 3
        noElementoComun(CS,X)						aridad 2
        mismoTamano(CS,V)							aridad 2
    cardsSetNthCard(CS,I,C)							aridad 3
    cardsSetFindTotalCards(C,I)						aridad 2
    cardsSetMissingCards(Cartas,CS)					aridad 2
    cardsSetToString(CS, CS_STR)					aridad 2
    	cardsSetToString1(CS,CS_STR)				aridad 2
		cardsSetToStringAux(Cartas,1,L,S3,CS_STR)	aridad 5    
*/

% cardsSet: Predicado que crea el cardsSet para su luego uso, En este caso MaxC es una Variable y crea todas las cartas posibles.
% Dominio: Lista de Elementos, NumE(Int), MaxC(Variable), Seed(Entero), CardsSet(Lista de cartas).
cardsSet(LE,NumE,MaxC,Seed,CS):-
    NumEA is NumE-1,
    mazo(NumEA,L),
    length(L,X),
    MaxC = X,
    melemento(L,LE,L2),
    limitarmazo(L2,MaxC,CS1),
    mazoAleatorio(Seed,CS1,CS),
    !.

% cardsSet Caso 2: En este caso MaxC es un Entero y Limita el mazo con el maxC dado.
% Dominio: Lista de Elementos, NumE(Int), MaxC(Int), Seed(Int), CardsSet.
cardsSet(LE,NumE,MaxC,Seed,CS):-
    NumEA is NumE-1,
    mazo(NumEA,L),
    melemento(L,LE,L2),
    limitarmaz(L2,MaxC,Cs1),
    mazoAleatorio(Seed,Cs1,CS),
    !.
    
% ------------------ ---------- SELECTORES  ------------------------------


% getFirstCard: Predicado que entrega la primera carta del CardsSet.
% Dominio: CardsSet, Carta.
getFirstCard([FirstCard|_],FirstCard).

% getNextCards: Predicado que entrega las siguientes cartas del CardsSet.
% Dominio: CardsSet, Cartas.
getNextCards([_|NextCards], NextCards).


% ----------------------------- PERTENENCIA  -----------------------------
%cardsSetIsDoble

% mismoTamano: Predicado que Verifica que todas las cartas tengan el mismo tamano.
% Dominio:  Mazo, Boolean.
mismoTamano([_|[]],_):-
    !.
mismoTamano([P|M], V):-
    nth1(1,M,SC),
    same_length(P,SC),
    mismoTamano(M,V).
    
% noElementoComun: Predicado que verifica que la carta no tenga elementos en comun en ella.
% Dominio: Mazo, Boolean.
noElementoComun([],_).
noElementoComun([P|M],X):-
    is_set(P),
    noElementoComun(M,X).

% interseccionCarta: Predicado que verifica que cada carta tenga 1 elemento en comun con las otras cartas.
% Dominio: Mazo, Boolean.
interseccionCarta([_|[]],_):-
    !.

interseccionCarta([P|M],V):-
    interseccionCartas(P,M,V),
    interseccionCarta(M,V).

interseccionCartas(_,[_|[]],_):-
    !.

% interseccionCarta: Predicado que realiza la recursion de las siguientes cartas, para compararla con la carta entregada al principio.
% Dominio: Carta, Mazo, Boolean
interseccionCartas(C,[P|M], V):-
    intersection(C,P,LI),
    length(LI,1),
    interseccionCartas(C,M,V).

% cardsSetIsDobble: Predicado que Verifica que el cardsSet dado cumple con las reglas del Juego Dobble.
% Dominio: CardsSet.
cardsSetIsDobble(CS):- 
    mismoTamano(CS,_),
    noElementoComun(CS,_),
    interseccionCarta(CS,_).

%------------------------------ OTROS PREDICADOS ------------------------

% cardsSetNthCard: Predicado que entrega la carta numero N del cardsSet respecto al indice indicado.
% Dominio: CardsSet, I(Int), Carta.
cardsSetNthCard(CS,I,C):-
    length(CS,X),
    I < X,
    nth0(I,CS,C),
    !.

% cardsSetFindTotalCards: Predicado que entrega el numero total de cartas que puede tener un mazo dependiendo de la carta entregada.
% Dominio: Carta, Int.
cardsSetFindTotalCards(C,I):-
    length(C,N),
    I is (((N-1)*(N-1))+(N-1)+1),
    !.

% cardsSetMissingCards: Predicado que entrega las cartas faltantes de un CardsSet dado.
% Dominio: Cartas, CardsSet.
cardsSetMissingCards(Cartas,CS):-
    cardsSetIsDobble(Cartas),
    cardsSetNthCard(Cartas,0,C),
    length(C,X),
    cardsSetFindTotalCards(C,TC),
    cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],X,TC,12,CS1),
    subtract(CS1,Cartas,CS).

    
% cardsSetToString1: Predicado que cambia de Lista a string las cartas.
% Dominio: CardsSet, CardsSet en string.
cardsSetToString1(CS,CS_STR):-
    length(CS,L),
    getFirstCard(CS,C),
    atomics_to_string(C, C1),
    getNextCards(CS,Cartas),
    string_concat(" Carta", " 1", S1),
    string_concat("= ", C1, S2),
    string_concat(S1,S2,S3),
    cardsSetToStringAux(Cartas,1,L,S3,CS_STR).

cardsSetToStringAux(_,Cont,L,CS_STR,[CS_STR]):-
	L = Cont,!.

% cardsSetToStringAux: Predicado que cambia una carta a String para su posterior uso en cardsSetToString.
% Dominio: CardsSet, Contandor(Int), Largo del cardset(Int), Carta en string, Cartas en string
cardsSetToStringAux(CS,Cont,L,S,[S|CS_STR]):-
    Cont =< L,
    ContAux is Cont+1,
    getFirstCard(CS,C),
    atomics_to_string(C, C1),
    getNextCards(CS,Cartas),
    string_concat("  \nCarta ", ContAux, S1),
    string_concat("= ", C1, S2),
    string_concat(S1,S2,S3),
    cardsSetToStringAux(Cartas,ContAux,L,S3,CS_STR).

% cardsSetToString: Predicado que Entrega el CardsSet en string.
% Dominio: cardsSet, CardsSet en string.
cardsSetToString(CS, CS_STR):-
    cardsSetToString1(CS,X),
    atomics_to_string(X,L1),
    string_concat(" -----------  MAZO ---------------  ", "\n",S1),
    string_concat(S1,L1 ,S2),
    string_concat(S2, "\n -------- FIN --------", CS_STR).



/*-------------------------- TDA JUGADOR ----------------------------

 Dominios:
    
    Name,NP:                                                    String
    Cards,LT,LP,NextPlayers:                                    Estructura Lista
    Turno, Puntos,I, NMayor, Cont,L, Int, MaxP:                 Entero
    Player, PsA, PlayerOut, FirstPlayer:                        Player

 Predicados:
    
    player(Name, Player)                                   aridad 2
    getPlayerName(Player, Name)                            aridad 2
    getPlayerCards(Player, Cards)                          aridad 2
    getPlayerTurno(Player, Turno)                          aridad 2
    getPlayerPuntos(Player, Puntos)                        aridad 2
    getFirstPlayer(Players, FirstPlayer)                   aridad 2
    getNextPlayers(Players, NextPlayers)                   aridad 2
    actualizarPlayer(Name,Cards,Turno,Puntos,PlayerOut)    aridad 5
    ActualizarPlayers(Players, Player, PsA)                aridad 3
    listTurnos(Players, LT)                                aridad 2
    turnoPlayer(Players,Name, NP)                          aridad 3
    cantNMayor(Players, NMayor, Cont, L, I)                aridad 5
    buscar(Players,Name,Player)                            aridad 3
    listPuntos(Players, LP)                                aridad 3
    esganador(Players,MaxP, Int)                           aridad 3
    buscarganador(Players,MaxP,NP)                         aridad 3
    buscarganadores(Players,MaxP,NP)                       aridad 3
   
 Metas Principales
    player, actualizarPlayer, actualizarPlayers, buscar, buscarganador, buscarganadores.
 Metas Secundarias
    getPlayerName,getPlayerTurno,getPlayerPuntos, getFirstPlayer, getNextPlayers, listTurnos, turnoPlayer,
    cantNMayor, listPuntos, esganador.

*/
%------------------------- REPRESENTACION ------------------------------
/*

 El TDA Player se representa a travez de una Lista, que contiene los elementos Name, Cards, Turno y Puntos, que es un requisito para
 un buen rendimiento del player dentro del juego, las cartas que posee el players, el turno que lo podemos utilizar para saber de que
 player es el turno de juego ,los puntos para saber quien es el ganador y por ultimo el  nombre que es unico para cada jugador.

*/
% ------------------------ CONSTRUCTOR PLAYER --------------------------

% player: Predicado que crea un player valido.
% Dominio: Name, Player.
player(Name,[Name,[],0,0]):-
    string(Name),!.

% ------------------------ SELECTORES ----------------------------------

% getPlayerName: Predicado que obtiene el nombre del Player.
% Dominio: Player, Name.
getPlayerName([Name|_],Name).

% getPlayerCards: Predicado que obtiene las cartas del Player.
% Dominio: Player, Cards.
getPlayerCards([_,Cards,_,_],Cards).

% getPlayerTurno: Predicado que obtiene el turno del Player.
% Dominio: Player, Turno.
getPlayerTurno([_,_,Turno,_],Turno).

% getPlayerPuntos: Predicado que obtiene los Puntos del Player.
% Dominios: Player, Puntos.
getPlayerPuntos([_,_,_,Puntos],Puntos).

% getFirstPlayer: Predicado que obtiene el primer Player de una lista de Jugadores.
% Dominios: Players, Player.
getFirstPlayer([Player|_],Player).

% getNextPlayers: Predicado que obtiene desde el segundo hasta el ultimo Player de una lista de Jugadores.
% Dominios: Players, NextPlayers.
getNextPlayers([_|NextPlayers],NextPlayers).

% -------------------- MODIFICADORES --------------------------------------

% actualizarPlayer: Predicado que actualiza un Player con los datos entregados.
% Dominios: Name, Cards, Turno, Puntos, PlayerOut.
actualizarPlayer(Name,Cards,Turno,Puntos,PlayerOut):-
    PlayerOut = [Name,Cards,Turno,Puntos],!.


% actualizarPlayersAux: Predicado que se utiliza dentro de la funcion actualizarPlayers.
% Caso donde se encuentra al jugador especificado lo actualiza en la lista de jugadores.
% Dominio: Players, Player(Jugador a actualizar), TL (Tamano Lista(Int)), Cont, PsA.
actualizarPlayersAux(Players,Player,TL,Cont,[Player|PsA]):-
    Cont=< TL,
  	getFirstPlayer(Players, FirstPlayer),
    getNextPlayers(Players, NextPlayers),
    getPlayerName(FirstPlayer, NameP),
    getPlayerName(Player, Name),
    Name = NameP,
    ContAux is Cont+1,
    actualizarPlayersAux(NextPlayers,Player,TL,ContAux,PsA).

% Caso donde no se encuentra al jugador especificado, entonces anade al Jugador del momento a la lista PsA sin actualizar.
% Dominio: Players, Player(Jugador a actualizar), TL, Cont, PsA.
actualizarPlayersAux(Players,Player,TL,Cont,[FirstPlayer|PsA]):-
    Cont=< TL,
    getFirstPlayer(Players, FirstPlayer),
    getNextPlayers(Players, NextPlayers),
    ContAux is Cont+1,
    actualizarPlayersAux(NextPlayers,Player,TL,ContAux,PsA).

% Caso Base o de termino del predicado actualizarPlayersAux.
actualizarPlayersAux(_,_,TL,Cont,[]):-
    Cont=TL,!.

% actualizarPlayers: Predicado que actualiza un Players en especifico en una lista de Players.
% Dominio: Players, Player, PsA.
actualizarPlayers(Players,Player,PsA):-
    length(Players,TL),
    actualizarPlayersAux(Players,Player,TL,0,PsA).

% -------------------------- OTROS PREDICADOS ----------------------------


% Caso basde de listTurnos, cuando la lista es vacia retorta la lista con los turnos de los jugadores.
listTurnos([],[]):-!.

% listTurnos: Predicado que entrega una lista con los turnos de todos los jugadores.
% Dominio: Players, LT (Lista de Turnos).
listTurnos(Players, [Turno|LT]):-
    getFirstPlayer(Players, Player),
    getPlayerTurno(Player, Turno),
    getNextPlayers(Players,NextPlayers),
    listTurnos(NextPlayers, LT).

%
%
turnoPlayer([[Name,_,X,_]|_],N,Name):- 
    X<N,!.

turnoPlayer(Players,N,NP):-
    getNextPlayers(Players, NextPlayers),
    turnoPlayer(NextPlayers,N,NP).
    
%
%
cantNMayor([],_,Cont,L,Cont):-
    Cont = L,
    !.

%
%
cantNMayor(Players,NMayor,Cont,L,I):-
    getFirstPlayer(Players,P),
    getNextPlayers(Players, NextPlayers),
    getPlayerTurno(P,T),
    T = NMayor,
    ContAux is Cont+1,
    cantNMayor(NextPlayers, NMayor,ContAux,L, I).

%
%
cantNMayor(Players, NMayor,Cont,L ,I):-
    getNextPlayers(Players, NextPlayers),
    cantNMayor(NextPlayers,NMayor,Cont,L,I).

%
%
buscar([[Name,X,Y,Z]|_],Name,[Name,X,Y,Z]):-!.
buscar(Players,Name,P):-
   getNextPlayers(Players,NextPlayers),
   buscar(NextPlayers,Name,P).

%
%
listPuntos([],[]):-!.
listPuntos(Players, [Puntos|LT]):-
    getFirstPlayer(Players, Player),
    getPlayerPuntos(Player, Puntos),
    getNextPlayers(Players,NextPlayers),
    listPuntos(NextPlayers, LT).

%
esganadorAux(Players,MaxP,Int):-
    esganador(Players,MaxP,Int),!.

%
esganador([],_,0):-!.

%
%
esganador([[_,_,_,X]|NextPlayers],X,Int):-
    esganador(NextPlayers,X,IntAux),
    Int is IntAux+1.
%
%
esganador([_|NextPlayers], MaxP, Int):-
    esganador(NextPlayers,MaxP,Int).

%
%
buscarganador([[NP,_,_,MaxP]|_],MaxP,NP):-!.

%
%
buscarganador([_|NextPlayers],MaxP,N):-
    buscarganador(NextPlayers,MaxP,N).

%
%
buscarganadores([],_,[]):-!.

%
%
buscarganadores([[NP,_,_,MaxP]|NextPlayers],MaxP,[NP|L]):-
    buscarganadores(NextPlayers,MaxP,L).

%
%
buscarganadores([_|NextPlayers],MaxP,L):-
    buscarganadores(NextPlayers,MaxP,L).




/* ------------------------------ TDA GAME --------------------------------



*/


%TDA Game
%
%
%
dobbleGame(NumPlayers,CardsSet,Mode,_,[NumPlayers,CardsSet,Mode,[],[],0,""]):-
    cardsSetIsDobble(CardsSet),
    integer(NumPlayers),!.

%Selectores
getGameNumPlayers([NumPlayers|_],NumPlayers).
getGamecardsSet([_,CardsSet,_,_,_,_,_],CardsSet).
getGameMode([_,_,Mode,_,_,_,_],Mode).
getGamePlayers([_,_,_,Players,_,_,_],Players).
getGameMesa([_,_,_,_,Mesa,_,_],Mesa).
getGameEstado([_,_,_,_,_,Estado,_],Estado).
getGameFin([_,_,_,_,_,_,Fin],Fin).

%Modificadores
actualizarGame(NumPlayers,CardsSet,Mode,Players,Mesa,Estado,Fin,GameOut):-
    GameOut = [NumPlayers,CardsSet,Mode,Players,Mesa,Estado,Fin].

%
 mymember(X,[[X,_,_,_]|_]):-!.
       mymember(X,[_|T]) :- mymember(X,T).

%dobbleGameRegister
dobbleGameRegister(Nombre,GameIn,GameOut):-
    player(Nombre,P),
    getGameNumPlayers(GameIn,NP),
    integer(NP),
    getGamecardsSet(GameIn,CardsSet),
    getGameMode(GameIn,Mode),
    getGamePlayers(GameIn, Players),
    length(Players,TP),
    getGameMesa(GameIn,Mesa),
    getGameEstado(GameIn,Estado),
    getGameFin(GameIn,Fin),
    TP < NP,
    not(mymember(Nombre,Players)),
    actualizarGame(NP,CardsSet,Mode,[P|Players],Mesa,Estado,Fin,GameOut),!.

dobbleGameRegister(Name,GameIn,GameOut):-
    getGameNumPlayers(GameOut,NP),
    integer(NP),
    getGamecardsSet(GameOut,CardsSet),
    getGameMode(GameOut,Mode),
    getGamePlayers(GameOut, Players),
    mymember(Name,Players),
    getGameMesa(GameOut,Mesa),
    getGameEstado(GameOut,Estado),
    getGameFin(GameOut,Fin),
    actualizarGame(NP,CardsSet,Mode,Players,Mesa,Estado,Fin,GameIn),!.

%whoseTurnIsIt

dobbleGameWhoseTurnIsIt(G,NP):-
    getGamePlayers(G,Players),
    listTurnos(Players,LT),
    max_list(LT,NMayor),
    length(Players,X),
    cantNMayor(Players,NMayor,0,X,_),
    getFirstPlayer(Players,P),
    getPlayerName(P,NP),!.

dobbleGameWhoseTurnIsIt(G,NP):-
    getGamePlayers(G,Players),
    listTurnos(Players,LT),
    max_list(LT,NMayor),
    turnoPlayer(Players,NMayor,NP).

%dobbleGamePlay

% ----null---
twoCards(CS,[C,C1]):-
    getFirstCard(CS,C),
    getNextCards(CS,NextCards),
    getFirstCard(NextCards,C1),!.

getCartasMesa(Mesa,Mesa).

% Se quitan 2 cartas de la baraja para dejarlas en Mesa, ningun jugador utiliza turno.
dobbleGamePlay(Game,Action,GameOut):-
    getGameMode(Game,Mode),
    Mode = "Stack",
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = null,
    getGameMesa(Game,Mesa),
    length(Mesa, 0),
    getGamecardsSet(Game,CardsSet),
    twoCards(CardsSet,C2),
    subtract(CardsSet,C2,CSGame),
    getGameNumPlayers(Game,NP),
    getGameMode(Game,Mode),
    getGamePlayers(Game, Players),
    getGameEstado(Game,Estado),
    getGameFin(Game,Fin),
    actualizarGame(NP,CSGame,Mode,Players,C2,Estado,Fin,GameOut),!.
    
% El jugador que posee el turno realiza la accion Pass, utilizando si turno y la baraja del juego
% se le anaden las cartas de la mesa
dobbleGamePlay(Game,Action,GameOut):-
    getGameMode(Game,Mode),
    Mode = "Stack",
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = pass,
    dobbleGameWhoseTurnIsIt(Game,NP),
    getGamePlayers(Game,Players),
 	buscar(Players,NP,P),
    getPlayerName(P,Name),
    getPlayerCards(P,Cards),
    getPlayerTurno(P,Turno),
    TurnoAux is Turno+1,
    getPlayerPuntos(P,Puntos),
    actualizarPlayer(Name,Cards,TurnoAux,Puntos,PActualizado),
    actualizarPlayers(Players,PActualizado,PsActualizado),
	%reverse(PA1, [_|PA2]),
    %reverse(PA2, PsActualizado),
    getGameNumPlayers(Game,NumP),
    getGamecardsSet(Game,CardsSet),
    getGameMode(Game,Mode),
    getGameMesa(Game,Mesa),
    getGameEstado(Game,Estado),
    getGameFin(Game,Fin),
    append(CardsSet,Mesa,CSActualizado),
    actualizarGame(NumP,CSActualizado,Mode,PsActualizado,[],Estado,Fin,GameOut),!.

% Termina el Juego, cambiando el estado del Juego a 1, y crea el mensaje del Ganador
dobbleGamePlay(Game,Action, GameOut):-
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = finish,
    getGamePlayers(Game,Players),
    listPuntos(Players,LPuntos),
    max_list(LPuntos, MaxPunto),
    esganadorAux(Players,MaxPunto,I),
    I = 1,
    buscarganador(Players,MaxPunto,Name),
    getGameNumPlayers(Game,NumP),
    getGamecardsSet(Game,CardsSet),
    getGameMesa(Game,Mesa),
    getGameMode(Game,Mode),
    string_concat("El Ganador es = ", Name, Fin),
    actualizarGame(NumP,CardsSet,Mode,Players,Mesa,1,Fin,GameOut),!.

% Termina el Juego, cambiando el estado del Juego a 1, y crea el mensaje de los Empatados
dobbleGamePlay(Game,Action, GameOut):-
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = finish,
    getGamePlayers(Game,Players),
    listPuntos(Players,LPuntos),
    max_list(LPuntos, MaxPunto),
    buscarganadores(Players,MaxPunto,LNames),
    getGameNumPlayers(Game,NumP),
    getGamecardsSet(Game,CardsSet),
    getGameMode(Game,Mode),
    getGameMesa(Game,Mesa),
    atomics_to_string(LNames,Names),
    string_concat("Hay Empate entre = ", Names, Fin),
    actualizarGame(NumP,CardsSet,Mode,Players,Mesa,1,Fin,GameOut),!.

% Caso donde Mesa si tiene cartas, y 
% Jugador acierta en decir el elemento
dobbleGamePlay(Game, [Action,Name,Element], GameOut):-
    getGameMode(Game,Mode),
    Mode = "Stack",
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = spotIt,
    dobbleGameWhoseTurnIsIt(Game,N),
    Name = N,
    getGameMesa(Game,Mesa),
    not(length(Mesa,0)),
    getCartasMesa(Mesa,[C1,C2]),
    intersection(C1,C2, [Elem]),
    Elem = Element,
    getGamePlayers(Game,Players),
    buscar(Players, Name, Player),
    %getPlayerCards(Player, Cards),
    getPlayerTurno(Player, Turno),
    getPlayerPuntos(Player, Puntos),
	TurnoAux is Turno+1,
    PuntosAux is Puntos+1,
    actualizarPlayer(Name,Mesa,TurnoAux,PuntosAux,PlayerActualizado),
    actualizarPlayers(Players,PlayerActualizado,PlayersActualizado),
    getGameNumPlayers(Game, NumPlayers),
    getGamecardsSet(Game, CardsSet),
    getGameEstado(Game, Estado),
    getGameFin(Game, Fin),
    actualizarGame(NumPlayers,CardsSet,Mode,PlayersActualizado,[],Estado,Fin,GameOut),!.

% Caso donde Mesa si tiene cartas, y
% Jugador falla en decir el elemento
dobbleGamePlay(Game, [Action,Name,_], GameOut):-
    getGameMode(Game,Mode),
    Mode = "Stack",
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = spotIt,
    dobbleGameWhoseTurnIsIt(Game,N),
    Name = N,
    getGameMesa(Game,Mesa),
    not(length(Mesa,0)),
    getCartasMesa(Mesa,[C1,C2]),
    intersection(C1,C2, [_]),
    getGamePlayers(Game,Players),
    buscar(Players, Name, Player),
    getPlayerCards(Player, Cards),
    getPlayerTurno(Player, Turno),
    getPlayerPuntos(Player, Puntos),
	TurnoAux is Turno+1,
    actualizarPlayer(Name,Cards,TurnoAux,Puntos,PlayerActualizado),
    actualizarPlayers(Players,PlayerActualizado,PlayersActualizado),
    getGameNumPlayers(Game, NumPlayers),
    getGamecardsSet(Game, CardsSet),
    getGameEstado(Game, Estado),
    getGameFin(Game, Fin),
    actualizarGame(NumPlayers,CardsSet,Mode,PlayersActualizado,Mesa,Estado,Fin,GameOut),!.

% Si El juego esta Finalizado el Juego queda igual.
dobbleGamePlay(Game, _, Game):-
    getGameEstado(Game, Estado),
    Estado = 1,!.

%GameStatus
dobbleGameStatus(Game,Str):-
    getGameEstado(Game,Estado),
    Estado = 0,
    string_concat("\nEl Estado de Juego es :", " En Partida", Str),!.
dobbleGameStatus(Game,Str):-
    getGameEstado(Game,Estado),
    Estado = 1,
    string_concat("\nEl Estado de Juego es :", " Finalizado", Str),!.

%GameScore

dobbleGameScore(Game,Name, Score):-
    getGamePlayers(Game,Players),
    buscar(Players,Name, Player),
	getPlayerPuntos(Player, Score),!. 