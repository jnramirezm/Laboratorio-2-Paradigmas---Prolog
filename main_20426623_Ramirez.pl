/*------------------------------------------------------
         _          _      ____      ____          
 ----   | |        / \    | __ )    |___ \    -------  
 ----   | |       / _ \   |  _ \      __) |   -------
 ----   | |___   / ___ \  | |_) |    / __/    -------
 ----   |_____| /_/   \_\ |____/    |_____|   -------

--------------------------------------------------------

 Nombre: Juan Ramirez Montero
 Profesor de seccion: Roberto Gonzalez I.
 Seccion: A-1
 Rut: 20.426.623-9
 
 ----------------- TDA CARDSSET Constructor ------------
 
 Dominios:
 N, J, K, X, MaxC, Xn,Xn1, FirstSimbol:	 				Enteros
 L, L1, Mazo, Carta, LE, Xs, MazoL: 	                Estructura Lista
 Element, Elemento: 					                Atomic
 RestMazo, CardsActualizada,MazoActualizado,M:          Mazo
 CardActualizada,NextSimbols,CardA:                     Carta

 Predicados:
 
 fcard(N, L)						                    aridad: 2
 ncards2(N, L)						                    aridad: 2
 nncards(N, L)						                    aridad: 2
 mazo(N, L)							                    aridad: 2 
 limitarmazo(Mazo,MaxC,L)			                    aridad: 3
 intercambiarelementos(FirstSimbol, LE, Element)	    aridad: 3
 ie(Card,LE,CardActualizada)						    aridad: 3
 melemento(Mazo,LE,MazoActualizado)				        aridad: 3
 mazoelemento(RestMazo,LE,CardsActulizada)		    	aridad: 3
 myRandom(Xn,Xn1)					                    aridad: 2
 myShuffle(Xs, Xn1, L)				                    aridad: 3
 mazoAleatorio(N, Mazo, MazoAl)		                    aridad: 3

 Metas Primarias
 mazo, melemento, mazoAleatorio, limitarmazo

 Metas Secundarias
 fcard,ncards2,nncards, intercambiarelementos,ie,myRandom,myShuffle.

 --------------------------------------------------------------

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
% Dominio: Entero, Entero, Carta
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
% Dominio: Entero , Lista de Cartas
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
limitarmaz(Mazo,MaxC,L):-
    length(L,MaxC),
    length(Mazo, LM),
    LM > MaxC,
    append(L,_,Mazo),
    length(L,Y),
    MaxC >= Y.

% Caso cuando MaxC es mayor que el total de cartas.
limitarmaz(Mazo,MaxC,L):-
    length(Mazo, LM),
    length(L,LM),
    append(L,_,Mazo),
    length(L,Y),
    MaxC >= Y.

% limitarmazo: Predicado para el segundo caso, cuando el M ingresado es una variable este entrega el total de cartas del mazo y el M correspodiente a este total.
% Dominio: Mazo, Entero, Mazo actualizado.
limitarmazo(Mazo,MaxC,L):-
    length(L,MaxC),
    append(L,_,Mazo),
    length(L,Y),
    MaxC = Y.

%[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]

% intercambiarelementos: Predicado que entrega el elemento de la Lista L2 dado el indice I.
% Dominio: Carta, Lista Simbolos, Simbolo
intercambiarelementos([FirstSimbol|_],LE,Element):-
    nth1(FirstSimbol,LE,Element).

% ie: Predicado que intercambia los elementos de la primera lista de la lista.
% Dominio: Carta, Lista Simbolos, Carta actualizada
ie(Card,LE,CardActualizada):-
    intercambiarelementos(Card,LE,Elemento),
    ieaux(Card,LE,Elemento,CardActualizada).

ieaux(NextSimbols,_,CardActualizada,[CardActualizada]):-
    same_length(NextSimbols,[CardActualizada]), !.

ieaux([_|NextSimbols],LE,Elemento,[Elemento|CardActualizada]):-
    intercambiarelementos(NextSimbols,LE,Element),
    ieaux(NextSimbols,LE,Element,CardActualizada).

% melemento: Predicado que actualiza los elementos de una Lista numerica en una Lista actualizada con una Lista dada.
% Dominio: Mazo, Lista de Simbolos, Mazo actualizado con los simbolos.
melemento([FCard|M],LE,MazoActualizado):-
    ie(FCard,LE,CardActualizada),
    mazoelemento(M,LE,CardActualizada,MazoActualizado).

mazoelemento(RestMazo,_,CardsActualizada,[CardsActualizada]):-
    length(RestMazo,X),
    length([CardsActualizada],X1),
    X2 is X+1,
    X2 = X1.

% mazoelemento: Predicado que utiliza recursividad para llamar a ie el cual actualiza la carta, por lo tanto mazoelemento actualiza todas las cartas.
% Dominio: Mazo, Lista de Simbolos, Simbolo, Lista de cartas actualizada.
mazoelemento([Card|RestMazo],LE,CardActualizada,[CardActualizada|CardsActualizada]):-
    ie(Card,LE,CardA),
    mazoelemento(RestMazo,LE,CardA,CardsActualizada).

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

 
%cardsSet [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]

% -------------------------------------- TDA CARDSSET ------------------------------------------

/*
 Dominios:
    CS:                                             CardsSet
 	LE,FirstCard,NextCards,P, M,C,Cartas:		    Estructura Lista
	NumE,MaxC,Seed,I,TotalCards:					Entero
    CS_STR,S1,S2,S3,X:								String
 
 Predicados:
 	cardsSet(LE,NumE,MaxC,Seed,CS)					aridad 5
	getFirstCard(CS,FirstCard)						aridad 2
    getNextCards(CS,NextCards)						aridad 2
    cardsSetIsDobble(CS)							aridad 1
    	interseccionCartas(C,CS, V)					aridad 3
        noElementoComun(CS,V)						aridad 2
        mismoTamano(CS,V)							aridad 2
    cardsSetNthCard(CS,I,C)							aridad 3
    cardsSetFindTotalCards(C,I)						aridad 2
    cardsSetMissingCards(Cartas,CS)					aridad 2
    cardsSetToString(CS, CS_STR)					aridad 2
    	cardsSetToString1(CS,CS_STR)				aridad 2
		cardsSetToStringAux(Cartas,1,L,S3,CS_STR)	aridad 5  

 Metas Primarias:
    cardsSet, cardsSetIsDobble, cardsSetNthCard, cardsSetFindTotalCards, cardsSetMissingCards, cardsSetToString.
 Metas Secundarias:
    getFirstCard, getNextCardsm interseccionCartas, noElementoComun, mismoTamano, cardsSetToString1, cardsSetToStringAux.

 ------------------------------------------------------------------------------------------------

*/

% cardsSet Caso 2: En este caso MaxC es un Entero y Limita el mazo con el maxC dado.
% Dominio: Lista de Elementos, NumE(Int), MaxC(Int), Seed(Int), CardsSet.
cardsSet(LE,NumE,MaxC,Seed,CS):-
    integer(Seed),
    integer(MaxC),
    NumEA is NumE-1,
    mazo(NumEA,L),
    melemento(L,LE,L2),
    limitarmaz(L2,MaxC,Cs1),
    mazoAleatorio(Seed,Cs1,CS),
    !.

% cardsSet: Predicado que crea el cardsSet para su luego uso, En este caso MaxC es una Variable y crea todas las cartas posibles.
% Dominio: Lista de Elementos, NumE(Int), MaxC(Variable), Seed(Entero), CardsSet(Lista de cartas).
cardsSet(LE,NumE,MaxC,Seed,CS):-
    integer(Seed),
    NumEA is NumE-1,
    mazo(NumEA,L),
    length(L,X),
    MaxC = X,
    melemento(L,LE,L2),
    limitarmazo(L2,MaxC,CS1),
    mazoAleatorio(Seed,CS1,CS),
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
noElementoComun([P|M],V):-
    is_set(P),
    noElementoComun(M,V).

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
    cardsSetIsDobble(CS),
    integer(I),
    length(CS,X),
    I < X,
    nth0(I,CS,C),
    !.

% cardsSetFindTotalCards: Predicado que entrega el numero total de cartas que puede tener un mazo dependiendo de la carta entregada.
% Dominio: Carta, Int.
cardsSetFindTotalCards(C,TotalCards):-
    is_list(C),
    not(length(C,0)),
    length(C,N),
    TotalCards is (((N-1)*(N-1))+(N-1)+1),
    !.

% cardsSetMissingCards: Predicado que entrega las cartas faltantes de un CardsSet dado.
% Dominio: Cartas, CardsSet.
cardsSetMissingCards(Cartas,CS):-
    cardsSetIsDobble(Cartas),
    cardsSetNthCard(Cartas,0,C),
    length(C,X),
    nth1(1,C,Element),
    member(Element,[a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz]),
    cardsSetFindTotalCards(C,TC),
    cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],X,TC,12,CS1),
    cardsSetIsDobble(CS1),
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
    cardsSetIsDobble(CS),
    cardsSetToString1(CS,X),
    atomics_to_string(X,L1),
    string_concat(" -----------  MAZO ---------------  ", "\n",S1),
    string_concat(S1,L1 ,S2),
    string_concat(S2, "\n -------- FIN --------", CS_STR).



/*-------------------------- TDA JUGADOR ----------------------------

 Dominios:
    
    Name,NP,PStr:                                                    String
    Cards,LT,LP,NextPlayers:                                         Estructura Lista
    Turno, Puntos,I, NMayor, Cont,L, Int, MaxP,TL:                   Entero
    Player, PsA, PlayerOut, FirstPlayer:                             Player

 Predicados:
    
    player(Name, Player)                                   aridad 2
    getPlayerName(Player, Name)                            aridad 2
    getPlayerCards(Player, Cards)                          aridad 2
    getPlayerTurno(Player, Turno)                          aridad 2
    getPlayerPuntos(Player, Puntos)                        aridad 2
    getFirstPlayer(Players, FirstPlayer)                   aridad 2
    getNextPlayers(Players, NextPlayers)                   aridad 2
    actualizarPlayer(Name,Cards,Turno,Puntos,PlayerOut)    aridad 5
    actualizarPlayers(Players, Player, PsA)                aridad 3
        actualizarPlayersAux(Players,Player,TL,Cont,PsA)   aridad 5   
    listTurnos(Players, LT)                                aridad 2
    turnoPlayer(Players,Name, NP)                          aridad 3
    cantNMayor(Players, NMayor, Cont, L, I)                aridad 5
    buscar(Players,Name,Player)                            aridad 3
    listPuntos(Players, LP)                                aridad 3
    esganador(Players,MaxP, Int)                           aridad 3
        esganadorAux(Players,MaxP,Int)                     aridad 3
    buscarganador(Players,MaxP,NP)                         aridad 3
    buscarganadores(Players,MaxP,NP)                       aridad 3
    playersToString(Players, PStr)                         aridad 2
        playersToStringAux(Players,Player,L, Cont, PStr)   aridad 5
   
 Metas Principales

    player, actualizarPlayer, actualizarPlayers, turnoPlayer, buscar, buscarganador, buscarganadoresm playersToString.

 Metas Secundarias

    getPlayerName,getPlayerTurno,getPlayerPuntos, getFirstPlayer, getNextPlayers, listTurnos,
    cantNMayor, listPuntos, esganador, playersToStringAux, esganadorAux.

------------------------------------------------------------------------
*/
%------------------------- REPRESENTACION ------------------------------
/*

 El TDA Player se representa a travez de una Lista, que contiene los elementos Name, Cards, Turno y Puntos, que es un requisito para
 un buen rendimiento del player dentro del juego. Podemos observar que player tiene las cartas que posee s, el turno que lo podemos utilizar 
 para saber de que player es el turno de juego ,los puntos para saber quien es el ganador y por ultimo el nombre que es unico 
 para cada jugador.

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


% Caso base de listTurnos, cuando la lista es vacia retorta la lista con los turnos de los jugadores.
listTurnos([],[]):-!.

% listTurnos: Predicado que entrega una lista con los turnos de todos los jugadores.
% Dominio: Players, LT (Lista de Turnos).
listTurnos(Players, [Turno|LT]):-
    getFirstPlayer(Players, Player),
    getPlayerTurno(Player, Turno),
    getNextPlayers(Players,NextPlayers),
    listTurnos(NextPlayers, LT).

% Caso base del predicado turnoPlayer, al cumplirse la condicion obtiene el nombre del Player correspondiente.
turnoPlayer([[Name,_,X,_]|_],N,Name):- 
    X<N,!.

% turnoPlayer: Predicado que obtiene el nombre del player al que corresponde el turno, lo hace a traves de un N que es el turno mayor,
% por lo tanto al que corresponda el turno debe  de tener un turnos menor al de ese N.
% Dominio: Players , Entero, NP (Nombre Player).
turnoPlayer(Players,N,NP):-
    getNextPlayers(Players, NextPlayers),
    turnoPlayer(NextPlayers,N,NP).
    
% Caso base del predicado cantNMayor, al cumplirse la condicion devuelve un Int.
cantNMayor([],_,Cont,L,Cont):-
    Cont = L,
    !.

% cantNMayor: Predicado que cuenta cuantos Players tienen el mismo turno, se utiliza para saber cuando todos los Players 
%tienen el  mismo turno, asi empieza desde el primero nuevamente.
% Dominio: Players, NMayor(Int), Cont(Int), L(Tamano Lista Players), I(Int).
cantNMayor(Players,NMayor,Cont,L,I):-
    getFirstPlayer(Players,P),
    getNextPlayers(Players, NextPlayers),
    getPlayerTurno(P,T),
    T = NMayor,
    ContAux is Cont+1,
    cantNMayor(NextPlayers, NMayor,ContAux,L, I).

% Caso donde el player no tiene el mismo turno, entonces no suma el contador.
cantNMayor(Players, NMayor,Cont,L ,I):-
    getNextPlayers(Players, NextPlayers),
    cantNMayor(NextPlayers,NMayor,Cont,L,I).


% Caso base donde si el nombre corresponde al del player obtiene el player.
buscar([[Name,X,Y,Z]|_],Name,[Name,X,Y,Z]):-!.

% buscar: Predicado que obtiene al Player correspondiente al nombre dado.
% Dominio: Players, Name, Player.
buscar(Players,Name,P):-
   getNextPlayers(Players,NextPlayers),
   buscar(NextPlayers,Name,P).

% Caso basde del predicado ListPuntos.
listPuntos([],[]):-!.

% listPuntos: Predicado que crea una lista con todos los Puntos de los jugadores
% Dominio: Players, LT(Lista Puntos).
listPuntos(Players, [Puntos|LP]):-
    getFirstPlayer(Players, Player),
    getPlayerPuntos(Player, Puntos),
    getNextPlayers(Players,NextPlayers),
    listPuntos(NextPlayers, LP).

% esganadorAux: Predicado que utiliza la funcion esganador, pero solo obtiene el primer Jugador (Esto ya que el predicado es ganador
% entregaba mas de 1 Int (Al ser una recursion de arbol empezaba a entregar los proximos resultados) y ocurrian problemas con el predicado finish ) 
% Dominio: Players, MaxP(Max Puntos), Int.
esganadorAux(Players,MaxP,Int):-
    esganador(Players,MaxP,Int),!.

% Caso base de esganador.
esganador([],_,0):-!.

% esganador: Predicado que obtiene el numero de personas que tienen el mayor puntaje, si el entero obtenido es mayor a 1, quiere decir que hubo empate.
% Caso: en este caso si los puntos son iguales al MaxP, entonces se le suma 1 al contador.
% Dominio: Players, MaxP(Int), Int.
esganador([[_,_,_,X]|NextPlayers],X,Int):-
    esganador(NextPlayers,X,IntAux),
    Int is IntAux+1.

% Caso: Si los puntos no son iguales, entonces pasa de jugador sin sumar al contador.
esganador([_|NextPlayers], MaxP, Int):-
    esganador(NextPlayers,MaxP,Int).

% Caso Base de buscar ganador, si encuentra que el MaxP es igual al puntaje del jugador, devuelve el nombre de este.
buscarganador([[NP,_,_,MaxP]|_],MaxP,NP):-!.

% buscarganador: Predicado que obtiene el nombre del jugador con Mayor Puntaje.
% Dominio: Players, MaxP, Name.
buscarganador([_|NextPlayers],MaxP,N):-
    buscarganador(NextPlayers,MaxP,N).

% Caso Base de buscar ganadores, cuando la lista de Players es vacia, termina la recursion.
buscarganadores([],_,[]):-!.

% buscarganadores: Predicado que obtiene una Lista con los nombres de los ganadores/empatados para su luego uso.
% Caso: Cuando encuentra al user con el mismo puntaje MaxP lo agrega a la Lista.
% Dominio: Players, MaxP, Lista de Nombres.
buscarganadores([[NP,_,_,MaxP]|NextPlayers],MaxP,[NP|L]):-
    buscarganadores(NextPlayers,MaxP,L).

% Caso: Si no tiene el mismo puntaje entonces pasa al siguiente usuario, sin agregarlo a la Lista.
buscarganadores([_|NextPlayers],MaxP,L):-
    buscarganadores(NextPlayers,MaxP,L).


% playersToString: Predicado que cambia la representacion de Lista de los jugadores a un String esto por cada jugador y se empieza la recursion.
% Caso: Si el primer jugador no tiene cartas a su disposicion se utiliza este caso.
% Dominio: Players, PStr.
playersToString(Players,PStr):-
    length(Players, L),
    getNextPlayers(Players,NextPlayers),
    getFirstPlayer(Players,FirstPlayer),
    getPlayerName(FirstPlayer, Name),
    getPlayerCards(FirstPlayer, Cards),
    getPlayerTurno(FirstPlayer, Turno),
    getPlayerPuntos(FirstPlayer, Puntos),
    length(Cards,0),
    atomics_to_string(Cards, CardsStr),
    atomics_to_string([Turno], TurnoStr),
    atomics_to_string([Puntos], PuntosStr),
    string_concat(" \n Nombre Jugador: ", Name, S1),
    string_concat(S1, "  / Cartas Jugador : ", S2),
    string_concat(S2, CardsStr, S3),
    string_concat(S3, "  / Turnos Jugador : ", S4),
    string_concat(S4 , TurnoStr, S5 ),
    string_concat(S5, "  / Puntos Jugador : ", S6),
    string_concat(S6,  PuntosStr, PlayerStr),
    playersToStringAux(NextPlayers,PlayerStr,L,1,PStr).

% Caso: Si el jugador tiene cartas a su disposicion entonces llamamos a cardsSetToString para pasar las cartas a string.
 playersToString(Players,PStr):-
    length(Players, L),
    getNextPlayers(Players,NextPlayers),
    getFirstPlayer(Players,FirstPlayer),
    getPlayerName(FirstPlayer, Name),
    getPlayerCards(FirstPlayer, Cards),
    getPlayerTurno(FirstPlayer, Turno),
    getPlayerPuntos(FirstPlayer, Puntos),
    not(length(Cards,0)),
    cardsSetToString(Cards, CardsStr),
    atomics_to_string([Turno], TurnoStr),
    atomics_to_string([Puntos], PuntosStr),
    string_concat("\n Nombre Jugador: ", Name, S1),
    string_concat(S1, "  / Cartas Jugador : ", S2),
    string_concat(S2, CardsStr, S3),
    string_concat(S3, "  / Turnos Jugador : ", S4),
    string_concat(S4 , TurnoStr, S5 ),
    string_concat(S5, "  / Puntos Jugador : ", S6),
    string_concat(S6,  PuntosStr, PlayerStr),
    playersToStringAux(NextPlayers,PlayerStr,L,1,PStr).

% Caso Base de playersToStringAux, si el tamano de lista de Players es igual al contador temrina la recursion y entrega los players en string.
playersToStringAux(_,PStr, L, Cont,[PStr]):-
    L = Cont,!.

% Caso cuando el player no tiene cartas a disposicion entonces no se llama a cardsSet,
% Dominio: Players, Players, L, Cont, PStr.
playersToStringAux(Players,Player,L,Cont,[Player|PStr]):-
    Cont =< L,
    ContAux is Cont+1,
    getNextPlayers(Players,NextPlayers),
    getFirstPlayer(Players,FirstPlayer),
    getPlayerName(FirstPlayer, Name),
    getPlayerCards(FirstPlayer, Cards),
    getPlayerTurno(FirstPlayer, Turno),
    getPlayerPuntos(FirstPlayer, Puntos),
    length(Cards,0),
    atomics_to_string(Cards, CardsStr),
    atomics_to_string([Turno], TurnoStr),
    atomics_to_string([Puntos], PuntosStr),
    string_concat("\n Nombre Jugador: ", Name, S1),
    string_concat(S1, "  / Cartas Jugador : ", S2),
    string_concat(S2, CardsStr, S3),
    string_concat(S3, "  / Turnos Jugador : ", S4),
    string_concat(S4 , TurnoStr, S5 ),
    string_concat(S5, "  / Puntos Jugador : ", S6),
    string_concat(S6,  PuntosStr, PlayerStr),
    playersToStringAux(NextPlayers,PlayerStr,L,ContAux,PStr).

% Caso cuando el player si tiene cartas a disposicion.
playersToStringAux(Players,Player,L,Cont,[Player|PStr]):-
    Cont =< L,
    ContAux is Cont+1,
    getNextPlayers(Players,NextPlayers),
    getFirstPlayer(Players,FirstPlayer),
    getPlayerName(FirstPlayer, Name),
    getPlayerCards(FirstPlayer, Cards),
    getPlayerTurno(FirstPlayer, Turno),
    getPlayerPuntos(FirstPlayer, Puntos),
    not(length(Cards,0)),
    cardsSetToString(Cards, CardsStr),
    atomics_to_string([Turno], TurnoStr),
    atomics_to_string([Puntos], PuntosStr),
    string_concat("\n Nombre Jugador: ", Name, S1),
    string_concat(S1, "  / Cartas Jugador : ", S2),
    string_concat(S2, CardsStr, S3),
    string_concat(S3, "  / Turnos Jugador : ", S4),
    string_concat(S4 , TurnoStr, S5 ),
    string_concat(S5, "  / Puntos Jugador : ", S6),
    string_concat(S6,  PuntosStr, PlayerStr),
    playersToStringAux(NextPlayers,PlayerStr,L,ContAux,PStr).



/* ------------------------------ TDA GAME --------------------------------

 Dominios:

    cardsSet,CS:                                CardsSet
    Mesa, game, Cards:                          Estructura Lista
    Players:                                    Players
    NumPlayers, Estado,Score:                   Entero
    Mode,Fin,Nombre,NP,Action,Name,Str:         String
    GameIn,GameOut,Game:                        Game
    Element:                                    atomic

 Predicados:
    
    dobbleGame(NumPlayers,CardsSet,Mode,Seed,Game)                                     aridad 5
    getGameNumPlayers(Game, NumPlayers)                                                aridad 2
    getGamecardsSet(Game, CardsSet)                                                    aridad 2
    getGameMode(Game, Mode)                                                            aridad 2
    getGanePlayers(Game, Players)                                                      aridad 2
    getGameMesa(Game, Mesa)                                                            aridad 2
    getCartasMesa(Mesa, Cards)                                                         aridad 2
    getGameEstadp(Game, Estado)                                                        aridad 2
    getGameFin(Game, Fin)                                                              aridad 2
    actualizarGame(NumPlayers,CardsSet,Mode,Players,Mesa,Estado,Fin,GameOut)           aridad 8
    mymember(Nombre,Players)                                                           aridad 2
    dobbleGameRegister(Nombre,GameIn,GameOut)                                          aridad 3
    dobbleGameWhoseTurnIsIt(Game, NP)                                                  aridad 2
    twoCards(CS,Cards)                                                                 aridad 2
    dobbleGamePlay(GameIn,Action,GameOut)                                              aridad 3
    dobbleGamePlay(GameIn,[Action,Name,Element],GameOut)                               aridad 3
    dobbleGameStatus(Game, Str)                                                        aridad 2
    dobbleGameScore(Game, Name, Score)                                                 aridad 3
    dobbleGameToString(Game, Str)                                                      aridad 2

 Metas Principales
    dobbleGame, dobbleGameRegister, dobbleGameWhoseTurnIsIt, dobbleGamePlay, dobbleGameStatus, dobbleGameScore, dobbleGameToString

 Metas Secundarias
    getGameNumPlayers, getGamecardsSet, getGameMode, getGamePlayers, getGameMesa, getCartasMesa, getGameEstado, getGameFin,
    actualizarGame,mymember, twoCards.

----------------------------------------------------------------------------------------
*/

% ---------------------------- REPRESENTACION GAME -------------------------------------
/*

    El TDA Game se representa a traves de una estructura Lista, que contendra un entero que representara la cantidad de players
    que podran registrarse al juego, un cardsSet con el que se jugara, un Mode es un string que tiene el tipo de modo de juego con el que jugaran los
    jugadores, tendra una Lista con los jugadores (Los jugadores tienen su propia representacion), una Mesa que sera las cartas
    que veran los jugadores al jugar, el estado que es un entero 0 y 1, que representa el estado del juego si esta en resumen o finalizado,
    y por ultimo el Fin, que es un string que contendra el nombre del ganador o los empatados al terminar el juego.

*/

% ---------------------------- CONSTRUCTOR GAME ----------------------------------------

% dobbleGame: Predicado que crea el game con los correspondientes argumentos dados y que se usara durante todo el TDA GAME.
% Dominio: NumPlayers, cardsSet, Mode, Seed, Game.
dobbleGame(NumPlayers,CardsSet,Mode,_,[NumPlayers,CardsSet,Mode,[],[],0,""]):-
    cardsSetIsDobble(CardsSet),
    integer(NumPlayers),
    string(Mode),!.

% ----------------------------  SELECTORES ---------------------------------------------

% getGameNumPlayers: Predicado que obtiene el entero NumPlayers del game.
% Domninio: Game, NumPlayers.
getGameNumPlayers([NumPlayers|_],NumPlayers).

% getGamecardsSet: Predicado que obtiene el cardsSet del game.
% Dominio: Game, CardsSet.
getGamecardsSet([_,CardsSet,_,_,_,_,_],CardsSet).

% getGameMode: Predicado que obtiene el string Mode del game.
% Dominio: Game, Mode.
getGameMode([_,_,Mode,_,_,_,_],Mode).

% getGamePlayers: Predicado que obtiene la Lista de jugadores del game.
% Dominio: Game, Players.
getGamePlayers([_,_,_,Players,_,_,_],Players).

% getGameMesa: Predicado que obtiene las Mesa del game.
% Dominio: Game, Mesa.
getGameMesa([_,_,_,_,Mesa,_,_],Mesa).

% getGameEstado: Predicado que obtiene el Int Estado del game.
% Dominio: Game, Estado.
getGameEstado([_,_,_,_,_,Estado,_],Estado).

% getGameFin: Predicado que obtiene el string de Fin del game.
% Dominio: Game, Fin.
getGameFin([_,_,_,_,_,_,Fin],Fin).

%---------------------------------- MODIFICADORES -------------------------------------------

% actualizarGame: Predicado que actualiza un Game como los argumentos dados.
% Dominio: NumPlayers, CardsSet, Mode, Players, Mesa, Estado, Fin, GameOut.
actualizarGame(NumPlayers,CardsSet,Mode,Players,Mesa,Estado,Fin,GameOut):-
    GameOut = [NumPlayers,CardsSet,Mode,Players,Mesa,Estado,Fin].

% --------------------------------- OTROS PREDICADOS -----------------------------------------

% mymember: Predicado que retorna True si encuentra el Nombre en la lIsta de Players.
% Dominio: Name, Players.
 mymember(X,[[X,_,_,_]|_]):-!.
       mymember(X,[_|T]) :- mymember(X,T).

% --------------------------------  dobbleGameRegister ------------------------------

% dobbleGameRegister: Predicado que anade a un Player a una Lista de la Players si este no existe en la lista.
% Caso: Se entrega un Game por GameIn entonces registra al Player si este aun no esta registrado y aun no sobrepasa el total de registros.
% Dominio: Nombre, GameIn, GameOut.
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

% Caso cuando se entrega una variable por GameIn.
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

% ------------------------- whoseTurnIsIt ----------------------------------

% dobbleGameWhoseTurnIsIt: Predicado que obtiene el nombre del Player al que le corresponde el Turno.
% Dominio: Game, NP(Nombre Jugador).
dobbleGameWhoseTurnIsIt(G,NP):-
    getGamePlayers(G,Players),
    listTurnos(Players,LT),
    max_list(LT,NMayor),
    length(Players,X),
    cantNMayor(Players,NMayor,0,X,_),
    getFirstPlayer(Players,P),
    getPlayerName(P,NP),!.

% Caso cuando todos los Players tienen la misma cantidad de turnos hechos, entonces entrega el primer usuario de la Lista.
dobbleGameWhoseTurnIsIt(G,NP):-
    getGamePlayers(G,Players),
    listTurnos(Players,LT),
    max_list(LT,NMayor),
    turnoPlayer(Players,NMayor,NP).

% ------------------------- dobbleGamePlay --------------------------------

% ---------------------- Caso null-----------------------

% twoCards: Predicado que obtiene las 2 primeras cartas del CardsSet.
% Dominio: CardsSet, Cards.
twoCards(CS,[C,C1]):-
    getFirstCard(CS,C),
    getNextCards(CS,NextCards),
    getFirstCard(NextCards,C1),!.

% getCartasMesa: Predicado que obtiene las cartas de una Mesa.
% Dominio: Mesa, Cards.
getCartasMesa(Mesa,Mesa).

% dobbleGamePlay: Predicado que verifica que la Action sea null y se quitan 2 cartas de la baraja para dejarlas en Mesa, ningun jugador utiliza turno.
% Dominio: Game, Action, GameOut.
dobbleGamePlay(Game,Action,GameOut):-
    getGameMode(Game,Mode),
    Mode = "Stack",
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = null,
    getGameMesa(Game,Mesa),
    length(Mesa, 0),
    getGamecardsSet(Game,CardsSet),
    length(CardsSet, LengthCardsSet),
    LengthCardsSet > 1,
    twoCards(CardsSet,C2),
    subtract(CardsSet,C2,CSGame),
    getGameNumPlayers(Game,NP),
    getGameMode(Game,Mode),
    getGamePlayers(Game, Players),
    getGameEstado(Game,Estado),
    getGameFin(Game,Fin),
    actualizarGame(NP,CSGame,Mode,Players,C2,Estado,Fin,GameOut),!.

% ------------------------- Pass ----------------------------
    
% dobbleGamePlay: Predicado que verifica si el Action es pass. El jugador que posee el turno realiza la accion Pass, utilizando su 
% turno, al realizar la accion Pass, las cartas en juego (en Mesa) se devuelven a la baraja (cardsSet).
% Dominio: Game, Action, GameOut.
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

% ------------------------- Finish ----------------------------

% dobbleGamePlay: Predicado que verifica que la Action sea finish. Termina el Juego, cambiando el estado del Juego a 1, y 
% crea el mensaje del Ganador, guardandolo en el string Fin del Game.
% Dominio: Game, Action, GameOut.
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

% Caso donde Termina el Juego, cambiando el estado del Juego a 1, pero no hay ganadores, sino que crea el mensaje de los jugadores Empatados
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

% ------------------------- Play --------------------------------

% dobbleGamePlay: Predicado que verifica que el Action es spotIt, entonces realiza la accion del jugador.
% Caso donde Mesa si tiene cartas, y Jugador acierta en decir el elemento, entonces se le suma el Punto y se le agregan las cartas de la Mesa
% a sus cartas, e igualmente se le suma 1 al turno.
% Dominio: Game, [Action,Name, Element], GameOut
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

% Caso donde Mesa si tiene cartas, y Jugador falla en decir el elemento, se le suma 1 al turno del jugador 
% y la mesa queda igual.
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

% --------------------------------- Game Status -------------------------------

% dobbleGameStatus: Predicado que compara entrega un string, donde nos hace saber si el juego sigue en partida o ya termino, realizando una comparacion en el estado del game.
% Caso: Donde Estado es igual a 0, entonces el  juego sigue en partida.
% Dominio: Game, Str.
dobbleGameStatus(Game,Str):-
    not(var(Game)),
    getGameEstado(Game,Estado),
    Estado = 0,
    string_concat("\nEl Estado de Juego es :", " En Partida", Str),!.

% Caso donde el Estado es igual 1, por lo tanto el juego ya ha terminado.
dobbleGameStatus(Game,Str):-
    not(var(Game)),
    getGameEstado(Game,Estado),
    Estado = 1,
    string_concat("\nEl Estado de Juego es :", " Finalizado", Str),!.

% ----------------------------------- Game Score ---------------------------------

% dobbleGameScore: Predicado que entrega el Score de un Jugador el cual se entrega su nombre por el argumento.
% Dominio: Game, Name, Score.
dobbleGameScore(Game,Name, Score):-
    string(Name),
    var(Score),
    getGamePlayers(Game,Players),
    buscar(Players,Name, Player),
	getPlayerPuntos(Player, Score),!. 

% ---------------------------------- Game To String -------------------------------

% dobbleGameToString: Predicado que entrega el game en string.
% Caso: Caso cuando el length de la Mesa es igual a 0, por lo que se utiliza atomics to string para hacerlo un string.
% Dominio: Game, Str.
dobbleGameToString(Game, Str):-
    is_list(Game),
    getGameNumPlayers(Game, NumPlayers),
    getGamecardsSet(Game, CardsSet),
    getGameMode(Game, Mode),
    getGamePlayers(Game, Players),
    getGameMesa(Game, Mesa),
    length(Mesa,0),
    getGameFin(Game, Fin),
    atomics_to_string([NumPlayers], NumPlayersStr),
    cardsSetToString(CardsSet, CStr),
    atomics_to_string(Mesa, MesaStr),
    atomics_to_string([Mode], ModeStr),
    playersToString(Players,PlayersL),
    atomics_to_string(PlayersL, PlayersStr),
    dobbleGameStatus(Game, EstadoStr),
    %atomics_to_string([Fin], FinStr),
	string_concat(" ------ DOBBLE ------ \n", "Cantidad de Jugadores en partida : ", S1),
	string_concat(S1, NumPlayersStr, S2),
	string_concat(S2, "/n   Baraja :", S3),
	string_concat(S3, CStr, S4),
	string_concat(S4, "\n Modo de Juego :", S5),
	string_concat(S5, ModeStr, S6),
	string_concat(S6, "\n  Jugadores de la Partida :", S7),
	string_concat(S7, PlayersStr, S8),
	string_concat(S8, "\n Cartas en Mesa : ", S9),
	string_concat(S9, MesaStr, S10),
	string_concat(S10, "Estado partida : ", S11),
	string_concat(S11, EstadoStr, S12),
	string_concat(S12, "Fin de la partida : ", S13),
	string_concat(S13, Fin, Str),!.

% Caso cuando la Mesa si posee cartas, por lo tanto se utiliza cardsSet to String (Ya que la Mesa posee cartas), asi no ocurren errores.
dobbleGameToString(Game, Str):-
    is_list(Game),
    getGameNumPlayers(Game, NumPlayers),
    getGamecardsSet(Game, CardsSet),
    getGameMode(Game, Mode),
    getGamePlayers(Game, Players),
    getGameMesa(Game, Mesa),
    getGameFin(Game, Fin),
    atomics_to_string([NumPlayers], NumPlayersStr),
    cardsSetToString(CardsSet, CStr),
    cardsSetToString(Mesa, MesaStr),
    atomics_to_string([Mode], ModeStr),
    playersToString(Players,PlayersL),
    atomics_to_string(PlayersL, PlayersStr),
    dobbleGameStatus(Game, EstadoStr),
   % atomics_to_string([Fin], FinStr),
	string_concat(" ------ DOBBLE ------ \n", "Cantidad de Jugadores en partida : ", S1),
	string_concat(S1, NumPlayersStr, S2),
	string_concat(S2, "/n   Baraja : ", S3),
	string_concat(S3, CStr, S4),
	string_concat(S4, "\n Modo de Juego : ", S5),
	string_concat(S5, ModeStr, S6),
	string_concat(S6, "\n  Jugadores de la Partida : ", S7),
	string_concat(S7, PlayersStr, S8),
	string_concat(S8, "\n Cartas en Mesa : ", S9),
	string_concat(S9, MesaStr, S10),
	string_concat(S10, "Estado partida : ", S11),
	string_concat(S11, EstadoStr, S12),
	string_concat(S12, "Fin de la partida : ", S13),
	string_concat(S13, Fin, Str),!.


/*
       -------------------------------------- EJEMPLOS ----------------------------------------------

--------------------------- Ejemplos Predicado CardsSet -------------------------------

% Ej: se crea el set de Cartas con 8 elementos, y con una variable, esta variable da el total de cartas en el mazo.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS).

% Ej: se crea un set de cartas pero este estara limitado hasta 35 cartas.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,35,51,CS2).

% Ej: se crea un set de cartas de 5 elementos con su totalidad de elementos.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],5,21,26,CS3).

--------------------------- Ejemplos Predicado cardsSetIsDobble ------------------------

% Ej: ejemplo donde se verifica que el cardsSet creado si cumple con las reglas de IsDobble.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS),cardsSetIsDobble(CS).

% Ej: se utiliza el predicado cardsSetIsDobble, pero el cardsSet no cumple las reglas por lo cual retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],5,21,26,CS3),cardsSetIsDobble(CS3).

% Ej: se utiliza el predicado cardsSetIsDobble en una variable con valor indeterminado, retorna false.
% cardsSetIsDobble(CS).

--------------------------- Ejemplos Predicado NthCard ------------------------------------

% Ej: Ejemplo que obtiene la carta numero 35 del mazo.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS),cardsSetIsDobble(CS),cardsSetNthCard(CS,35,C).

% Ej: Ejemplo donde se quiere obtener una carta mayor al total de carta del mazo, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS3),cardsSetIsDobble(CS3),cardsSetNthCard(CS3,14,C).

% Ej: Ejemplo donde se quiere obtener una carta de un cardsSet no definido, retorna false.
% cardsSetNthCard(CS,5,C).

--------------------------- Ejemplos Predicado FindTotalCards -----------------------------

% Ej: Se entrega una carta obtenida del NthCard, por lo que deberia retornar el mismo valor de X = 57, una carta de 8 elementos.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS),cardsSetIsDobble(CS),cardsSetNthCard(CS,35,C), cardsSetFindTotalCards(C,TC).

% Ej: Se le entrega una carta vacia, retorna false.
% cardsSetFindTotalCards([],TC).

% Ej: Se le entrega una carta sin definir, retorna false.
% cardsSetFindTotalCards(C,TC).

--------------------------- Ejemplos Predicado MissingCards -------------------------------

% Ej: Se entrega un cardsSet que anteriormente fue limitado hasta 35 cartas, por lo tanto debera retornar las 22 restantes.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,35,51,CS),cardsSetIsDobble(CS),cardsSetMissingCards(CS,CS2).

% Ej: Si se entrega un mazo de cartas que no cumple las reglas de dobble, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],5,21,26,CS), cardsSetMissingCards(CS,CS2).

% Ej: Si se entrega un mazo de cartas vacio, retorna false.
% cardSetMissingCards([],CS2).

--------------------------- Ejemplos Predicado cardsSetToString -----------------------------

% Ej: Se entrega un cardsSet dobble, lo retorna como String.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS),cardsSetIsDobble(CS),cardsSetToString(CS,CS_STR).

% Ej: Se entrega un cardsSet que no cumple las reglas de dobble, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],5,21,26,CS), cardsSetToString(CS,CS_STR).

% Ej: Se entrega un cardsSet sin su totalidad de cartas, retorna el CardsSet en string.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,8,26,CS), cardsSetIsDobble(CS), cardsSetToString(CS,CS_STR).

---------------------------- Ejemplos Predicado dobbleGame ------------------------------------

% Ej: Se entrega un cardsSet valido y un string en el modo de juego "Stack" y el NumPlayer.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],8,X,26,CS),cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game).

% Ej: Se entrega un cardsSet que no ha sido valido, y que tampoco cumple las reglas de Dobble, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],5,21,26,CS), dobbleGame(3,CS,"Stack",25,Game).

% Ej: se entrega un Modo de juego que no es string, retorna False.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,Stack,15,Game).

---------------------------- Ejemplos Predicado dobbleGameRegister -----------------------------

% Ej: Se registra al jugador "Juan" a la partida de dobble.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2).

% Ej: Se vuelve a registrar al jugador Juan a la partida dobble, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Juan",G2,G3).

% Ej: Se registran 2 jugadores mas a la partida.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4).

$ Ej: Se entrega un game que no tiene registro al player, por lo cual entrega false. (Caso que aparece en el documento).
$ cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",G2,Game).

% Ej: Se intenta registrar el cuarto jugador cuando el limite es de 3 jugadores, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGameRegister("Pedro",G4,G5).

---------------------------- Ejemplos Predicado dobbleGameWhoseTurnIsIt -----------------------------

% Ej: entrega el turno del primer jugador, en este caso sera el de Fernando.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGameWhoseTurnIsIt(G4,NP).

% Ej: En este caso Fernando y copo tienen 6 turnos por lo tanto sera el turno de Juan.
% dobbleGameWhoseTurnIsIt([3,[[d, f, h, m],[b, e, h, k],[a, b, c, d],[b, g, j, m],[c, g, h, l],[a, h, i, j],[a, e, f, g],[d, e, j, l],[a, k, l, m],[d, g, i, k],[b, f, i, l],[c, f, j, k],[c, e, i, m]],"Stack",[["Fernando", [], 6, 0], ["Copo", [], 6, 0], ["Juan", [], 5, 0]],[],0,""],NP).

% Ej: Indicamos como argumento el nombre "Copo", retonara false ya que el nombre que entrega es Fernando.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGameWhoseTurnIsIt(G4,"Copo").

---------------------------- Ejemplos Predicado dobbleGamePlay -----------------------------

----------- null ------------

% Ej: Se realiza la accion null para quitar cartas del cardsSet y dejarlas sobre la mesa.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5).

% Ej: Si se vuelve a realizar un null pero ya hay cartas sobre la mesa, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,null,G6).
 
----------- pass -------------

% Ej: un usuario realiza la accion pass, las cartas que estaban en mesa vuelven al cardsSet y se le suma +1 al turno del usuario que realizo la accion.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6).

----------- spotIt -----------

% Ej: el usuario Copo realiza la accion SpotIt, pero no acierta el elemento de la carta, por lo tanto hace uso de su turno y las cartas quedan en mesa.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8).

% Ej: el usuario Juan realiza la accion SpotIt, acertando en decir el elemento correcto, por lo tanto se le suma puntaje.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9).

% Ej: Caso donde un usuario realiza la accion spotIt pero no es su turno, retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9),  dobbleGamePlay(G9,[spotIt,"Juan",b],Game10).

% Ej: Caso donde un usuario realiza spotIt pero no hay cartas en Mesa. retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9),  dobbleGamePlay(G9,[spotIt,"Fernando",c],Game10).

---------- finish ----------

% Ej: Se termina el juego, donde existe un ganador y se actualiza el estado del juago a 1.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGamePlay(G9, finish, Game1).

% Ej: se termina el juego, pero hay empate y se actualiza el estado del juego a 1.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGamePlay(G9,null,Game1),dobbleGamePlay(Game1,[spotIt,"Fernando",h],Game2), dobbleGamePlay(Game2, finish, Game3).

---------------------------- Ejemplos Predicado dobbleGameStatus -----------------------------

% Ej: Se entrega un game el cual aun no ha terminado, entrega una string que aun sigue en partida el juego.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGameStatus(G9,Str).

% Ej: Se entrega un game el cual ya ha finalizado, entrega un string que dice que el juego ya ha finalizado.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGamePlay(G9, finish, Game1), dobbleGameStatus(Game1, Str).

% Ej: Se entrega una variable sin definir en Game, retorna false.
% dobbleGameStatus(Game, Str).

---------------------------- Ejemplos Predicado dobbleGameScore -----------------------------

% Ej: Se consulta sobre el puntaje de Juan. retorna el score = 1.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGameScore(G9,"Juan",Score).

% Ej: Se consulta sobre un jugador que no existe retorna false.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGameScore(G9,"Diego",Score).

% Ej: Se consulta con una constante en el argumento del score, retorna false (Caso del documento)
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGameScore(G9,"Juan", 2).

 ---------------------------- Ejemplos Predicado dobbleGameToString -----------------------------

% Ej: Se hace uso del GameToString cuando el juego ya esta terminado.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGamePlay(G9, finish, Game1), dobbleGameToString(Game1,GStr).

% Ej: Se hace uso del GameToString cuando el juego aun no ha temrinado.
% el usuario Juan realiza la accion SpotIt, acertando en decir el elemento correcto, por lo tanto se le suma puntaje.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGameToString(G9,GStr).

% Ej: Se hace uso cuando la partida ya ha terminado y hay empate.
% cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z],4,13,26,CS), cardsSetIsDobble(CS), dobbleGame(3,CS,"Stack",15,Game),dobbleGameRegister("Juan",Game,G2), dobbleGameRegister("Copo",G2,G3), dobbleGameRegister("Fernando",G3,G4), dobbleGamePlay(G4,null,G5), dobbleGamePlay(G5,pass,G6), dobbleGamePlay(G6,null,G7), dobbleGamePlay(G7,[spotIt,"Copo",m],G8), dobbleGamePlay(G8,[spotIt,"Juan",b],G9), dobbleGamePlay(G9,null,Game1),dobbleGamePlay(Game1,[spotIt,"Fernando",h],Game2), dobbleGamePlay(Game2, finish, Game3), dobbleGameToString(Game3,GStr).

*/