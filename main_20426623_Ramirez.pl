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
limitarmazo(Mazo,M,L):-
    length(L,M),
    append(L,_,Mazo),
    length(L,Y),
    M = Y.

limitarmaz(Mazo,M,L):-
    length(L,M),
    append(L,_,Mazo),
    length(L,Y),
    M >= Y.

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

myRandom(Xn, Xn1):-
AX is 110 * Xn,
AXC is AX + 123,
Xn1 is (AXC mod 226).

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
    
mazoAleatorio(N,Mazo, MazoAl):-
    myRandom(N, Seed),
    myshuffle(Mazo,Seed,MazoAl),!.
    

cardsSet(LE,NumE,MaxC,Seed,CS):-
    NumEA is NumE-1,
    mazo(NumEA,L),
    length(L,X),
    MaxC = X,
    melemento(L,LE,L2),
    limitarmazo(L2,MaxC,CS1),
    mazoAleatorio(Seed,CS1,CS),
    !.

cardsSet(LE,NumE,MaxC,Seed,CS):-
    NumEA is NumE-1,
    mazo(NumEA,L),
    melemento(L,LE,L2),
    limitarmaz(L2,MaxC,Cs1),
    mazoAleatorio(Seed,Cs1,CS),
    !.
    
% Selectores Cartas
getFirstCard([First|_],First).
getNextCards([_|NextCards], NextCards).

%cardsSetIsDoble
% Mismo tamano toda carta
mismoTamano([_|[]],_):-
    !.
mismoTamano([P|M], V):-
    nth1(1,M,SC),
    same_length(P,SC),
    mismoTamano(M,V).
    
% No elemento en comun en la carta
noElementoComun([],_).
noElementoComun([P|M],X):-
    is_set(P),
    noElementoComun(M,X).

% Interseccion = 1
interseccionCarta([_|[]],_):-
    !.

interseccionCarta([P|M],V):-
    interseccionCartas(P,M,V),
    interseccionCarta(M,V).

interseccionCartas(_,[_|[]],_):-
    !.
interseccionCartas(C,[P|M], V):-
    intersection(C,P,LI),
    length(LI,1),
    interseccionCartas(C,M,V).

cardsSetIsDobble(CS):- 
    mismoTamano(CS,_),
    noElementoComun(CS,_),
    interseccionCarta(CS,_).

% cardsSetNthCard
cardsSetNthCard(CS,I,C):-
    length(CS,X),
    I < X,
    nth0(I,CS,C),
    !.

%cardsSetFindTotalCards
cardsSetFindTotalCards(C,I):-
    length(C,N),
    I is (((N-1)*(N-1))+(N-1)+1),
    !.

%cardsSetMissingCards
cardsSetMissingCards(Cartas,CS):-
    cardsSetIsDobble(Cartas),
    cardsSetNthCard(Cartas,0,C),
    length(C,X),
    cardsSetFindTotalCards(C,TC),
    cardsSet([a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, t,u,v,x,y,z,aa,ab,ac,ad,ae,af,ag,ah,ai,aj,ak,al,am,an,ao,ap,aq,ar,at,au,av,ax,ay,az,ba,bb,bc,bd,be,bf,bg,bh,bi,bj,bk,bl,bm,bn,bo,bp,bq,br,bs,bt,bu,bv,bx,by,bz],X,TC,12,CS1),
    subtract(CS1,Cartas,CS).

    %cardsSetToString
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

cardsSetToString(CS, CS_STR):-
    cardsSetToString1(CS,X),
    atomics_to_string(X,L1),
    string_concat(" -----------  MAZO ---------------  ", "\n",S1),
    string_concat(S1,L1 ,S2),
    string_concat(S2, "\n -------- FIN --------", CS_STR).

% TDA JUGADOR
%
%
%
player(Name,[Name,[],0,0]):-
    string(Name),!.

%Selectores
getPlayerName([Name|_],Name).
getPlayerCards([_,Cards,_,_],Cards).
getPlayerTurno([_,_,Turno,_],Turno).
getPlayerPuntos([_,_,_,Puntos],Puntos).

getFirstPlayer([Player|_],Player).
getNextPlayers([_|NextPlayers],NextPlayers).

%Modificador
actualizarPlayer(Name,Cards,Turno,Puntos,PlayerOut):-
    PlayerOut = [Name,Cards,Turno,Puntos],!.
%TDA Game
%
%
%
game(NumPlayers,CardsSet,Mode,_,[NumPlayers,CardsSet,Mode,[],[],0,""]):-
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

dobbleGameRegister(Nombre,GameIn,GameOut):-
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
    mymember(Nombre,Players),
    actualizarGame(NP,CardsSet,Mode,Players,Mesa,Estado,Fin,GameOut),!.

dobbleGameRegister(_,GameIn,GameIn):-
    getGameNumPlayers(GameIn,NP),
    integer(NP),
    getGamePlayers(GameIn, Players),
    length(Players,TP),
    TP = NP,!.

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
listTurnos([],[]):-!.
listTurnos(Players, [Turno|LT]):-
    getFirstPlayer(Players, Player),
    getPlayerTurno(Player, Turno),
    getNextPlayers(Players,NextPlayers),
    listTurnos(NextPlayers, LT).

turnoPlayer([[Name,_,X,_]|_],N,Name):- 
    X<N,!.

turnoPlayer(Players,N,NP):-
    getNextPlayers(Players, NextPlayers),
    turnoPlayer(NextPlayers,N,NP).
    
cantNMayor([],_,Cont,L,Cont):-
    Cont = L,
    !.

cantNMayor(Players,NMayor,Cont,L,I):-
    getFirstPlayer(Players,P),
    getNextPlayers(Players, NextPlayers),
    getPlayerTurno(P,T),
    T = NMayor,
    ContAux is Cont+1,
    cantNMayor(NextPlayers, NMayor,ContAux,L, I).

cantNMayor(Players, NMayor,Cont,L ,I):-
    getNextPlayers(Players, NextPlayers),
    cantNMayor(NextPlayers,NMayor,Cont,L,I).

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

buscar([[Name,X,Y,Z]|_],Name,[Name,X,Y,Z]):-!.
buscar(Players,Name,P):-
   getNextPlayers(Players,NextPlayers),
   buscar(NextPlayers,Name,P).



actualizarPlayersAux(Players,Player,TL,Cont,[Player|PsA]):-
    Cont=< TL,
  	getFirstPlayer(Players, FirstPlayer),
    getNextPlayers(Players, NextPlayers),
    getPlayerName(FirstPlayer, NameP),
    getPlayerName(Player, Name),
    Name = NameP,
    ContAux is Cont+1,
    actualizarPlayersAux(NextPlayers,Player,TL,ContAux,PsA).

actualizarPlayersAux(Players,Player,TL,Cont,[FirstPlayer|PsA]):-
    Cont=< TL,
    getFirstPlayer(Players, FirstPlayer),
    getNextPlayers(Players, NextPlayers),
    ContAux is Cont+1,
    actualizarPlayersAux(NextPlayers,Player,TL,ContAux,PsA).
                     
actualizarPlayersAux(_,_,TL,Cont,[]):-
    Cont=TL,!.

actualizarPlayers(Players,Player,PsA):-
    length(Players,TL),
    actualizarPlayersAux(Players,Player,TL,0,PsA).

listPuntos([],[]):-!.
listPuntos(Players, [Puntos|LT]):-
    getFirstPlayer(Players, Player),
    getPlayerPuntos(Player, Puntos),
    getNextPlayers(Players,NextPlayers),
    listPuntos(NextPlayers, LT).

esganadorAux(Players,MaxP,Int):-
    esganador(Players,MaxP,Int),!.

esganador([],_,0):-!.
esganador([[_,_,_,X]|NextPlayers],X,Int):-
    esganador(NextPlayers,X,IntAux),
    Int is IntAux+1.
esganador([_|NextPlayers], MaxP, Int):-
    esganador(NextPlayers,MaxP,Int).

buscarganador([[NP,_,_,MaxP]|_],MaxP,NP):-!.
buscarganador([_|NextPlayers],MaxP,N):-
    buscarganador(NextPlayers,MaxP,N).

buscarganadores([],_,[]):-!.
buscarganadores([[NP,_,_,MaxP]|NextPlayers],MaxP,[NP|L]):-
    buscarganadores(NextPlayers,MaxP,L).
buscarganadores([_|NextPlayers],MaxP,L):-
    buscarganadores(NextPlayers,MaxP,L).

getCartasMesa(Mesa,Mesa).

% Se quitan 2 cartas de la baraja para dejarlas en Mesa, ningun jugador utiliza turno.
dobbleGamePlay(Game,Action,GameOut):-
    getGameEstado(Game, Estado),
    Estado = 0,
    Action = null,
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
    getGameMode(Game,Mode),
    getGameMesa(Game,Mesa),
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

% Si El juego esta Finalizado el Juego queda igual.
dobbleGamePlay(Game, _, Game):-
    getGameEstado(Game, Estado),
    Estado = 1,!.