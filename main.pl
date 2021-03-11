system(kepler).
system(sol).
system(sirius).
system(trappist).
system(alphaCentauri).
system(procyon).
system(splica).
system(polaris).
system(castor).
system(mira).
system(barnardstar).
system(gliese1061).
system(wolf359).
system(ross248).
system(tauCeti).
system(arcturus).
system(vega).
system(altair).
system(canopus).
system(poolux).

/*******************************************/
stargate(keplerToTrappist,kepler,trappist).
stargate(keplerToSol,kepler,sol).
stargate(siriusToAplhaCentauri,sirius,alphaCentauri).
stargate(siriusToSol,sirius,sol).
stargate(altairToSol,altair,sol).
stargate(polluxToVega,pollux,vega).
stargate(vegaToPollux,vega,pollux).
stargate(vegaToSol,vega,sol).
stargate(solToVega,sol,vega).
stargate(solToSirius,sol,sirius).
stargate(solToAltair,sol,altair).
stargate(solToKepler,sol,kepler).

stargate(ross248ToMira,ross248,mira).
stargate(miraToRoss248,mira,ross248).
stargate(miraToProcyon,mira,procyon).
stargate(procyonToMira,procyon,mira).
stargate(procyonToTrappist,procyon,trappist).
stargate(trappistToProcyon,trappist,procyon).
stargate(trappistToKepler,trappist,kepler).
stargate(trappistToSplica,trappist,splica).
stargate(splicaToTrappist,splica,trappist).
stargate(splicaToBarnardstar,splica,barnardstar).
stargate(barnardstarToSplica,barnardstar,splica).
stargate(barnardstarToTauceti,barnardstar,tauCeti).
stargate(tauetiToBarnardstar,tauCeti,barnardstar).

stargate(arcturusToGliese1061,arcturus,gliese1061).
stargate(gliese1061ToArcturus,gliese1061,arcturus).
stargate(gliese1061ToPolaris,gliese1061,polaris).
stargate(polarisToGliese1061,polaris,gliese1061).
stargate(polarisToAlphaCentauri,polaris,alphaCentauri).
stargate(alphaCentauriToPolaris,alphaCentauri,polaris).
stargate(alphaCentauriToSirius,alphaCentauri,sirius).
stargate(alphaCentauriToCastor,alphaCentauri,castor).
stargate(castorToAlphacentauri,castor,alphaCentauri).
stargate(castorToWolf359,castor,wolf359).
stargate(wolf359ToCastor,wolf359,castor).
stargate(wolf359ToCanopus,wolf359,canopus).
stargate(canopusToWolf359,canopus,wolf359).


/**********************************/
station(shapesetShipyard).
station(aliastraWarehouse).

station(station1).
station(station2).
station(station3).
station(station4).
station(station5).
station(station6).
station(station7).
station(station8).
station(station9).
station(station10).
station(station11).
station(station12).
station(station13).
station(station14).
station(station15).
station(station16).
station(station17).
station(station18).
station(station19).
station(station20).



/**********************************/
:- dynamic ship/3.
ship(mining1,miningType,[]).

cargoCapacity(miningType,100).

:-dynamic oreIncargo/2.
oreIncargo(,).

/**********************************/

asteroid(asteroid1,iron).
asteroid(asteroid2,gold).
asteroid(asteroid3,cobalt).
asteroid(asteroid4,gold).
asteroid(asteroid5,cobalt).
asteroid(asteroid6,gold).
asteroid(asteroid7,cobalt).
asteroid(asteroid8,gold).
asteroid(asteroid9,cobalt).
asteroid(asteroid10,gold).
asteroid(asteroid11,cobalt).
asteroid(asteroid12,gold).
asteroid(asteroid13,cobalt).
asteroid(asteroid14,gold).
asteroid(asteroid15,cobalt).
asteroid(asteroid16,gold).
asteroid(asteroid17,cobalt).
asteroid(asteroid18,gold).
asteroid(asteroid19,cobalt).
asteroid(asteroid20,gold).
asteroid(asteroid21,cobalt).
asteroid(asteroid22,gold).
asteroid(asteroid23,cobalt).
asteroid(asteroid24,gold).
asteroid(asteroid25,cobalt).
asteroid(asteroid26,gold).
asteroid(asteroid27,cobalt).
asteroid(asteroid28,gold).
asteroid(asteroid29,cobalt).
asteroid(asteroid30,gold).
asteroid(asteroid31,cobalt).
asteroid(asteroid32,gold).
asteroid(asteroid33,cobalt).
asteroid(asteroid34,gold).
asteroid(asteroid35,cobalt).
asteroid(asteroid36,gold).
asteroid(asteroid37,cobalt).
asteroid(asteroid38,gold).
asteroid(asteroid39,cobalt).
asteroid(asteroid40,gold).
asteroid(asteroid41,cobalt).


price(iron,100).
price(gold,500).
price(cobalt,200).


/**********************************/
location(kepler,keplerToTrappist).
location(kepler,keplerToSol).
location(sirius,siriusToAplhaCentauri).
location(sirius,siriusToSol).
location(altair,altairToSol).
location(pollux,polluxToVega).
location(vega,vegaToPollux).
location(vega,vegaToSol).
location(sol,solToVega).
location(sol,solToSirius).
location(sol,solToAltair).
location(sol,solToKepler).

location(ross248,ross248ToMira).
location(mira,miraToRoss248).
location(mira,miraToProcyon).
location(procyon,procyonToMira).
location(procyon,procyonToTrappist).
location(trappist,trappistToProcyon).
location(trappist,trappistToKepler).
location(trappist,trappistToSplica).
location(splica,splicaToTrappist).
location(splica,splicaToBarnardstar).
location(barnardstar,barnardstarToSplica).
location(barnardstar,barnardstarToTauceti).
location(tauCeti,tauetiToBarnardstar).

location(arcturus,arcturusToGliese1061).
location(gliese1061,gliese1061ToArcturus).
location(gliese1061,gliese1061ToPolaris).
location(polaris,polarisToGliese1061).
location(polaris,polarisToAlphaCentauri).
location(alphaCentauri,alphaCentauriToPolaris).
location(alphaCentauri,alphaCentauriToSirius).
location(alphaCentauri,alphaCentauriToCastor).
location(castor,castorToAlphacentauri).
location(castor,castorToWolf359).
location(wolf359,wolf359ToCastor).
location(wolf359,wolf359ToCanopus).
location(canopus,canopusToWolf359).

%
location(sol,shapesetShipyard).
location(sol,aliastraWarehouse).

location(sol,station1).
location(kepler,station2).
location(altair,station3).
location(sirius,station4).
location(vega,station5).
location(pollux,station6).
location(ross248,station7).
location(mira,station8).
location(procyon,station9).
location(trappist,station10).
location(splica,station11).
location(barnardstar,station12).
location(tauCeti,station13).
location(arcturus,station14).
location(gliese1061,station15).
location(polaris,station16).
location(alphaCentauri,station17).
location(castor,station18).
location(wolf359,station19).
location(canopus,station20).


%
location(sol,asteroid1).
location(sol,asteroid2).
location(sol,asteroid3).
location(kepler,asteroid4).
location(kepler,asteroid5).
location(altair,asteroid6).
location(altair,asteroid7).
location(sirius,asteroid8).
location(sirius,asteroid9).
location(vega,asteroid10).
location(vega,asteroid11).
location(pollux,asteroid12).
location(pollux,asteroid13).
location(ross248,asteroid14).
location(ross248,asteroid15).
location(mira,asteroid16).
location(mira,asteroid17).
location(procyon,asteroid18).
location(procyon,asteroid19).
location(trappist,asteroid20).
location(trappist,asteroid21).
location(splica,asteroid22).
location(splica,asteroid23).
location(barnardstar,asteroid24).
location(barnardstar,asteroid25).
location(tauCeti,asteroid26).
location(tauCeti,asteroid27).
location(arcturus,asteroid28).
location(arcturus,asteroid29).
location(gliese1061,asteroid30).
location(gliese1061,asteroid31).
location(polaris,asteroid32).
location(polaris,asteroid41).
location(alphaCentauri,asteroid33).
location(alphaCentauri,asteroid34).
location(castor,asteroid35).
location(castor,asteroid36).
location(wolf359,asteroid37).
location(wolf359,asteroid38).
location(canopus,asteroid39).
location(canopus,asteroid40).


/********************************************/


/********************************************/
commandsList:-
    write("jump - Lec do innego systemu."),nl,
    write("fly(X) - Lec do innego obiektu w systemie."),nl,
    write("menu - Wroc do glownego menu."),nl,
    write("save - Zapisz gre."),nl,
    write("info - Wyswietla obiekty w systemie"),nl,
    write("map - Wyswietla mape."),nl,
    write("mining(X) - Wydobywanie"),nl,
    write("cargo - Wyswietla ladunek statku."),nl,
    write("sell - Sprzedaj surowce."),nl,nl.
/********************************************/
:- dynamic currentSystem/1.
currentSystem(sol).
getCurrentSystem:-currentSystem(X),write(X).
jumptosystem :- currentLocation(L),stargate(L,_,X),currentSystem(Y), retract(currentSystem(Y)), assert(currentSystem(X)),stargate(S,X,Y),approach(S),pay(100).
jumptosystem :- currentLocation(L),not(stargate(L,_,_)),write("Musisz byc przy wrotach"),nl.

:- dynamic currentLocation/1.
currentLocation(asteroid1).
getCurrentLocationt:-currentLocation(X),write(X).
approach(X) :-currentSystem(Z),location(Z,X),currentLocation(Y),retract(currentLocation(Y)), assert(currentLocation(X)),gameMenu.
approach(X) :-write(X),write(" - nie ma w tym systemie."),nl.

:-dynamic balance/1.
balance(1000).
getBalance:-balance(X),write(X).
changeBalance(X) :-balance(Y),Y1 is Y + X, retract(balance(Y)), assert(balance(Y1)).

pay(X) :- balance(Y), Y>=100, changeBalance(-X),gameMenu.
pay(_) :- write("Za malo pieniedzy."),nl.

getSystemInfo(X):-write("Lokacje dostepne w systemie:"),nl,location(X,Y),write(Y),nl,fail.
getSystemInfo(_):-nl,true.

getCargoInfo:-write("Surowce na statku: "),nl,oreIncargo(X,Y),write(X),write(": "),write(Y),nl,fail.

mine(X):-currentLocation(L),asteroid(L,T),miningLoop(X,0,T).
mine(_):-write("Musisz byc przy asteroidzie"),nl.

miningLoop(0,O,T):-write("Zakonczono wydobycie."),nl,addOreToCargo(O,T).
miningLoop(X,O,T):-

    random_between(1,10,R),
    O1 is O+R,
    X1 is X-1,
    sleep(1),
    write("+"),write(R),write(" "),write(T),nl,
    X1>=0,
    miningLoop(X1,O1,T).

addOreToCargo(O,T):-
    ship(_,_,C),
    member(T,C),
    oreIncargo(T,ValueInCargo),
    O2 is ValueInCargo + O,
    retract(oreIncargo(T,_)),
    assert(oreIncargo(T,O2)).

addOreToCargo(O,T):-
    ship(_,_,C),
    not(member(T,C)),
    assert(oreIncargo(T,O)),
    append(C,[T],NewCargo),
    ship(Name,Type,Cargo),
    retract(ship(Name,Type,Cargo)),
    assert(ship(Name,Type,NewCargo)).

/*
sellAll:-sellOre.

sellOre:-
    currentLocation(L),station(L),!,oreIncargo(T,V),price(T,P),M is V*P,
    changeBalance(M),retract(oreIncargo(T,_)).
%sellAll:-gameMenu.
*/
sellAll:-currentLocation(L),station(L),ship(_,_,C),sellLoop(C),write("test!!!!!"),gameMenu.
sellAll:-write("Musisz byc na stacji."),nl.

sellLoop([Head|Tail]):-
    write("Head"),
    oreIncargo(Head,V),
    price(Head,P),M is V*P,
    changeBalance(M),
    retract(oreIncargo(Head,_)),
    
    sellLoop(Tail).

sellLoop([]):-retract(ship(_,_,_)),assert(ship(mining1,miningType,[])),!.



menuSelection(1):-
    retractall(currentSystem(_)),assert(currentSystem(sol)),
    retractall(currentLocation(_)),assert(currentLocation(asteroid1)),
    retractall(balance(_)),assert(balance(1000)),
    retractall(ship(_,_,_)),assert(ship(mining1,miningType,[])),
    retractall(oreIncargo(_,_)),
    gameMenu, mainLoop(_).
menuSelection(2):-write("Podaj nazwe zapisu: "),nl,read(X),loadGame(X),gameMenu,mainLoop(_).
menuSelection(3):-write("Quit game"),break.

mainMenu:-cls(50),write("1.New game"),nl,nl,write("2.Load game"),nl,nl,write("3.Quit"),nl,nl,read(X),nl,nl,menuSelection(X).
gameMenu:-cls(50),
          write('********************************************************************************'), nl,
          write('*****                               SPACE MINING!                         ******'), nl,
          write('********************************************************************************'), nl, nl,
          write("Aktulany system: "),currentSystem(S), write(S),write("  |  "),
          write("Aktulna lokacja: "),currentLocation(L), write(L),write("  |  "),
          write("Stan konta: $"),balance(B), write(B),nl,nl,cls(5),
          write("Wpisz help zeby zobaczyc dostepne komendy"),nl,nl.



              
gameCommand(jump):-jumptosystem.
gameCommand(fly(X)):-approach(X).
gameCommand(menu):-!,mainMenu.
gameCommand(quit):-break.
gameCommand(info):-currentSystem(X),getSystemInfo(X).
gameCommand(help):-commandsList.
gameCommand(save):-write("Podaj nazwe zapisu: "),nl,read(X),saveGame(X).
gameCommand(map):-gameMenu,readMap.
gameCommand(mining(X)):-mine(X).
gameCommand(cargo):-getCargoInfo.
gameCommand(sell):-sellAll.
gameCommand(_):-write(""),nl.

mainLoop(_):-
    read(Y),nl,
    gameCommand(Y),!,
    mainLoop(_).





stringFromFile("").
saveGame(F):-currentSystem(X),currentLocation(Y),balance(B),write_to_file(F,(X,Y,B)),!,
             string_concat(F,"cargo",Y1),retractall(typeList(_)),assert(typeList("")),ship(_,_,C),getCargoTypes(C),typeList(Tl),write_to_file(Y1,Tl),
             retract(vauluesList(_)),assert(vauluesList("")),getCargoValues(C),
             string_concat(F,"values",Z1),vauluesList(VL),write_to_file(Z1,VL).


:- dynamic vauluesList/1.
vauluesList("").
:- dynamic typeList/1.
typeList("").


getCargoTypes([Head|Tail]):-
    typeList(X),
    string_concat(X,Head,Xnew1),
    string_concat(Xnew1,",",Xnew2),
    retract(typeList(X)),assert(typeList(Xnew2)),
    getCargoTypes(Tail).
getCargoTypes([]):-true.

getCargoValues([Head|Tail]):-
    vauluesList(X),
    oreIncargo(Head,V),
    string_concat(X,V,Xnew1),
    string_concat(Xnew1,",",Xnew2),
    retract(vauluesList(X)),assert(vauluesList(Xnew2)),
    getCargoValues(Tail).
getCargoValues([]):-true.


loadGame(F):-loadMainData(F),string_concat(F,"cargo",F2),loadOresInCargo(F2),string_concat(F,"values",F3),loadOreValues(F3).
loadMainData(F):-retractall(stringFromFile(_)),assert(stringFromFile("")),read_file(F),split(_).
loadOresInCargo(F2):-retractall(stringFromFile(_)),assert(stringFromFile("")),read_file(F2),splitTypes.
loadOreValues(F3):-retractall(stringFromFile(_)),assert(stringFromFile("")),read_file(F3),splitValues.
split(L):-stringFromFile(S),write(S),split_string(S, ",", "", L),getDataFromSaveList(L,1).
splitTypes:-stringFromFile(S),write(S),split_string(S, ",", "", L),delete(L,"",L2),addTypes(L2,[]).
splitValues:-stringFromFile(S),write(S),split_string(S, ",", "", L),delete(L,"",L2),retractall(oreIncargo(_,_)),addValue(L2,0).



addTypes([Head|Tail],ConvertedTypes):-
    string_to_atom(Head,Type),
    append(ConvertedTypes,[Type],NewConverted),
    addTypes(Tail,NewConverted).
    

addTypes([],ConvertedTypes):-retractall(ship(_,_,_)),assert(ship(mining1,miningType,ConvertedTypes)),true.

addValue([Head|Tail],N):-
    ship(_,_,X),nth0(N, X, E, _),
    number_string(ValueNumber,Head),
    assert(oreIncargo(E,ValueNumber)),
    N2 is N + 1,
    addValue(Tail,N2).


addValue([],_):-true.

clear:-stringFromFile(X),retract(stringFromFile(X)).


getDataFromSaveList([Head|Tail],X):-
    data(X,Head),
    X1 is X+1,
    getDataFromSaveList(Tail,X1).
getDataFromSaveList([],_):-true.

data(1,X):-string_to_atom(X,A),retractall(currentSystem(_)) , assert(currentSystem(A)).
data(2,X):-string_to_atom(X,A),retractall(currentLocation(_)), assert(currentLocation(A)).
data(3,X):-number_string(A,X),retractall(balance(_)), assert(balance(A)).



:- dynamic stringFromFile/1.
changeString(X) :-stringFromFile(Y),string_concat(Y,X,Y1), retract(stringFromFile(Y)), assert(stringFromFile(Y1)).


%retA:-stringFromFile(X),retract(stringFromFile(X)).


readMap :- retractall(stringFromFile(_)),assert(stringFromFile("")),read_file("map.txt"),stringFromFile(X),write(X).

write_to_file(File, Text) :-
    open(File, write, Stream),
    write(Stream, Text), nl,
    close(Stream).

read_file(File) :- 
    open(File, read, Stream),
    get_char(Stream, Char1),
    process_stream(Char1, Stream),
    close(Stream).

process_stream(end_of_file, _) :- !.

process_stream(Char, Stream) :-
    %write(Char),
    changeString(Char),
    
    get_char(Stream, Char2),
    process_stream(Char2, Stream).



cls(0) :-nl.
cls(X) :- nl,X1 is X-1,cls(X1).


