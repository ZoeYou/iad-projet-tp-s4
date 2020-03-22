:- use_module(library(clpfd)).

%exo1
reines4_modele2(Vars,Options) :-
Vars = [X1,X2,X3,X4],
Vars ins 1..4,
all_distinct([X1,X2,X3,X4]),
X1+1 #\= X2+2, X1+1 #\= X3+3, X1+1 #\= X4+4,
X2+2 #\= X3+3, X2+2 #\= X4+4, X3+3 #\= X4+4,
X1-X1 #\= X2-2, X1-1 #\= X3-3, X1-1 #\= X4-4,
X2-2 #\= X3-3, X2-2 #\= X4-4, X3-3 #\= X4-4,
labeling(Options, Vars).


nb_reines4_modele2(Nb):-
findall(L, reines4_modele2(L, [ff]), Res),
length(Res, Nb).



%exo2
declareDomaines([E2,E1,C50,C20,C10],[X1,X2,X3,X4,X5]) :-
	Pieces_a_retourner = [X1,X2,X3,X4,X5],
	Pieces_a_retourner ins 0..sup,
	E2 in 0..X1,
	E1 in 0..X2,
	C50 in 0..X3,
	C20 in 0..X4,
	C10 in 0..X5.

monnaie1(TotalDonne, TotalDu, Pieces_en_reserve, Pieces_a_retourner):-
	Pieces_a_retourner = [R1,R2,R3,R4,R5],
	declareDomaines(Pieces_a_retourner,Pieces_en_reserve),
	TotalDonne - TotalDu #= R1*200+R2*100+R3*50+R4*20+R5*10,
	labeling([ff], Pieces_a_retourner).



%exo3
send([S,E,N,D,M,O,R,Y]):-
          Vars = [S,E,N,D,M,O,R,Y],
          Vars ins 0..9,
          all_different(Vars),
                    S*1000 + E*100 + N*10 + D +
                    M*1000 + O*100 + R*10 + E #=
          M*10000 + O*1000 + N*100 + E*10 + Y,
          M #\= 0, S #\= 0,
          labeling([ff], Vars).



%exo4
commandes(CM,CP,CR,CH,JM,JP,JR,JH,MM,MP,MR,MH,YM,YP,YR,YH):-
	Morceux = [CM,CP,CR,CH,JM,JP,JR,JH,MM,MP,MR,MH,YM,YP,YR,YH],
	Morceux ins 0..sup,
	CM+CP+CR+CH #>= 3,
    CM+CP+CR+CH #=< 5,
    CR #= 0,
    CH #=< 1,
    JM+JP+JR+JH #= 4,
    JP #>= 2,
  	MM+MP+MR+MH #= 3, 
 	MP #= YP,
 	YM+YP+YR+YH #>= 6,  
 	YM+YP+YR+YH #=< 10, 
  	2*YR #=< (YM+YP+YR+YH),
  	mod(CM+JM+MM+YM,4)#=0,
  	mod(CP+JP+MP+YP,4)#=0,
  	mod(CR+JR+MR+YR,4)#=0,
  	mod(CH+JH+MH+YH,4)#=0,
  	labeling([ff],Morceux).

pizza(Margherita,Picante,Romaine,Hawai):-
	Vars = [Margherita,Picante,Romaine,Hawai],
	Vars ins 0..sup,
	commandes(CM,CP,CR,CH,JM,JP,JR,JH,MM,MP,MR,MH,YM,YP,YR,YH),
  	(CM+JM+MM+YM) #= Margherita*4,
  	(CP+JP+MP+YP) #= Picante*4,
  	(CR+JR+MR+YR) #= Romaine*4,
  	(CH+JH+MH+YH) #= Hawai*4,
	labeling([ff],Vars).

nb_pizza_solution(Nb):-
findall([Margherita,Picante,Romaine,Hawai], pizza(Margherita,Picante,Romaine,Hawai), Res),
length(Res, Nb).
 
%% ===une solution=====:
%% pizza([Margherita,Picante,Romaine,Hawai]).
%% Margherita = Picante, Picante = 1,
%% Romaine = 0,
%% Hawai = 2.
%% ===nb de solution===: 3545.
	


%exo5
seven_eleven(Price1,Price2,Price3,Price4):-
	Vars = [Price1,Price2,Price3,Price4],
	Vars ins 0..sup,
	Price1 #=< Price2,
	Price2 #=< Price3,
	Price3 #=< Price4,
	(Price1*Price2*Price3*Price4)#=711000000,
	(Price1+Price2+Price3+Price4)#=711,
	labeling([ff],Vars).

nb_seven_eleven(Nb):-
	findall([Price1,Price2,Price3,Price4],seven_eleven(Price1,Price2,Price3,Price4),Res),
	length(Res, Nb).



%exo6
count([],_,0).
count([X|T],V,C):- count(T,V,C1), X is V, C is C1+1,!.
count([_|T],V,C):- count(T,V,C).

congres(A,B,C,D,E,F,G,H,I,J,K):-
	Vars = [A,B,C,D,E,F,G,H,I,J,K],
	Vars ins 1..4,
	Salles =[C1,C2,C3,C4],
	Salles ins 0..3,

    all_distinct([A,J]),
    all_distinct([J,I]),
    all_distinct([I,E]),
    all_distinct([C,F]),
    all_distinct([F,G]),
    all_distinct([D,H]),
    all_distinct([B,D]),
    all_distinct([K,E]),
    all_distinct([B,I,H,G]),
    all_distinct([A,G,E]),
    all_distinct([B,H,K]),
    all_distinct([A,B,C,H]),
    all_distinct([D,F,J]),

	E#<J,
	D#<K, F#<K,	

	count(Vars,1,C1), 
	count(Vars,2,C2), 
	count(Vars,3,C3), 
	count(Vars,4,C4),
	labeling([ff],Vars).



%exo7
make_square(0,_,[]) :- !.
make_square(I,N,[Row|Rest]) :-
	length(Row,N),
   	I1 is I - 1,
   	make_square(I1,N,Rest).

all_different_in_row([]) :- !.
all_different_in_row([Row|Rest]) :-
 	all_different(Row),
    all_different_in_row(Rest).

all_different_in_column(Square) :-
	transpose(Square,TSquare),
   	all_different_in_row(TSquare).   

latin_square(N,Square) :-
	make_square(N,N,Square),
   	append(Square,AllVars),
   	AllVars ins 1..N,
   	all_different_in_row(Square),
   	all_different_in_column(Square),
   	labeling([ff],AllVars).



%exo8
blocks([], [], []).
blocks([N1,N2,N3|Ns1], [N4,N5,N6|Ns2], [N7,N8,N9|Ns3]) :-
        all_distinct([N1,N2,N3,N4,N5,N6,N7,N8,N9]),
        blocks(Ns1, Ns2, Ns3).

sudoku(Rows) :-
    length(Rows, 9), maplist(same_length(Rows), Rows),
    append(Rows, Vs), Vs ins 1..9,
    maplist(all_distinct, Rows),
    transpose(Rows, Columns),
    maplist(all_distinct, Columns),
    Rows = [As,Bs,Cs,Ds,Es,Fs,Gs,Hs,Is],
    blocks(As, Bs, Cs),
    blocks(Ds, Es, Fs),
    blocks(Gs, Hs, Is).

problemS(1, [[_,_,9,_,_,1,6,2,_],
            [5,7,_,_,2,8,_,3,_],
            [3,_,_,7,_,_,_,_,4],
            [8,9,_,_,7,_,4,_,_],
            [_,6,_,5,_,3,_,9,_],
            [_,_,1,_,9,_,_,7,6],
            [6,_,_,_,_,7,_,_,8],
            [_,4,_,1,3,_,_,6,5],
            [_,2,7,6,_,_,9,_,_]]).

displayline(L) :- printf('|%d %d %d|%d %d %d|%d %d %d|%n',L). 
displayseperator :- printf('+-----+-----+-----+%n',[]).

solve(Rows):- problemS(1, Rows), sudoku(Rows),
   maplist(labeling([ff]), Rows), maplist(writeln, Rows).



%exo9
%problem(1,[A1,A2,A3,A5,B1,B2,B3,B4,B5,C1,C2,C3,C4,C5,D2,D3,D4,D5,E1,E2,E3,E5]).

nombre_croiss(P):-
      P = [A1,A2,A3,A5,B1,B2,B3,B4,B5,C1,C2,C3,C4,C5,D2,D3,D4,D5,E1,E2,E3,E5],
      P ins 0..9,
      A1*A2*A3 #= 3,
      B1+B2+B3+B4+B5 #= 12,
      C1*10000+C2*1000+C3*100+C4*10+C5 #= (B4*100+C4*10+D4)*(B4*100+C4*10+D4),

      square(D2*1000+D3*100+D4*10+D5),
      E1*E2*E3 #= 18,
      A1*B1*C1 #= 2,
      A2 #= E2,B2 #= D2,
      A3*10000+B3*1000+C3*100+D3*10+E3 #= (E1*100+E2*10+E3)*(E1*100+E2*10+E3),
      B4*C4*D4 #= 12,
      div(E1*100+E2*10+E3,A5+B5+C5+D5+E5)*(A5+B5+C5+D5+E5) #= E1*100+E2*10+E3,
      N1 is A5+B5+C5+D5+E5,
      isPrime(N1),

      A1 #\= 0,A2 #\= 0,A3 #\= 0,A5 #\= 0,B1 #\= 0,C1 #\= 0,E1 #\= 0,
      labeling([ff],P).



divisible(X,Y) :- 0 is X mod Y, !.
divisible(X,Y) :- X > Y+1, divisible(X, Y+1).

isPrime(2) :- true,!.
isPrime(X) :- X < 2,!,false.
isPrime(X) :- not(divisible(X, 2)).

square(Square):- N#>0, Square #= N*N.

solove(P):- problem(1,P), nombre_croiss(P).




