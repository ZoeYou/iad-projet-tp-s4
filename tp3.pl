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
pizza(Margherita,Picante,Romaine,Hawai):-
	Vars = [Margherita,Picante,Romaine,Hawai],
	Vars ins 0..sup,

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
  	YR #=< (YM+YP+YR+YH)/2,

  	CM+JM+MM+YM #= Margherita*4,
  	CP+JP+MP+YP #= Picante*4,
  	CR+JR+MR+YR #= Romaine*4,
  	CH+JH+MH+YH #= Hawai*4,

	labeling([ff],Vars).
	
%exo5
seven_eleven(Price1,Price2,Price3,Price4):-
	Vars = [Price1,Price2,Price3,Price4],
	(Price1*Price2*Price3*Price4)#=7.11,
	sum([Price1,Price2,Price3,Price4], #=, 7.11),
	labeling([ff],Vars).
      
      








