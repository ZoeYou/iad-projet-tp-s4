%exo1
%1)
entier(N,N) .
entier(N,I) :- integer(I), I>= N .
entier(N,I) :- N1 is N+1, entier(N1,I) .
%2)
racine(N,R) :- entier(0,R), MIN is R*R, N >= MIN, MAX is (R+1)*(R+1), N < MAX, !.

%exo2
%1） 
liste(1,[1]) .
liste(N,L) :- N>1, N1 is N-1, liste(N1,L1), append(L1,[N],L).
%2)
genere(N,L) :- M is N*N, liste(M,L) .
%3)
insertion(L,X,[X|L]).
insertion([Y|L],X,[Y|P]) :- insertion(L,X,P) . 

perm([],[]) .
perm([X|L],P) :- perm(L,G), insertion(G,X,P) .
%4)
nombre_latin(N,M) :- M is (N*(N*N+1)) // 2.
%5)
somme_premiers(L,0,0).
somme_premiers([X|L],N,R) :- N1 is N-1, somme_premiers(L,N1,S), R is S+X.
%6)
elimine(L,0,L).
elimine([_|L],N,M) :- N1 is N-1, elimine(L,N1,M).
%7)
somme_ligne(L,N,1,R) :- somme_premiers(L,N,R).
somme_ligne(L,N,M,R) :- elimine(L,N,J), P is M-1, somme_ligne(J,N,P,R).
%8)
% calculer la somme de la 1er colonne
somme_colonne_aux([X|L],N,X) :- length(L,P), P<N.
somme_colonne_aux([X|L],N,R) :- length(L,P), P>=N, elimine([X|L],N,J), somme_colonne_aux(J,N,Q), R is X+Q.

somme_colonne(L,N,M,R) :- P is M-1, elimine(L,P,J), somme_colonne_aux(J,N,R).
%9)
lignes_aux(L,N,0,V).
lignes_aux(L,N,M,V) :- somme_ligne(L,N,M,V), P is M-1, lignes_aux(L,N,P,V).

ligne_ok(L,N,V) :- lignes_aux(L,N,N,V).
%10)
colonne_aux(L,N,0,V).
colonne_aux(L,N,M,V) :- somme_colonne(L,N,M,V), P is M-1, colonne_aux(L,N,P,V).

colonne_ok(L,N,V) :- colonne_aux(L,N,N,V).
%11)
carre_latin(N,L1) :-
	nombre_latin(N,M), genere(N,L), perm(L,L1), ligne_ok(L1,N,M), colonne_ok(L1,N,M).





 