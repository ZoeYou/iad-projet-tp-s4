%exo1
horsdoeuvre(artichauts).
horsdoeuvre(crevettes).
horsdoeuvre(oeufs).
viande(grillade-de-boeuf).
viande(poulet).
poisson(loup).
poisson(sole).
dessert(glace).
dessert(tarte).
dessert(fraises).

plat(X) :- poisson(X) .
plat(X) :- viande(X) .

repas(H,P,D) :- horsdoeuvre(H), plat(P), dessert(D) .

calories(artichauts, 150).
calories(crevettes, 250).
calories(oeufs, 200).
calories(grillade-de-boeuf, 500).
calories(poulet, 430).
calories(loup, 250).
calories(sole, 200).
calories(glace, 300).
calories(tarte, 400).
calories(fraises, 250). 

calories_repas(H,P,D,C) :- repas(H,P,D), calories(H,X), calories(P,Y), calories(D,Z), C is (X+Y+Z).

repas_equilibre(H,P,D) :- calories_repas(H,P,D,C), C < 900 .

boisson(vin).
boisson(eau-minerale) .
boisson(biere) .

repas_complete(H,P,D,B) :- repas(H,P,D), boisson(B) .

%exo2_Listes

%?- [a,[a]]=[H|T].
%H = a,
%T = [[a]].

%?- [[a,b],c]=[[H|T1]|T2].
%H = a,
%T1 = [b],
%T2 = [c].

%?- [a,b,[c]]=[H1|[H2|[H3|T]]].
%H1 = a,
%H2 = b,
%H3 = [c],
%T = [].

%exo3_quelquesPredicats
app([],L,L) .
app([T|X],Y,[T|Z]) :- app(X,Y,Z) .


%methode1
dernier(X,[X]) .
dernier(X,[_|L]) :- dernier(X,L).

%methode2
dernier2(X,L) :- app(_,[X],L).


mem(X,[X|_]) .
mem(X,[_|L]) :- mem(X,L) .


double([X],[X,X]) .
double([H|L],[H|[H|S]]) :- double(L,S) .


longueurpaire([]) .
longueurpaire([_|T]) :- longueurimpaire(T) .
longueurimpaire([_|T]) :- longueurpaire(T) .


rev([],[]).
rev([X|L], Y) :- rev(L, Z), append(Z,[X],Y).

% version avec accumulateur
rev_aux([], Accu, Accu).
rev_aux([X|R], Accu, L) :- rev_aux(R,[X｜Accu],L).
rev1(L1,L2) :- rev_aux(L1,[],L2).


% version avec app
prefixe(L1,L2) :- append(L1,_,L2).

$ version sans app
prefixe1([],L).
prefixe1([X|P],[X|L]) :- prefixe1(P,L). 


setof(X,prefixe(X,[1,2,3]),L).
setof(pair(L1,L2), app(L1,L2,[1,2,3]),L).


nodouble([X|L]) :- mem(X,L).
nodouble([X|L]) :- nodouble(L).


%exo4_predicat_arithmetiques
fact(0,1),
fact(N,P) :- N>0, N1 is N-1, fact(N1,P1), P is P1*N .


division(A,B,Q,R) :- integer(A), integer(B), A>=0, B>0, div(A,B,Q,R).
div(A,B,0,A) :- A < B.
div(A,B,Q,R) :- A >= B, A1 is A - B, div(A1,B,Q1,R), Q is Q1 + 1.


sumlist([],0).
sumlist([X|L], S) :- integer(X), sumlist(L,S1), S is S1 + X.


sorted([]).
sorted([X]).
sorted([X,Y|L]) :- integer(X), integer(Y), sorted([Y|X]), Y >=X .


merge([],L,L).
merge(L,[],L).
merge([X1|Y1], [X2|Y2], [X1|L]):- integer(X1), integer(X2), X2 >= X1, merge(Y1,[X2|Y2],L).
merge([X1|Y1], [X2|Y2], [X2|L]):- integer(X1), integer(X2), X1 > X2, merge([X1|Y1],Y2,L).


%exo5_arbres
somme(arbre(E,G,D),N) :- integer(E), somme(G,N1), somme(D,N2), N is E+N1+N2.
somme(X,X) :- integer(X).


%exo6_predicatsProlog_sur_listes
insert(X,[],[X]).
insert(X,[Y|L],[X,Y|L]) :- Y >= X, !.
insert(X, [Y|L], [Y|P]) :- integer(X), insert(X, L, P).


tri([],[]).
tri([X|L],R) :- integer(X), tri(L,R1), insert(X,R1,R).


trie([]).
trie([_]).
trie([X,Y|L]) :- trie([Y|L]), Y >= X.

trie1(L) :- tri(L,L).


intervalles([],[]).
intervalles([_],[]).
intervalles([X,Y|L],[I|R]) :- intervalles([Y|L],R), I is Y - X.

