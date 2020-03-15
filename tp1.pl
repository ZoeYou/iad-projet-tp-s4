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

calories_repas(H,P,D,C) :- repas(H,P,D), calories(H,X), calories(P,Y), calories(D,Z),C is (X+Y+Z).

repas_equilibre(H,P,D) :- calories_repas(H,P,D,C), C < 900 .

boisson(vin).
boisson(eau-minerale) .
boisson(biere) .

repas_complete(H,P,D,B) :- repas(H,P,D), boisson(B) .

%exo2
%?- [a,[a]]=[H|T].
%H = a,
%T = [[a]].

%?- [[a,b],c]=[[H|T1]|T2]
%|    .
%H = a,
%T1 = [b],
%T2 = [c].

%?- [a,b,[c]]=[H1|[H2|[H3|T]]]
%|    .
%H1 = a,
%H2 = b,
%H3 = [c],
%T = [].

%exo3
app([],L,L) .
app([T|X],Y,[T|Z]) :- app(X,Y,Z) .


%methode1
dernier(X,[X]) .
dernier(X,[_|L]) :- dernier(X,L).
%methode2
%dernier2(X,[X]) .
%dernier2(X,L) :- append(_,X,L) .


mem(X,[X|_]) .
mem(X,[_|L]) :- mem(X,L) .


double([X],[X,X]) .
double([H|L],[H|[H|S]]) :- double(L,S) .


longueurpaire([]) .
longueurpaire([_|T]) :- longueurimpaire(T) .


longueurimpaire([_]) .
longueurimpaire([_|T]) :- longueurpaire(T) .


rev([X],[X]) .
rev() :- rev([a|L2],L) .


