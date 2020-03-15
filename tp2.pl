%exo1
%1)
entier(N,I) :- integer(I), I>= N .
entier(N,N) .
entier(N,I) :- N1 is N+1, entier(N1,I) .
%2)
racine(N,R) :- entier(0,R), MIN is R*R, N >= MIN, MAX is (R+1)*(R+1), N < MAX, !.

%exo2
%1） 
liste(1,[1]) .
liste(N,L) :- N>1, N1 is N-1, liste(N1,L1), append(L1,[N],L).
%2)
genere(N,L) :- N1 is N*N, liste(N1,L) .
%3)
perm([],[]) .
perm([X|L],P) :- perm(L,G), insertion(G,X,P) .

insertion(L,X,[X|L]).
insertion([Y|L],X,[Y|P]) :- insertion(L,X,P) . 


 