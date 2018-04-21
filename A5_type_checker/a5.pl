bl(true).
bl(false).

variable(X):- string(X).
hastype(_,const(N),intT):- integer(N).
hastype(_,bl(B),boolT):- bl(B).

lookup([],X):- fail.
lookup([X|Xs],X):- !.
lookup([Y|Ys],X):- lookup(Ys,X).

hastype(Gamma,variable(X),T):- lookup(Gamma,p(variable(X),T)).
hastype(Gamma,abso(X),T):- hastype(Gamma,X,intT).
hastype(Gamma,add(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,sub(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,mul(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,div(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,mod(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,expo(E1,E2),intT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

hastype(Gamma,nott(E1),boolT):- hastype(Gamma,E1,boolT).
hastype(Gamma,andd(E1,E2),boolT):- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,orr(E1,E2),boolT):- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,xorr(E1,E2),boolT):- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).
hastype(Gamma,implies(E1,E2),boolT):- hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT).

hastype(Gamma,equ(E1,E2),boolT):- (hastype(Gamma,E1,intT),hastype(Gamma,E2,intT));(hastype(Gamma,E1,boolT),hastype(Gamma,E2,boolT)).
hastype(Gamma,gtr(E1,E2),boolT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,les(E1,E2),boolT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,geq(E1,E2),boolT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).
hastype(Gamma,leq(E1,E2),boolT):- hastype(Gamma,E1,intT),hastype(Gamma,E2,intT).

hastype(Gamma,tuple([]),cartesianT([])).
hastype(Gamma,tuple([X]),cartesianT([T])):- hastype(Gamma,X,T).
hastype(Gamma,tuple([X|Xs]),cartesianT([T|Ts])):- hastype(Gamma,X,T),hastype(Gamma,tuple(Xs),cartesianT(Ts)).

hastype(Gamma,proj(0,tuple([X|Xs])),T):- hastype(Gamma,X,T).
hastype(Gamma,proj(I,tuple([X|Xs])),T):- Z is I-1,hastype(Gamma,proj(Z,tuple(Xs)),T).

hastype(Gamma,ifthen(E0,E1,E2),T):- hastype(Gamma,E0,boolT),hastype(Gamma,E1,T),hastype(Gamma,E2,T).

hastype(Gamma,lambda(variable(X),E1),arrowT(T1,T2)):- hastype([p(variable(X),T1)|Gamma],E1,T2).
hastype(Gamma,call(E1,E2),T2):- hastype(Gamma,E1,arrowT(T1,T2)),hastype(Gamma,E2,T1).

dv(L,def(X,E),M):- append(L,[X],M).
dv(L,seq(D1,D2),M):- dv([],D1,M1),dv([],D2,M2),append(L,M1,M3),append(M3,M2,M).
dv(L,par(D1,D2),M):- dv([],D1,M1),dv([],D2,M2),\+check(M1,M2),append(L,M1,M3),append(M3,M2,M).
dv(L,loc(D1,D2),M):- dv([],D2,M1),append(L,M1,M).

member(X,[]):- fail.
member(X,[X|Xs]):- true.
member(X,[Y|Ys]):- member(X,Ys).

check([],L2):- false.
check([X|Xs],L2):- member(X,L2);check(Xs,L2).

typeElaborates(Gamma,def(variable(X),E),Gamma1):- hastype(Gamma,E,T),append([],[p(variable(X),T)],Gamma1).
typeElaborates(Gamma,seq(D1,D2),Gamma3):- typeElaborates(Gamma,D1,Gamma1),append(Gamma1,Gamma,G1),typeElaborates(G1,D2,Gamma2),append(Gamma2,Gamma1,Gamma3).
typeElaborates(Gamma,par(D1,D2),Gamma3):- dv([],D1,M1),dv([],D2,M2),\+check(M1,M2),typeElaborates(Gamma,D1,Gamma1),typeElaborates(Gamma,D2,Gamma2),append(Gamma1,Gamma2,Gamma3).
typeElaborates(Gamma,loc(D1,D2),Gamma2):- typeElaborates(Gamma,D1,Gamma1),append(Gamma1,Gamma,G1),typeElaborates(G1,D2,Gamma2).
hastype(Gamma,letin(D,E2),T):- typeElaborates(Gamma,D,Gamma1),append(Gamma1,Gamma,G),hastype(G,E2,T).

%% hastype([],const(3),T).
%% hastype([],equ(andd(bl(true),bl(true)),add(const(4),const(5))),boolT).
%% hastype([p(variable("x"),intT),p(variable("y"),intT)],variable("x"),T).
%% hastype([],ifthen(bl(true),const(5),const(10)),T).
%% hastype([],proj(0,tuple([bl(true),const(6)])),T).
%% hastype([],letin(par(def(variable("x"),const(5)),def(variable("x"),const(4))),const(4)),T).
%% hastype([],letin(seq(def(variable("x"),const(5)),def(variable("x"),const(4))),const(4)),T).
%% hastype([],letin(loc(def(variable("x"),const(5)),def(variable("x"),const(4))),const(4)),T).
%% hastype([p(variable("x"),intT)],lambda(variable("x"),bl(true)),arrowT(intT,boolT)).
%% hastype([p(variable("x"),intT)],call(lambda(variable("x"),bl(true)),const(5)),T).

%% typeElaborates([],par(def(variable("x"),const(5)),def(variable("x"),const(4))),T).
%% gamma:- [p(variable("x"),intT),p(variable("y"),intT)].
