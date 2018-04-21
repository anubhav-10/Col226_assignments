open List;;
type variable = Vars of string;;
type symbol = Sym of string;;
type term = V of variable | Node of symbol * (term list);;

let t1 = [(Sym "plus",2);(Sym "int",0);(Sym "bool",0);(Sym "minus",2);(Sym "not",1)];;
let t2 = [(Sym "plus",2);(Sym "int",0);(Sym "bool",0);(Sym "minus",2);(Sym "minus",1)];;

let rec foldl f e l = match l with
			  [] -> e
			| x::xs -> foldl f (f(x,e)) xs;;

let rec filter f l = match l with
			  [] -> []
			| x::xs -> if (f x) then x::(filter f xs)
			 		  else filter f xs;;

let rec map f l = match l with
			  [] -> []
			| (x::xs) -> (f x)::(map f xs);;

let isZero t = 	match t with 
			  (_,a) -> if a = 0 then true else false;;

let isNeg t = 	match t with 
			  (_,a) -> if a < 0 then true else false;;

let rec isRepeat t l = match (t,l) with
			  ((s,_),[]) -> false
			| ((s,_),x::xs) -> (match x with
							(y,_) -> if (s=y) then true else isRepeat t xs);;

(* t = signature and s = symbol *)
let rec arity t s = match (t,s) with
			  (x::xs,a) -> (match x with
			  				(b,c) -> if (a=b) then c else arity xs s);;

let rec isPresent x l = match l with
			  [] -> false
			| (y::ys) -> if(x=y) then true else isPresent x ys;;

let andd (a,b) = a && b;;
let orr (a,b) = a || b;;
let snd (a,b) = b;;
let max (a,b) = if a>b then a else b;;
let add (a,b) = a+b;;
let con (a,b) = a@b;;

let rec repeat t = match t with
			  [] -> true
			| (x::xs) -> if (isRepeat x xs) then false
			 			else repeat xs;;

let check_sig t = (foldl orr false (List.map isZero t1)) && (not(foldl orr false (List.map isNeg t1))) && (repeat t);;
check_sig t1;;
check_sig t2;;

let t = Node(Sym "plus",[Node (Sym "not",[V (Vars "x")]);V (Vars "y")]);;
let te1 = Node (Sym "not",[V (Vars "x");V (Vars "y")]);;

let rec wfterm t = match t with
			  V x -> true
			| Node (x,y) -> if (arity t1 x)=length(y) then foldl andd true (map wfterm y)
							 else false;;
(* changed *)
(* let rec wfterm t sig = match t with
			  V x -> true
			| Node (x,y) -> if (arity sig x)=length(y) then foldl andd true (map wfterm y)
							 else false;;
 *)
wfterm t;;
wfterm te1;;

let rec ht t = match t with
			  V x -> 0
			| Node (x,y) -> 1+foldl max (-1) (map ht y);;

let rec size t = match t with
			  V x -> 0
			| Node (x,y) -> 1+foldl add 0 (map size y);;
let rec duplicate l = match l with 
			  [] -> []
			| (x::xs) -> if (isPresent x xs) then (duplicate xs) else [x]@(duplicate xs);;
let rec vars_util t = match t with
			  V x -> [x]
			| Node (x,y) -> foldl con [] (map vars_util y);;
let vars t = duplicate (vars_util t);;

type substitution = (variable * term) list;; 
let s1 = [Vars "x",V (Vars "x");Vars "y",V (Vars "y")];;

let rec isKey v s = match s with
			  [] -> false
			| (x,y)::tl -> if v=x then true else isKey v tl;;
let rec find v s = match s with
			(x,y)::tl -> if v=x then y else find v tl;;

let rec subst sigma t = match t with 
			  V x -> if (isKey x sigma) then (find x sigma) else V x 
			| Node (x,[]) -> Node (x,[])
			| Node (x,y) -> Node(x,map (subst sigma) y);;
			
let sigma1 = [(Vars "x",Node (Sym "a",[]))];;
let s1 = Node (Sym "f",[Node (Sym "h",[Node (Sym "a",[]);V (Vars "y")]);V (Vars "x")]);;
let s2 = Node (Sym "f",[V (Vars "x");Node (Sym "h",[Node (Sym "b",[]);V (Vars "y")])]);;
subst sigma1 s1;;

exception NOT_UNIFIABLE;;
let rec replace a l = match (a,l) with
			  (a,(b,c)::t) -> if (a=b) then c else replace a t;;
let rec contain a l = match (a,l) with
			  (_,[]) -> false
			| (a,(b,c)::t) -> if (a=b) then true else contain a t;;
let rec compose l1 l2 = match l1 with
			  [] -> l2
			| ((a,b)::xs) -> if (contain a l2) then (a,replace a l2)::xs else (a,b)::(compose xs l2);;
let rec mgu_util l1 l2 l f= match (l1,l2) with
			  ([],[]) -> l
			| (x::xs,y::ys) -> mgu_util xs ys (compose (f (subst l x) (subst l y) l) l) f;;
let rec mgu t1 t2 l= match (t1,t2) with
			  (V x,V y) -> if (x=y) then l else l@[(x,V y)]
			| (V u,Node (x,[])) | (Node(x,[]), V u)-> l@[(u,Node (x,[]))]
			| (V u,Node (x,y)) | (Node(x,y), V u)-> if(isPresent u (vars (Node (x,y)))) then raise NOT_UNIFIABLE
								  else l@[(u,Node (x,y))]
			| (Node (u,[]),Node (x,[])) -> if (u=x) then l else raise NOT_UNIFIABLE
			| (Node (u,[]),Node (x,y)) -> raise NOT_UNIFIABLE
			| (Node (u,v),Node (x,y)) -> if (u<>x) then raise NOT_UNIFIABLE
									     else mgu_util v y [] mgu ;;
(* let rec mgu t1 t2 l= match (t1,t2) with
			  (V x,V y) -> if (x=y) then l else l@[(x,V y)]
			| (V u,Node (x,[])) -> l@[(u,Node (x,[]))]
			| (V u,Node (x,y)) -> if(isPresent u (vars (Node (x,y)))) then raise NOT_UNIFIABLE
								  else l@[(u,Node (x,y))]
			| (Node (u,[]),Node (x,[])) -> if (u=x) then l else raise NOT_UNIFIABLE
			| (Node (u,[]),Node (x,y)) -> raise NOT_UNIFIABLE
			| (Node (u,v),Node (x,y)) -> if (u<>x) then raise NOT_UNIFIABLE
									     else begin 	
									     let rec unify s0 l1 l2 = match (l1,l2) with
									     		([],[]) -> l
									     	| 	(x::xs,y::ys) -> let s1 = mgu (subst s0 x) (subst s0 y) [] in
									     						 unify (compose s1 s0) xs ys in 
									      	unify [] v y
										end ;;
 *)
let sigma = [Vars "x",(Node(Sym "f",[Node (Sym "a",[])]));Vars "y",(Node (Sym "g",[Node (Sym "b",[]);V(Vars "z")]));Vars "z",V (Vars "x")];;
let rho = [Vars "x",(Node (Sym "w",[]));Vars "y",(Node (Sym "h",[V (Vars "z")]));Vars "z",Node (Sym "a",[])];;
let p =Node(Sym "P",[V (Vars "x");V (Vars "y");V (Vars "z")]);;
let term1 = Node (Sym "P",[Node (Sym "a",[]);V (Vars "X");Node (Sym "f",[Node (Sym "g",[V (Vars "y")])])]);;
let term2 = Node (Sym "P",[V (Vars "z");Node (Sym "f",[V (Vars "z")]);Node (Sym "f",[V (Vars "w")])]);;
let term3 = Node (Sym "f",[V (Vars "x");V (Vars "x")]);;
let term4 = Node (Sym "f",[Node (Sym "2",[]);Node (Sym "3",[])]);;

let a = V (Vars "z");;
let b = Node (Sym "a",[]);;
let c = Node (Sym "f",[V (Vars "z")]);;
let d = V (Vars "X");;
subst sigma p;;
subst rho p;;
compose sigma rho;;
mgu term1 term2 [];;
mgu term3 term4 [];;
ht p;;
ht term1;;
ht term2;;
size term1;;
size term2;;
vars term1;;
vars term2;;
