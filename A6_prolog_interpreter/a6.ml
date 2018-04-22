type symbol = Sym of string;;
type term = Const of string | Vars of string | Node of (symbol * (term list));;
type atom = symbol * (term list);;
type goal = atom list;;
type head = Head of atom;;
type body = Body of atom list;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;

(* util functions *)
let rec map f l = match l with
			  [] -> []
			| (x::xs) -> (f x)::(map f xs);;
let rec foldl f e l = match l with
			  [] -> e
			| x::xs -> foldl f (f(x,e)) xs;;

(* find all variables  *)
let con (a,b) = a@b;;

let rec vars t = match t with
			  Vars x -> [x]
			| Node (x,y) -> foldl con [] (map vars y);;

(* substitution *)
let rec isKey v s = match s with
			  [] -> false
			| (x,y)::tl -> if v=x then true else isKey v tl;;
let rec find v s = match s with
			(x,y)::tl -> if v=x then y else find v tl;;
let rec subst sigma t = match t with 
	| Const x -> Const x
	| Vars x -> if (isKey x sigma) then (find x sigma) else Vars x 
	| Node (x,y) -> Node(x,map (subst sigma) y);;

(* mgu *)
exception NOT_UNIFIABLE;;
let rec isPresent x l = match l with
			  [] -> false
			| (y::ys) -> if(x=y) then true else isPresent x ys;;
let rec replace a l = match (a,l) with
			  (a,(b,c)::t) -> if (a=b) then c else replace a t;;
let rec contain a l = match (a,l) with
			  (_,[]) -> false
			| (a,(b,c)::t) -> if (a=b) then true else contain a t;;
let rec compose l1 l2 = match l1 with 
	| [] -> l2
	| ((a,b)::xs) -> 
		if (contain a l2) then (a,replace a l2)::xs 
		else (a,b)::(compose xs l2);;
		
let rec mgu_util l1 l2 l f = match (l1,l2) with
	| ([],[]) -> l
	| (x::xs,y::ys) -> mgu_util xs ys (compose (f (subst l x) (subst l y) l) l) f;;
let rec mgu t1 t2 l = match (t1,t2) with
	| (Vars x,Vars y) -> if (x=y) then l else l@[(x,Vars y)]
	| (Const x,Const y) -> if (x=y) then l else raise NOT_UNIFIABLE
	| (Const x, Vars y) | (Vars y, Const x) -> l@[y,Const x]
	| (Vars u,Node(s,t_lis1)) | (Node(s,t_lis1),Vars u) ->
		if(isPresent u (vars (Node (s,t_lis1)))) then raise NOT_UNIFIABLE
		else l@[u,Node(s,t_lis1)]
	| (Node(u,v), Node(x,y)) -> 
		if(u<>x) then raise NOT_UNIFIABLE
		else mgu_util v y [] mgu;;

let rec solve prog goal = match prog with 
	| Fact (Head a) -> mgu (Node goal) (Node a) [] 
;;


let prog = Fact(Head(Sym "a",[Const "b"]));;
let gl = (Sym "a",[Const "b"]);;