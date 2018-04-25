type symbol = Sym of string;;
type term = Const of string | Vars of string | Node of (symbol * (term list));;
type atom = symbol * (term list);;
type goal = atom list;;
type head = atom;;
type body = atom list;;
type clause = Fact of head | Rule of head * body;;
type program = clause list;;

(* util functions *)
let rec map f l = match l with
			  [] -> []
			| (x::xs) -> (f x)::(map f xs);;
let rec map2 l = match l with
	| [] -> []
	| (x::xs) -> (Node x)::(map2 xs);;
let rec map3 l = match l with
	| [] -> []
	| ((Node x)::xs) -> (x)::(map3 xs);;
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

let rec print l = match l with
	| [] -> Printf.printf "\n\n"		
	| (a,Vars(b))::xs -> Printf.printf "%s = %s\n" a b;print xs
	| (a,Const(b))::xs -> Printf.printf "%s = %s\n" a b;print xs;; 

(* let rec solve_body l =  *)
exception FAIL;;
let rec solve orig_prog curr_prog goal stack ans final_ans= match (curr_prog,goal,stack) with
	| (_,[],[]) -> print ans
	| ([],x,[]) -> raise FAIL
	| ([],_,(g,prog,sigma)::ss) -> solve orig_prog prog g ss sigma final_ans
	| (_,[],(g,prog,sigma)::ss) -> print ans;solve orig_prog prog g ss sigma (final_ans@sigma)
	| (Fact(y)::ys,x::xs,s)-> 
					(try
						let l = compose ans (mgu (Node y) (subst ans (Node x)) []) in
							solve orig_prog orig_prog xs ((x::xs,ys,ans)::s) l final_ans
(* 						(let l = mgu (Node y) (subst ans (Node x)) [] in 
						if (l = []) then (Printf.printf "true")
						else print l;if (ys = []) then solve orig_prog orig_prog xs stack (compose ans l) final_ans else solve orig_prog ys goal stack (compose ans l) final_ans)  *)
					with _ -> (* if (ys = []) then solve orig_prog orig_prog xs stack ans final_ans else solve orig_prog ys goal stack ans final_ans) *)
						solve orig_prog ys (x::xs) s ans final_ans)

	| (Rule(h,b)::ys,x::xs,s) ->
					(try 
						(let l = compose ans (mgu (Node h) (subst ans (Node x)) []) in
						(* solve orig_prog orig_prog (map3 ((map (subst l) (map2 b)))@goal) ((goal,curr_prog,ans)::s) l) *)
						solve orig_prog orig_prog b ((x::xs,curr_prog,ans)::s) l final_ans)
					with _ -> solve orig_prog ys (x::xs) s ans final_ans)

;;

let c = [Fact(Sym "male",[Const "franc"]);Fact(Sym "male",[Const "marko"]);Fact(Sym "female",[Const "jozefa"]);Fact(Sym "child",[Const "marko";Const "franc"]);Fact(Sym "child",[Const "marko";Const "jozefa"]);Rule((Sym "son",[Vars "x";Vars "y"]),[(Sym "male",[Vars "x"]);(Sym "child",[Vars "x";Vars "y"])])];;
let g = [Sym "son",[Const "marko";Vars "Z"]];;
(* 
let c = [Fact(Sym "male",[Const "Ned"]);Fact(Sym "male",[Const "Jon"])];;
let g = [Sym "male",[Vars "x"]];; *)