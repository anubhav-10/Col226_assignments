open List;;
type exp = Const of int
		| Abs of exp
		| Vars of string
		| Add of exp * exp
		| Subtract of exp * exp
		| Multiply of exp * exp
		| Divide of exp * exp 
		| Mod of exp * exp
		| Exponent of exp * exp 
		| BoolConst of bool 
		| Not of exp 
		| And of exp * exp
		| Or of exp * exp
		| Implies of exp * exp
		| Equal of exp * exp
		| Greater of exp * exp
		| Less of exp * exp
		| GreaterEqual of exp * exp
		| LessEqual of exp * exp 
		| Tuple of exp list
		| Proj of int * exp

		| Assign of exp * exp
		| Seq of exp * exp
		| Par of exp * exp
		| LetD of exp * exp 

		| Letin of string * exp * exp 
		| Ifthen of exp * exp * exp
		| Lambda of exp * exp 
		| Call of exp * exp ;;

type tab = (exp * closure) list
and closure = Clos of exp * tab;;

let rec lookup x t = match t with
		| (e,c)::tl -> if e <> x then lookup x tl
					   else c;;

let add n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(a+b),[]);;

let sub n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(a-b),[]);;

let mult n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(a*b),[]);;

let div n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(a/b),[]);;

let modulo n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(a mod b),[]);;

let rec pow a n = match n with
    	  0 -> 1
    	| 1 -> a
  		| n1 -> let b = pow a (n/2) in
    		b * b * (if n mod 2 = 0 then 1 else a);;

let expo n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(Const(pow a b),[]);;

let absolute n1 = match n1 with
		 (Clos (Const a,t1)) -> Clos(Const(abs a),[]);;

let nott n1 = match n1 with
		 (Clos (BoolConst a,t1)) -> Clos(BoolConst(not a),[]);;

let booland n1 n2 = match (n1,n2) with
		 (Clos (BoolConst a,t1),Clos (BoolConst b,t2)) -> Clos(BoolConst(a && b),[]);;

let boolor n1 n2 = match (n1,n2) with
		 (Clos (BoolConst a,t1),Clos (BoolConst b,t2)) -> Clos(BoolConst(a || b),[]);;

let imply n1 n2 = match (n1,n2) with
		 (Clos (BoolConst a,t1),Clos (BoolConst b,t2)) -> Clos(BoolConst((not a) || b),[]);;

let equal n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(BoolConst(a = b),[]);;

let greater n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(BoolConst(a > b),[]);;

let less n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(BoolConst(a < b),[]);;

let greaterequal n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(BoolConst(a >= b),[]);;

let lessequal n1 n2 = match (n1,n2) with
		 (Clos (Const a,t1),Clos (Const b,t2)) -> Clos(BoolConst(a <= b),[]);;

let ifelse e0 e1 e2 t= match e0 with
		  (Clos (BoolConst b,_)) -> if b then Clos(e1,t) else Clos(e2,t);;

let rec delete x gamma l = match gamma with
		  [] -> l
		| (s,a)::ss -> if (s<>x) then delete x ss l@[(s,a)]
					   else l@ss;;

let rec map1 f t l = match l with
		  [] -> []
		| (x::xs) -> (f (Clos(x,t),[]))::(map1 f t xs);;

let rec map2 l = match l with
		| [] -> []
		| (Clos(e,t)::xs) -> e::(map2 xs);;

let rec krivine (c,s) = match c with
		|  Clos (Const n,t) -> Clos (Const n,t)
		|  Clos (BoolConst b,t) -> Clos (BoolConst b,t)
		|  Clos (Vars v,t) -> krivine (lookup (Vars v) t,s) 
		|  Clos (Abs n,t) -> krivine (absolute c,s)
		|  Clos (Add(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (add a1 a2,s)
		|  Clos (Subtract(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (sub a1 a2,s)
		|  Clos (Multiply(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (mult a1 a2,s)
		|  Clos (Divide(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (div a1 a2,s)
		|  Clos (Mod(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (modulo a1 a2,s)
		|  Clos (Exponent(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (expo a1 a2,s)
		|  Clos (Not b,t) -> krivine (nott c,s)
		|  Clos (And(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (booland a1 a2,s)
		|  Clos (Or(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (boolor a1 a2,s)
		|  Clos (Implies(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (imply a1 a2,s)
		|  Clos (Equal(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (equal a1 a2,s)
		|  Clos (Greater(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (greater a1 a2,s)
		|  Clos (Less(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (less a1 a2,s)
		|  Clos (GreaterEqual(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (greaterequal a1 a2,s)
		|  Clos (LessEqual(e1,e2),t) -> let (a1,a2) = (krivine (Clos(e1,t),[]),krivine (Clos(e2,t),[])) in krivine (lessequal a1 a2,s)
		|  Clos (Tuple(e),t) -> (Clos(Tuple(map2(map1 krivine t e)),t))
		|  Clos (Proj(i,e),t) -> let res = krivine (Clos(e,t),[]) in 
										(match res with
										| (Clos(Tuple(e),t)) -> Clos(List.nth e i,t))

		|  Clos (Letin(x,e1,e2),t) -> let res = (krivine (Clos(e1,t),[])) in let ans = krivine (Clos(e2,(Vars x,res)::t),s) in
										(match ans with
										| Clos(e,t) -> krivine (Clos(e,delete (Vars x) t []),s) )

		|  Clos (Assign(x,e0),t) -> let res = (krivine (Clos(e0,t),[])) in Clos(Const(0),(x,res)::t)
		
		|  Clos (LetD(e0,e1),t) -> let res = (krivine (Clos(e0,t),[])) in 
										(match res with
											Clos(_,t1) -> (krivine (Clos(e1,t1),[])) )

		|  Clos (Ifthen(e0,e1,e2),t) -> let b = (krivine (Clos(e0,t),[])) in krivine(ifelse b e1 e2 t,s) 
		|  Clos (Lambda(x,e),t) -> let aux c s = (match (c,s) with
										(Clos(Lambda(x,e),t),cl::s) -> (Clos(e,(x,cl)::t),s) ) in
										let (a,b) = aux c s in
										krivine (a,b)
		|  Clos (Call(e1,e2),t) -> krivine (Clos(e1,t),Clos(e2,t)::s)
		;;

let gamma = [];;
let a = Clos(Add (Const(4),(Const(5))),[]);;
let b = Clos(Call(Lambda(Vars "x",Add(Vars "x",Const(9))),Const(5)),[]);;