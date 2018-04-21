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
		| Proj of int * exp;;

type answer = N of int | B of bool | A of answer list;;

let add n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (e1+e2);;

let sub n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (e1-e2);;

let mult n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (e1*e2);;

let div n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (e1/e2);;

let modulo n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (e1 mod e2);;

let rec pow a n = match n with
		 0 -> 1
    	| 1 -> a
  		| n1 -> let b = pow a (n/2) in
    		b * b * (if n mod 2 = 0 then 1 else a);;

let expo n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> N (pow e1 e2);;

let absolute n1 = match n1 with
		 (N e2) -> N (abs e2);;

let nott n = match n with
		 (B n1) -> B (not n1);;

let booland n1 n2 = match (n1,n2) with
		 (B e1,B e2) -> B (e1 && e2);;

let boolor n1 n2 = match (n1,n2) with
		 (B e1,B e2) -> B (e1 || e2);;

let imply n1 n2 = match (n1,n2) with
		 (B e1, B e2) -> B ((not e1) || e2);;

let equal n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> B (e1 = e2);;

let greater n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> B (e1 > e2);;

let less n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> B (e1 < e2);;

let greaterequal n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> B (e1 >= e2);;

let lessequal n1 n2 = match (n1,n2) with
		 (N e1,N e2) -> B (e1 <= e2);;

let rho x = match x with
		_ -> N 5;;

let rec eval e = match e with 
		  Const n -> N n
		| BoolConst b -> B b
		| Vars s -> rho s
		| Tuple t -> A (List.map eval t)
		| Abs e1 -> absolute (eval e1)
		| Add (e1,e2) -> add (eval e1) (eval e2)
		| Subtract (e1,e2) -> sub (eval e1) (eval e2)
		| Multiply (e1,e2) -> mult (eval e1) (eval e2)
		| Divide (e1,e2) -> div (eval e1) (eval e2)
		| Mod (e1,e2) -> modulo (eval e1) (eval e2)
		| Exponent (e1,e2) -> expo (eval e1) (eval e2)
		| Not e1 -> nott (eval e1)
		| And (e1,e2) -> booland (eval e1) (eval e2)
		| Or (e1,e2) -> boolor (eval e1) (eval e2)
		| Implies (e1,e2) -> imply (eval e1) (eval e2)
		| Equal (e1,e2) -> equal (eval e1) (eval e2)
		| Greater (e1,e2) -> greater (eval e1) (eval e2)
		| Less (e1,e2) -> less (eval e1) (eval e2)
		| GreaterEqual (e1,e2) -> greaterequal (eval e1) (eval e2)
		| LessEqual (e1,e2) -> lessequal (eval e1) (eval e2)
		| Proj (i,e1) -> match e1 with 
				(Tuple a) -> eval (List.nth a i);;

type opcode = CONST of int
		| ABS
		| VARS of string
		| ADD
		| SUBTRACT
		| MULTIPLY
		| DIVIDE
		| MOD
		| EXPONENT
		| BOOLCONST of bool 
		| NOT
		| AND
		| OR
		| IMPLIES
		| EQUAL
		| GREATER
		| LESS
		| GREATEREQUAL
		| LESSEQUAL
		| TUPLE of opcode list list
		| PROJ ;;

let rec compile e = match e with
		 Const n -> [CONST n]
		| Abs e1 -> (compile e1) @ [ABS]
		| Vars s -> [VARS s]
		| Add (e1,e2) -> (compile e1) @ (compile e2) @ [ADD]
		| Subtract (e1,e2) -> (compile e1) @ (compile e2) @ [SUBTRACT]
		| Multiply (e1,e2) -> (compile e1) @ (compile e2) @ [MULTIPLY]
		| Divide (e1,e2) -> (compile e1) @ (compile e2) @ [DIVIDE]
		| Mod (e1,e2) -> (compile e1) @ (compile e2) @ [MOD]
		| Exponent (e1,e2) -> (compile e1) @ (compile e2) @ [EXPONENT]
		| BoolConst b -> [BOOLCONST b]
		| Not b -> (compile b) @ [NOT]
		| And (e1,e2) -> (compile e1) @ (compile e2) @ [AND]
		| Or (e1,e2) -> (compile e1) @ (compile e2) @ [OR]
		| Implies (e1,e2) -> (compile e1) @ (compile e2) @ [IMPLIES]
		| Equal (e1,e2) -> (compile e1) @ (compile e2) @ [EQUAL]
		| Greater (e1,e2) -> (compile e1) @ (compile e2) @ [GREATER]
		| Less (e1,e2) -> (compile e1) @ (compile e2) @ [LESS]
		| GreaterEqual (e1,e2) -> (compile e1) @ (compile e2) @ [GREATEREQUAL]
		| LessEqual (e1,e2) -> (compile e1) @ (compile e2) @ [LESSEQUAL]
		| Tuple t -> [TUPLE (List.map compile t)]
		| Proj (i,e) -> [CONST i] @ (compile e) @ [PROJ];;

open List;; 

let rec map f l = match l with
		| (x::xs) -> (f ([],rho,x))::(map f xs)
		| [] -> [];;

let rec execute (s,rho,c) = match (s,c) with
		 (s,[]) -> hd s
		| (s,CONST(n)::c) -> execute ((N n)::s,rho,c)
		| (n::s,ABS::c) -> execute ((absolute n)::s,rho,c)
		| (s,VARS(n)::c) -> execute ((rho n)::s,rho,c)
		| (s,TUPLE(t)::c) -> execute (A (map execute t)::s,rho,c)
		| (n1::n2::s,ADD::c)-> execute ((add n2 n1)::s,rho,c)
		| (n1::n2::s,SUBTRACT::c)-> execute ((sub n2 n1)::s,rho,c)
		| (n1::n2::s,MULTIPLY::c)-> execute ((mult n2 n1)::s,rho,c)
		| (n1::n2::s,DIVIDE::c)-> execute ((div n2 n1)::s,rho,c)
		| (n1::n2::s,MULTIPLY::c)-> execute ((mult n2 n1)::s,rho,c)
		| (n1::n2::s,MOD::c)-> execute ((modulo n2 n1)::s,rho,c)
		| (n1::n2::s,EXPONENT::c)-> execute ((expo n2 n1)::s,rho,c)
		| (s,BOOLCONST(b)::c) -> execute ((B b)::s,rho,c)
		| (n::s,NOT::c) -> execute ((nott n)::s,rho,c)
		| (n1::n2::s,AND::c)-> execute ((booland n2 n1)::s,rho,c)
		| (n1::n2::s,OR::c)-> execute ((boolor n2 n1)::s,rho,c)
		| (n1::n2::s,IMPLIES::c)-> execute ((imply n2 n1)::s,rho,c)
		| (n1::n2::s,EQUAL::c)-> execute ((equal n2 n1)::s,rho,c)
		| (n1::n2::s,GREATER::c)-> execute ((greater n2 n1)::s,rho,c)
		| (n1::n2::s,LESS::c)-> execute ((less n2 n1)::s,rho,c)
		| (n1::n2::s,GREATEREQUAL::c)-> execute ((greaterequal n2 n1)::s,rho,c)
		| (n1::n2::s,LESSEQUAL::c)-> execute ((lessequal n2 n1)::s,rho,c)
		| ((A e1)::(N i)::s,PROJ::c) -> execute ((List.nth e1 i)::s,rho,c);;

let a = Tuple [Add (Const 3,Const 10); Subtract (Vars "five",Const 10); Multiply (Const 100,Const 5); Divide (Const 50,Const 10); Exponent (Const 2,Const 10); Mod (Const 40,Const 3)];;
eval a;;
let b = compile a;;
let c = execute ([],rho,b);;

let d = Proj (3,a);;
eval d;;
let e = compile d;;
let f = execute ([],rho,e);;

let g = Not (And (Or (BoolConst true,BoolConst false),Implies (BoolConst true,BoolConst false)));;
eval g;;
let h = compile g;;
let i = execute ([],rho,h);;