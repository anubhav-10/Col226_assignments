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

		| Assign of string * exp
		| Seq of exp * exp
		| Par of exp * exp
		| Loc of exp * exp
		| LetD of exp * exp

		| Letin of string * exp * exp 
		| Ifthen of exp * exp * exp
		| Lambda of string * exp 
		| Call of exp * exp ;;

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
		| PROJ

		| SEQ 
		| PAR 
		| ASSIGN 
		| LETD 

		| COND of (opcode list) * (opcode list)
		| BIND of string
		| UNBIND of string
		| CLOS of string * opcode list
		| RET
		| APP ;;  

type table = (string * answer) list
and answer = N of int | B of bool | Vclosure of table * string * (opcode list) | A of answer list;;

let rec find gamma x = match gamma with
		   	y::ys -> if (x=(fst y)) then (snd y)  else find ys x;; 

exception Abd;;
let rec compile e = match e with
		|  Const n -> [CONST n]
		|  Vars s -> [VARS s]
		|  Abs e1 -> (compile e1) @ [ABS]
		|  Add (e1,e2) -> (compile e1) @ (compile e2) @ [ADD]
		|  Subtract (e1,e2) -> (compile e1) @ (compile e2) @ [SUBTRACT]
		|  Multiply (e1,e2) -> (compile e1) @ (compile e2) @ [MULTIPLY]
		|  Divide (e1,e2) -> (compile e1) @ (compile e2) @ [DIVIDE]
		|  Mod (e1,e2) -> (compile e1) @ (compile e2) @ [MOD]
		|  Exponent (e1,e2) -> (compile e1) @ (compile e2) @ [EXPONENT]
		|  BoolConst b -> [BOOLCONST b]
		|  Not b -> (compile b) @ [NOT]
		|  And (e1,e2) -> (compile e1) @ (compile e2) @ [AND]
		|  Or (e1,e2) -> (compile e1) @ (compile e2) @ [OR]
		|  Implies (e1,e2) -> (compile e1) @ (compile e2) @ [IMPLIES]
		|  Equal (e1,e2) -> (compile e1) @ (compile e2) @ [EQUAL]
		|  Greater (e1,e2) -> (compile e1) @ (compile e2) @ [GREATER]
		|  Less (e1,e2) -> (compile e1) @ (compile e2) @ [LESS]
		|  GreaterEqual (e1,e2) -> (compile e1) @ (compile e2) @ [GREATEREQUAL]
		|  LessEqual (e1,e2) -> (compile e1) @ (compile e2) @ [LESSEQUAL]
		|  Tuple t -> [TUPLE (List.map compile t)]
		|  Proj (i,e) -> [CONST i] @ (compile e) @ [PROJ]

		|  Ifthen (e0,e1,e2) -> compile(e0)@[COND(compile(e1),compile(e2))]
		|  Letin (x,e1,e2) -> compile(e1)@[BIND(x)]@compile(e2)@[UNBIND(x)]

		|  Assign (x,e1) -> compile(e1)@[BIND(x)]
		|  Seq (e0,e1) -> compile(e0)@compile(e1)@[SEQ]
		|  Par (e0,e1) -> compile(e0)@compile(e1)@[PAR]
		|  LetD (e0,e1) -> compile(e0)@compile(e1)@[LETD]

		|  Lambda (x,e1) -> [CLOS (x,compile(e1)@[RET])]
		|  Call (e1,e2) -> compile(e1)@compile(e2)@[APP] 
		|  _ -> raise Abd;;

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

let rec delete x gamma l = match gamma with
		  [] -> l
		| (s,a)::ss -> if (s<>x) then delete x ss l@[(s,a)]
					   else l@ss;;

let rec map f l gamma d= match l with
		| (x::xs) -> (f ([],gamma,x,d))::(map f xs gamma d) 
		| [] -> [];;

let rec secd (s,gamma,c,d) = match (s,gamma,c,d) with
		|  (s,gamma,[],d) -> hd s
		|  (s,gamma,CONST(n)::c,d) -> secd ((N n)::s,gamma,c,d)
		|  (s,gamma,BOOLCONST(n)::c,d) -> secd ((B n)::s,gamma,c,d)
		|  (s,gamma,VARS(x)::c,d) -> secd ((find gamma x)::s,gamma,c,d)
		|  (n::s,gamma,ABS::c,d) -> secd ((absolute n)::s,gamma,c,d)
		|  (n1::n2::s,gamma,ADD::c,d) -> secd ((add n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,SUBTRACT::c,d) -> secd ((sub n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,MULTIPLY::c,d) -> secd ((mult n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,DIVIDE::c,d) -> secd ((div n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,MOD::c,d) -> secd ((modulo n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,EXPONENT::c,d) -> secd ((expo n2 n1)::s,gamma,c,d)
		|  (s,gamma,BOOLCONST(b)::c,d) -> secd ((B b)::s,gamma,c,d)
		|  (n::s,gamma,NOT::c,d) -> secd ((nott n)::s,gamma,c,d)
		|  (n1::n2::s,gamma,AND::c,d) -> secd ((booland n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,OR::c,d) -> secd ((boolor n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,IMPLIES::c,d) -> secd ((imply n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,EQUAL::c,d) -> secd ((equal n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,GREATER::c,d) -> secd ((greater n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,LESS::c,d) -> secd ((less n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,GREATEREQUAL::c,d) -> secd ((greaterequal n2 n1)::s,gamma,c,d)
		|  (n1::n2::s,gamma,LESSEQUAL::c,d) -> secd ((lessequal n2 n1)::s,gamma,c,d)
		|  (s,gammma,TUPLE(t)::c,d) -> secd (A (map secd t gamma d)::s,gamma,c,d)
		|  ((A e1)::(N i)::s,gamma,PROJ::c,d) -> secd ((List.nth e1 i)::s,gamma,c,d)

		|  (b::s,gamma,COND(c1,c2)::c3,d) -> if(b=(B true)) then secd (s,gamma,c1@c3,d)
											 else secd (s,gamma,c2@c3,d)
		|  (a::s,gamma,BIND(x)::c1,d) -> secd (s,(x,a)::gamma,c1,d)
		|  (s,gamma,UNBIND(x)::c1,d) -> secd (s,delete x gamma [],c1,d)

		|  (s,gamma,SEQ::c,d) -> secd (s,gamma,c,d)
		|  (s,gamma,PAR::c,d) -> secd (s,gamma,c,d)

		|  (s,gamma,LETD::c,d) -> secd (s,gamma,c,d)

		|  (s,gamma,CLOS(y,c)::c1,d) -> secd ((Vclosure (gamma,y,c))::s,gamma,c1,d)
		|  (a2::Vclosure(gamma1,y,c)::s,gamma,APP::c1,d) -> secd ([],(y,a2)::gamma1,c,(s,gamma,c1)::d)
		|  (a::s1,gamma2,RET::c2,(s,gamma,c1)::d) -> secd (a::s,gamma,c1,d) ;;

let b = Call(Lambda("x",Add(Vars "x",Const(9))),Const(5));;
let c = compile b;;
secd ([],[],c,[]);;
(* ----------------------------------------------------------------------------------------------------------- *)