open String;;

exception Empty;;
exception AtLast;;
exception AtFirst;;
exception TooShort;;
(*I assumed alphabet to be a given data type and for example, I considered it as below*)
type alphabet = A of string;;
type astar = {mutable marker: int ; mutable l : alphabet array};;

let lgh a = Array.length a.l;;

let nonempty a = 
	let b = lgh a in
		if b = 0 then false 
		else true;;

let concat s1 s2 = {marker = s1.marker ; l = Array.append s1.l s2.l};;
(*let lgh s1 = l1 and lgh s2 = l2 then
(concat s1 s2) will contain all letters in s1 and s2,
therefore lgh (concat s1 s1) = lgh(s1) + lgh(s2) *)

let reverse a = 
	let b = Array.copy a.l in
	let n = lgh a in
		for i = 0 to n-1 do
			a.l.(i) <- b.(n-i-1)
		done;;
(*let lgh(s) = n 
creation of a copy array of size n will take O(n)(let it be k*n where k is constant) time and set operation in array takes constant time (say k1)
then the total time for setting all elements will be k1*n as the for loop run n time
therefore, total time = k*n + k1*n which is O(n)*)
let first a = 
	if nonempty a = false then
		raise Empty
	else a.l.(0);;

let last a = 
	if nonempty a = false then
		raise Empty
	else a.l.(lgh(a)-1);; 

let create s = 
	let rec exp i l = 
		if i < 0 then l else exp (i-1) (Array.append [|A (String.make 1 s.[i])|] l) in
	{marker =0 ; l=exp (String.length s - 1) [||]};;

let forward a = 
	let b = lgh a in 
		if a.marker = b - 1 then
			raise AtLast
		else a.marker <- a.marker +1;;
(*Time complexity is O(1) as we are just adding 1 to marker *)
let back a = 
	if a.marker = 0 then
		raise AtFirst
	else a.marker <- a.marker -1;;
(*Time complexity is O(1) as we are just subtracting adding 1 to marker *)
let moveTo n a = 
	let b = lgh a in 
		if n >= b then 
			raise TooShort
		else a.marker <- n;;
(*Time complexity is O(1) as we are just changing value of marker to given n 
and we can also write it as O(n) because Big-Oh is the upper bound time complexity and O(1) is less than O(n)*)
let replace s w = s.l.(s.marker) <- (A w);;
(*Time complexity is O(1) as we can access any element of array in O(1) time.
and we can also write it as O(n) because Big-Oh is the upper bound time complexity and O(1) is less than O(n)*)


(*let create s = 
	let s1 = {marker = 0 ; l = [||]} in
	for i = 0 to String.length s - 1 do
		concat s1 {marker = 0 ; l = [|A (String.make 1 s.[i])|]} 
	done ;;*)
(*let create s = 
	let s1 = {marker = 0 ; l = [||]} in
	for i = 0 to String.length s - 1 do
		Array.append s1.l [|A (String.make 1 s.[i])|]
	done ;;*)