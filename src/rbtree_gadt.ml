type red = R
type black = B
type db = DB
type nb = NB

type z = Z
type 'a s = S of 'a

type  (_,_,'a) node =
| Nil : (black, z, 'a) node
| BNode : (_, 'n, 'a) node * 'a * (_, 'n, 'a) node -> (black, 'n s, 'a) node
| RNode : (black, 'n, 'a) node * 'a * (black, 'n, 'a) node -> (red, 'n, 'a) node

type 'a tree = Tree : (black, _, 'a) node -> 'a tree

type ('n, 'a) in_res =
| Red : (red, 'n, 'a) node -> ('n, 'a) in_res
| Black : (black, 'n, 'a) node -> ('n, 'a) in_res
(* Invalid_ represent what would be a red-red node that needs to be pushed to the 
	parent/grandparent for rebalancing *)
| InvalidL : (red, 'n, 'a) node * 'a * (black, 'n, 'a) node -> ('n, 'a) in_res		
| InvalidR : (black, 'n, 'a) node * 'a * (red, 'n, 'a) node -> ('n, 'a) in_res

let empty : 'a tree = Tree Nil

let print : type a. (a -> string) -> a tree -> unit = fun f (Tree t) ->
	let s i = String.make i ' ' in
	let rec aux : type c n. int -> (c, n, a) node -> string = fun x n -> match n with 
	| RNode(l, v, r) -> (s x) ^ "R " ^ (f v) ^ "\n" ^ aux (x + 1) r ^ aux (x + 1) l
	| BNode(l, v, r) -> (s x) ^ "B " ^ (f v) ^ "\n" ^ aux (x + 1) r ^ aux (x + 1) l
	| Nil -> ""
in print_string ("\n" ^ (aux 0 t))


let insert_alt : type a. a tree -> a -> a tree = fun (Tree t) v ->
	let blackify : type n. (red, n, a) node -> (black, n s, a) node = 
		fun (RNode(l, v, r)) -> BNode(l, v, r) in
	let rec aux : type c n. (c, n, a) node -> (n, a) in_res = fun (n : (c, n, a) node) ->
	match n with
	| BNode(a, x, b) when v < x-> begin match aux a with
		| Red(n) -> BNode(n, x, b)
		| Black(n) -> BNode(n, x, b)
		| InvalidL(l, v', r) -> 
		| InvalidR(l, v', RNode(rl, rv, rr)) -> 
		end
	| BNode(a, x, b) when v > x-> begin
		
		end
	| RNode(a, x, b) when v < x -> begin
		
		end
	| RNode(a, x, b) when v > x -> begin
		
		end
	| Nil -> Red(RNode(Nil, v, Nil))
	| x -> x
	in match aux t with
	| Red(RNode(l, v, r)) -> Tree(BNode(l, v, r))
	| Black(n) -> Tree n
	| InvalidL(l, v, r) -> Tree(BNode(l, v, r))
	| InvalidR(l, v, r) -> Tree(BNode(l, v, r))




let insert : type a. a tree -> a -> a tree = fun (Tree t) v -> 
	let blackify : type n. (red, n, a) node -> (black, n s, a) node = 
		fun (RNode(l, v, r)) -> BNode(l, v, r) in
	let rec aux : type c n. (c, n, a) node -> (n, a) in_res = fun (n : (c, n, a) node) ->
	match n with
 	| BNode(RNode(n, y, c), z, d) when v < y -> begin match aux n with
		| Red(RNode(a, x, b)) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		| Black n -> Black(BNode(RNode(n, y, c), z, d))
		| InvalidL(l, v, r) -> Red(RNode(BNode(l, v, r), y, BNode(c, z, d)))
		| InvalidR(l, v, r) -> Red(RNode(BNode(l, v, r), y, BNode(c, z, d)))
		end
	| BNode(RNode(a, x, n), z, d) when x < v && v < z-> begin match aux n with
		| Red(RNode(b, y, c)) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		| Black n -> Black(BNode(RNode(a, x, n), z, d))
		| InvalidL(l, v, r) -> Red(RNode(BNode(a, x, l), v, BNode(r, z, d)))
		| InvalidR(l, v, r) -> Red(RNode(BNode(a, x, l), v, BNode(r, z, d)))
		end
	| BNode(a, x, RNode(n, z, d)) when x < v && v < z -> begin match aux n with
		| Red(RNode(b, y, c)) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		| Black n -> Black(BNode(a, x, RNode(n, z, d)))
		| InvalidL(l, v, r) -> Red(RNode(BNode(a, x, l), v, BNode(r, z, d)))
		| InvalidR(l, v, r) -> Red(RNode(BNode(a, x, l), v, BNode(r, z, d)))
		end
	| BNode(a, x, RNode(b, y, n)) when y < v -> begin match aux n with
		| Red(RNode(c, z, d)) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		| Black n -> Black(BNode(a, x, RNode(b, y, n)))
		| InvalidL(c, z, d) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		| InvalidR(c, z, d) -> Red(RNode(BNode(a, x, b), y, BNode(c, z, d)))
		end
	| BNode(BNode(n, y, c), z, d) when v < y -> begin match aux n with
		| Red(n) -> Black(BNode(BNode(n, y, c), z, d))
		| Black(n) -> Black(BNode(BNode(n, y, c), z, d))
		| InvalidL(a, x, b) -> Black(BNode(RNode(blackify a, x, BNode(b, y, c)), z, d))
		| InvalidR(a, x, RNode(a', b, b')) -> Black(BNode(RNode(BNode(a, x, a'), b, BNode(b', y, c)), z, d))
		end
 	| BNode(BNode(a, y, n), z, d) when y < v && v < z -> begin match aux n with
		| Red(n) -> Black(BNode(BNode(a, y, n), z, d))
		| Black(n) -> Black(BNode(BNode(a, y, n), z, d))
		| InvalidL(RNode(a', b', b''), b, c) -> Black(BNode(RNode(BNode(a, y, a'), b', BNode(b'', b, c)), z, d))
		| InvalidR(bl, b, x) -> Black(BNode(RNode(BNode(a, y, bl), b, blackify x), z, d))
		end
	| BNode(a, x, BNode(n, z, d)) when x < v && v < z -> begin match aux n with
		| Red(n) -> Black(BNode(a, x, BNode(n, z, d)))
		| Black(n) -> Black(BNode(a, x, BNode(n, z, d)))
		| InvalidL(b, y, c) -> Black(BNode(a, x, RNode(blackify b, y, BNode(c, z, d))))
		| InvalidR(b, y, RNode(cl, c, cr)) -> Black(BNode(a, x, RNode(BNode(b, y, cl), c, BNode(cr, z, d))))
		end
	| BNode(a, x, BNode(b, y, n)) when y < v -> begin match aux n with
		| Red(n) -> Black(BNode(a, x, BNode(b, y, n)))
		| Black(n) -> Black(BNode(a, x, BNode(b, y, n)))
		| InvalidL(RNode(cl, c, cr), z, d) -> Black(BNode(a, x, RNode(BNode(b, y, cl), c, BNode(cr, z, d))))
		| InvalidR(c, z, d) -> Black(BNode(a, x, RNode(BNode(b, y, c), z, blackify d)))
		end 
	| RNode(a, x, b) when v < x -> begin match aux a with
		| Red(n) -> InvalidL(n, x, b)
		| Black(n) -> Red(RNode(n, x, b))
		| _ -> assert false
		end
	| RNode(a, x, b) when v > x -> begin match aux b with
		| Red(n) -> InvalidR(a, x, n)
		| Black(n) -> Red(RNode(a, x, n))
		| _ -> assert false
		end

	(* Base Cases *)
	| BNode(Nil, x, b) when v < x -> Black(BNode(RNode(Nil, v, Nil), x, b))
	| BNode(a, x, Nil) when x < v -> Black(BNode(a, x, RNode(Nil, v, Nil)))
	| Nil -> Red(RNode(Nil, v, Nil))
	| RNode _ as r -> Red(r)
	| BNode _ as b -> Black(b)
	in match aux t with
	| Red(RNode(l, v, r)) -> Tree(BNode(l, v, r))
	| Black(n) -> Tree n
	| InvalidL(l, v, r) -> Tree(BNode(l, v, r))
	| InvalidR(l, v, r) -> Tree(BNode(l, v, r))

(* (* in_res of removal *)
type (_, 'a) rem_res = 
(* Red in_res *)
| R : (red, 'n, 'a) node -> ('n, 'a) rem_res
(* Black in_res *)
| B : (black, 'n, 'a) node -> ('n, 'a) rem_res
(* Double black in_res *)
| DB: (black, 'n, 'a) node -> (s 'n, 'a) rem_res
(* Negative black *)
| NB: (black, s 'n, 'a) node * 'a * (black, s 'n, 'a) node -> ('n, 'a) rem_res

let remove : type a. type a. a tree -> a -> a tree = fun (Tree n) v ->
	let rec replace : type c n. (c, n, a) node -> a -> (c, n, a) node = fun n x -> match n with
	| Red(Nil, v, Nil) -> Red(Nil, x, Nil)
	let rec aux : type c n. (c, n, a) node -> (n, a) in_res = fun n -> match n with
	| Red(l, v, r)

in match aux n with
| R(RNode(l, x, r)) -> Tree(BNode(l, x, r))
| B(n) -> Tree n
| DB(n) -> Tree n
(* I don't think this should ever happen *)
| NB(l, x, r) -> Tree n    

 *)




(* let remove_min : type a. a tree -> a tree = fun (Tree n) ->
	let rec aux : type c n. (c, n, a) node -> (n, a) in_res = fun n -> match n with
	| RNode(BNode(Nil, _, Nil), v, BNode(a, r, b)) -> 
		begin match a, b with 
		| Nil, _ -> Black(BNode(RNode(Nil, v, Nil), r, b))
		| RNode(Nil, av, Nil), Nil -> Black(BNode(RNode(Nil, v, Nil), av, Nil))
		| RNode(Nil, av, Nil), RNode(Nil, bv, Nil) -> Red(RNode(BNode(Nil, v, Nil), av, BNode(Nil, bv, Nil)))
		end
	| RNode(BNode(Nil, _, RNode(Nil, l, Nil)), v, r) ->
		Red(RNode(BNode(Nil, l, Nil), v, r))
	| BNode(RNode(Nil, _, Nil), v, r) -> Black(BNode(Nil, v, r))
	| BNode(BNode(Nil, _, Nil), v, RNode(rl, rv, rr)) -> failwith "Not implemented" (* Black(BNode(BNode(Nil, v, rl))) *)
	| BNode(BNode(Nil, _, Nil), v, BNode(Nil, rv, Nil)) -> 
	| RNode(a, x, b) -> begin match aux a with
		| Black(n) -> Red(RNode(n, x, b))
		| Red(n) -> InvalidL(n, x, b)
		| InvalidL(a, y, BNode(l, v, r)) -> failwith "Not implemented"
		| InvalidR(BNode(a, y, l), v, r) -> failwith "Not implemented"
		end
	| BNode(a, x, b) -> begin match aux a with
		| Black(n) -> Black(BNode(n, x, b))
		| Red(n) -> Black(BNode(n, x, b))
		| InvalidL(RNode(ll, lv, lr), v, r) -> Red(RNode(BNode(ll, lv, lr), v, BNode(r, x, b)))
		| InvalidR(l, v, RNode(rr, r, rl)) -> Red(RNode(BNode(l, v, rl), r, BNode(rr, x, b)))
		end
	| RNode(Nil, _, Nil) -> Black(Nil)
	| Nil -> assert false
in match aux n with
| Black(n) -> Tree(n)
| Red(RNode(l, v, r)) -> Tree(BNode(l, v, r))
| InvalidL(l, v, r) -> Tree(BNode(l, v, r))
| InvalidR(l, v, r) -> Tree(BNode(l, v, r)) *)

let check : type a. a tree -> unit = fun (Tree n) ->
	(* Only need to check that ∀ n : node, 
		value left_child(n) < value n < value right_child(n)*)
	let get_value : type c n. (c, n, a) node -> a = fun n -> match n with
	| RNode(_, v, _) -> v
	| BNode(_, v, _) -> v 
	| _ -> assert false in
	let rec f : type c n. (c, n, a) node -> unit = fun n -> match n with
	| Nil -> ()
	| BNode(Nil, v, Nil) -> ()
	| RNode(Nil, v, Nil) -> ()
	| BNode(Nil, v, r) -> assert(v < get_value r); f r
	| RNode(Nil, v, r) -> assert(v < get_value r); f r
	| BNode(l, v, Nil) -> assert(v > get_value l); f l
	| RNode(l, v, Nil) -> assert(v > get_value l); f l
	| BNode(l, v, r) -> assert(v < get_value r); assert(v > get_value l); f l; f r
	| RNode(l, v, r) -> assert(v < get_value r); assert(v > get_value l); f l; f r
in f n


(* Usefule auxilary functions *)
let to_list : type a. a tree -> a list = fun (Tree n) ->
	let rec aux : type c n. (c, n, a) node -> a list -> a list = fun n acc-> match n with
	| Nil -> acc
	| RNode(l, v, r) -> aux l (v :: (aux r acc))
	| BNode(l, v, r) -> aux l (v :: (aux r acc))
in aux n []

(* in-order traversal *)
let fold : type a b c. (a -> b -> b) -> a tree -> b -> b = fun f (Tree n) init ->
	let rec aux : type co n. (co, n, a) node -> b -> b = fun n acc -> match n with
	| Nil -> acc
	| RNode(l, v, r) -> aux r (f v (aux l acc))
	| BNode(l, v, r) -> aux r (f v (aux l acc))
in aux n init

(* Post-order traversal *)
let fold_post : type a b c. (a -> b -> b) -> a tree -> b -> b = fun f (Tree n) init ->
	let rec aux : type co n. (co, n, a) node -> b -> b = fun n acc -> match n with
	| Nil -> acc
	| RNode(l, v, r) -> f v (aux r (aux l acc))
	| BNode(l, v, r) -> f v (aux r (aux l acc))
in aux n init

(* pre-order traversal *)
let fold_pre : type a b c. (a -> b -> b) -> a tree -> b -> b = fun f (Tree n) init ->
	let rec aux : type co n. (co, n, a) node -> b -> b = fun n acc -> match n with
	| Nil -> acc
	| RNode(l, v, r) -> aux l (aux r (f v acc))
	| BNode(l, v, r) -> aux l (aux r (f v acc))
in aux n init

let mem : type a. a tree -> a -> bool = fun (Tree n) v -> 
	let rec aux : type c n. (c, n, a) node -> bool = fun n -> match n with
	| Nil -> false
	| RNode(l, v', r) -> if v = v' then true else if v < v' then aux l else aux r
	| BNode(l, v', r) -> if v = v' then true else if v < v' then aux l else aux r
in aux n