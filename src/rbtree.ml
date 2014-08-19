type color = Red | Black
type 'a node = Node of color * 'a node * 'a * 'a node | Nil
type 'a tree = Tree of 'a node

let empty : 'a tree = Tree Nil

let insert : 'a tree -> 'a -> 'a tree = fun (Tree t) v -> 
	let rec ins : 'a node -> 'a node = function
	| Node(c, l, v', r) when v < v' -> balance (Node(c, ins l, v', r))
	| Node(c, l, v', r) when v > v' -> balance (Node(c, l, v', ins r))
	| Nil -> Node(Red, Nil, v, Nil)
	| n -> n

	and balance : 'a node -> 'a node = function
	| Node(Black, Node(Red, Node(Red, a, x, b), y, c), z, d)
	| Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
	| Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
	| Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
		Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
	| n -> n

	in let Node(_, a, x, b) = ins t in Tree(Node(Black, a, x, b))

let check_invariant (Tree t) : bool = 
	let rec h : 'a node -> int = function
	| Node(Red, l, _, r) -> 
		let l = h l in
		let r = h r in
		assert (l = r); l
	| Node(Black, l, _, r) ->
		let (l, r) = (h l, h r) in
		assert (l = r); (1 + l)
	| Nil -> 0
	in
	let is_b = function
	| Node(Red, _, _, _) -> false
	| _ -> true in
	let rec check_rr : 'a node -> bool = function
	| Node(Red, l, _, r) ->
		assert (is_b l);
		assert (is_b r);
		check_rr l && check_rr r
	| Node(Black, l, _, r) ->
		check_rr l && check_rr r
	| Nil -> true
	in let _ = h t in(check_rr t)