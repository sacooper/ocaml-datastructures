module Trie : sig 
  type 'a trie

  val empty : 'a trie
  val insert : 'a trie -> string -> 'a -> 'a trie
  val find : 'a trie -> string -> 'a

end = struct
  module CharMap = Map.Make(Char)
  
  type 'a trie = Trie of 'a option * ('a trie) CharMap.t

  let empty : 'a trie = Trie(None, CharMap.empty)

  let find : 'a trie -> string -> 'a = fun t s ->
    let l = String.length s in
    let rec aux i = function
      | Trie(Some v, _) when i = (l-1) -> v
      | Trie(None, _) when i = (l-1) -> raise Not_found
      | Trie(_, map) -> aux (i+1) (CharMap.find s.[i] map)
    in aux 0 t 

  let insert : 'a trie -> string -> 'a -> 'a trie = fun t s x ->
    let l = String.length s in
    let rec aux cont i = function
      | Trie(v, map) when i = (l-1) -> cont (Trie(Some x, map))
      | Trie(v, map) -> 
          let t' = try
            CharMap.find s.[i] map
          with | Not_found -> empty in
          aux (fun x -> Trie(v, CharMap.add (s.[i]) x map)) (i+1) t'
    in aux (fun x -> x) 0 t

     (*  try 
        Trie(v, CharMap.add (s.[i]) (aux (i+1) (CharMap.find s.[i] map)) map)
      with Not_found ->
        let t' = aux (i+1) empty in
        Trie(v, CharMap.add (s.[i]) t' map) *)
end

module Make : 
  functor (Ord : Map.OrderedType) -> sig
    type 'a trie

    val empty : 'a trie
    val find : 'a trie -> Ord.t array -> 'a
    val insert : 'a trie -> Ord.t array -> 'a -> 'a trie
  end
  = functor (Ord : Map.OrderedType) -> struct
		module OrdMap = Map.Make(Ord)

		type 'a trie = Trie of 'a option * ('a trie) OrdMap.t

		let empty : 'a trie = Trie(None, OrdMap.empty)

		let find : 'a trie -> Ord.t array -> 'a = fun t s ->
		    let l = Array.length s in
		    let rec aux i = function
		      | Trie(Some v, _) when i = (l-1) -> v
		      | Trie(None, _) when i = (l-1) -> raise Not_found
		      | Trie(_, map) -> aux (i+1) (OrdMap.find s.(i) map)
		    in aux 0 t

		let insert : 'a trie -> Ord.t array -> 'a -> 'a trie = fun t s x ->
		    let l = Array.length s in
		    let rec aux cont i = function
		      | Trie(v, map) when i = (l-1) -> cont (Trie(Some x, map))
		      | Trie(v, map) ->
      		  let t' = try
              OrdMap.find s.(i) map
            with | Not_found -> empty in
            aux (fun x -> Trie(v, OrdMap.add (s.(i)) x map)) (i+1) t'
		    in aux (fun x -> x) 0 t

end
