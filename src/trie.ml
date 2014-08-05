module Trie : sig 
  type 'a trie

  val empty : 'a trie
  val find : 'a trie -> string -> 'a
  val mem : 'a trie -> string -> bool
  val insert : 'a trie -> string -> 'a -> 'a trie
  val remove : 'a trie -> string -> 'a trie
(*   val fold : 'a trie -> (char -> 'a option -> 'b -> 'b) -> 'b -> 'b *)

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

  let mem : 'a trie -> string -> bool = fun t s ->
    let l = String.length s in
    let rec aux i = function
      | Trie(Some v, _) when i = (l-1) -> true
      | Trie(None, _) when i = (l-1) -> false
      | Trie(_, map) -> try aux (i+1) (CharMap.find s.[i] map) with Not_found -> false
    in aux 0 t   

  let insert : 'a trie -> string -> 'a -> 'a trie = fun t s x ->
    let l = String.length s in
    let rec aux cont i = function
      | Trie(v, map) when i = (l-1) -> cont (Trie(Some x, map))
      | Trie(v, map) -> 
          let t' = try
            CharMap.find s.[i] map
          with | Not_found -> empty in
          aux (fun x -> cont(Trie(v, CharMap.add (s.[i]) x map))) (i+1) t'
    in aux (fun x -> x) 0 t

  let remove : 'a trie -> string -> 'a trie = fun t s ->
    let l = String.length s in
    let rec aux (cont : 'a trie -> 'a trie) i = function
      | Trie(v, map) when i = (l-1) -> cont (Trie(None, map))
      | Trie(v, map) ->
          aux (fun x -> cont (Trie(v, CharMap.add (s.[i]) x map))) (i+1) (CharMap.find (s.[i]) map)
    in aux (fun x -> x) 0 t

(*     let rec fold : 'a trie -> (char -> 'a option -> 'b -> 'b) -> 'b -> 'b =
    fun Trie(v, map) f init ->  *)
    

end

module Make : 
  functor (Ord : Map.OrderedType) -> sig
    type 'a trie

    val empty : 'a trie
    val find : 'a trie -> Ord.t array -> 'a
    val mem : 'a trie -> Ord.t array -> bool
    val insert : 'a trie -> Ord.t array -> 'a -> 'a trie
  end = 
  functor (Ord : Map.OrderedType) -> struct
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

    let mem : 'a trie -> Ord.t array -> bool = fun t s ->
      let l = Array.length s in
      let rec aux i = function
        | Trie(Some v, _) when i = (l-1) -> true
        | Trie(None, _) when i = (l-1) -> false
        | Trie(_, map) -> try aux (i+1) (OrdMap.find s.(i) map) with Not_found -> false
      in aux 0 t

		let insert : 'a trie -> Ord.t array -> 'a -> 'a trie = fun t s x ->
	    let l = Array.length s in
	    let rec aux cont i = function
	      | Trie(v, map) when i = (l-1) -> cont (Trie(Some x, map))
	      | Trie(v, map) ->
    		  let t' = try
            OrdMap.find s.(i) map
          with | Not_found -> empty in
          aux (fun x -> cont(Trie(v, OrdMap.add (s.(i)) x map))) (i+1) t'
	    in aux (fun x -> x) 0 t

    let remove : 'a trie -> Ord.t array -> 'a trie = fun t s ->
      let l = Array.length s in
      let rec aux cont i = function
        | Trie(v, map) when i = (l-1) -> cont (Trie(None, map))
        | Trie(v, map) ->
            aux (fun x -> cont(Trie(v, OrdMap.add (s.(i)) x map))) (i+1) (OrdMap.find (s.(i)) map)
      in aux (fun x -> x) 0 t
end
