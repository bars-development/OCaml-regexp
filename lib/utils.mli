module type OrderedSet = sig end
module Bitarray :
  sig
    val empty : int -> bytes
    val is_empty : bytes -> bool
    val add : int -> bytes -> unit
    val delete : int -> bytes -> unit
    val eq : bytes -> bytes -> bool
    val hd : bytes -> int
    val pop_hd : bytes -> int
    val contains : int -> bytes -> bool
    val merge : bytes -> bytes -> bytes
    val exclude_from : bytes -> bytes -> unit
    val fold_left : ('a -> int -> 'a) -> 'a -> bytes -> 'a
  end
module MyList :
  sig
    val add : 'a -> 'a list -> 'a list
    val eq : 'a list -> 'a list -> bool
    val isInSorted : 'a -> 'a list -> bool
    val mergeSorted : 'a list -> 'a list -> 'a list
  end
val search : 'a -> 'b list -> ('a -> 'b -> bool) -> bool
