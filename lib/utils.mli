(* Module for managing bit arrays with efficient set operations. *)
module Bitarray:
  sig
    (* Represents a bit array. *)
    type t

    (* Creates an empty bit array of the given size.
      @param int The size of the bit array.
      @return A new empty bit array. *)
    val empty : int -> t

    (* Checks if a bit array is empty (contains no set bits).
      @param t The bit array to check.
      @return true if the bit array is empty, false otherwise. *)
    val is_empty : t -> bool

    (* Adds an integer to the bit array.
      @param int The integer to add.
      @param t The bit array to modify. *)
    val add : int -> t -> unit

    (* Removes an integer from the bit array.
      @param int The integer to remove.
      @param t The bit array to modify. *)
    val delete : int -> t -> unit

    (* Compares two bit arrays for equality.
      @param t The first bit array.
      @param t The second bit array.
      @return true if the two bit arrays are equal, false otherwise. *)
    val eq : t -> t -> bool

    (* Retrieves the smallest element in the bit array.
      @param t The bit array.
      @return The smallest integer in the bit array. *)
    val hd : t -> int

    (* Removes and retrieves the smallest element in the bit array.
      @param t The bit array.
      @return The smallest integer that was removed. *)
    val pop_hd : t -> int

    (* Checks if a bit array contains a specific integer.
      @param int The integer to check for.
      @param t The bit array to search.
      @return true if the integer is in the bit array, false otherwise. *)
    val contains : int -> t -> bool

    (* Merges two bit arrays into a new bit array.
      @param t The first bit array.
      @param t The second bit array.
      @return A new bit array containing the union of the two. *)
    val merge : t -> t -> t

    (* Excludes all elements of one bit array from another.
      @param t The bit array to exclude elements from.
      @param t The bit array containing elements to exclude. *)
    val exclude_from : t -> t -> unit

    (* Applies a function to each element of the bit array, accumulating a result.
      @param ('a -> int -> 'a) The function to apply.
      @param 'a The initial accumulator value.
      @param t The bit array to iterate over.
      @return The final accumulated value. *)
    val fold_left : ('a -> int -> 'a) -> 'a -> t -> 'a
  end

(* Module for utility functions on lists. *)
module MyList:
  sig
    (* Adds an element to the beginning of a list.
      @param 'a The element to add.
      @param 'a list The list to modify.
      @return A new list with the element added. *)
    val add : 'a -> 'a list -> 'a list

    (* Compares two lists for equality.
      @param 'a list The first list.
      @param 'a list The second list.
      @return true if the two lists are equal, false otherwise. *)
    val eq : 'a list -> 'a list -> bool

    (* Checks if an element is in a sorted list.
      @param 'a The element to check.
      @param 'a list The sorted list.
      @return true if the element is in the list, false otherwise. *)
    val isInSorted : 'a -> 'a list -> bool

    (* Merges two sorted lists into a single sorted list.
      @param 'a list The first sorted list.
      @param 'a list The second sorted list.
      @return A new sorted list containing elements from both lists. *)
    val mergeSorted : 'a list -> 'a list -> 'a list
  end

(* Module for constructing and querying lookup tables. *)
module LookupTable:
  sig
    (* Represents a lookup table with precomputed entries and mapping. *)
    type lookupTable = { 
      entries : int array array;  (* Precomputed table of entries. *)
      mapping : int array;        (* Array mapping elements to indices. *)
    }

    (* Constructs a lookup table from input data.
      @param 'a list list The list of lists to populate the table.
      @param 'b array The array to map.
      @param ('b -> 'a -> bool) The comparison function.
      @return A constructed lookup table. *)
    val construct_table :
      'a list list -> 'b array -> ('b -> 'a -> bool) -> lookupTable

    (* Retrieves an entry from the lookup table.
      @param lookupTable The lookup table to query.
      @param int The row index.
      @param int The column index.
      @return The integer value at the specified position. *)
    val get : lookupTable -> int -> int -> int
  end

(* Searches for an element in a list using a custom comparison function.
   @param 'a The element to search for.
   @param 'b list The list to search.
   @param ('a -> 'b -> bool) The comparison function.
   @return true if the element is found in the list, false otherwise. *)
val search : 'a -> 'b list -> ('a -> 'b -> bool) -> bool
