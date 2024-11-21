(** Represents a character or an epsilon (empty transition) in the regex alphabet. *)
type char_expr = 
  | Epsilon  (** Epsilon transition (no input character). *)
  | C of char  (** A single character in the alphabet. *)

(** Represents the structure of an ordinary regular expression. *)
type regex_expr =
  | Empty  (** The empty set (matches no strings). *)
  | Eps  (** The empty string. *)
  | Character of char_expr  (** Matches a single character or epsilon. *)
  | Union of regex_expr * regex_expr  (** Logical OR of two regular expressions. *)
  | Product of regex_expr * regex_expr  (** Concatenation of two regular expressions. *)
  | Star of regex_expr  (** Kleene star: zero or more repetitions of a regex. *)

(** A predefined list of char_expr values representing the standard ASCII character set. *)
val ascii : char_expr list

(** Defines the interface for DFA construction implementations. *)
module type Impl = sig
  (** Represents the structure of a DFA. *)
  type dfa_structure = {
    dfa_alphabet : char_expr list;  (** The DFA's alphabet. *)
    start : int;  (** The start state of the DFA. *)
    accept : int list;  (** List of accepting states. *)
    empty : int;  (** The empty state of the DFA. *)
    table : Utils.LookupTable.lookupTable;  (** The DFA's transition table. *)
  }

  (** Constructs a DFA from a regular expression and an alphabet.
     @param regex_expr The input regular expression to convert.
     @param char_expr list The DFA's alphabet.
     @return A DFA represented as a dfa_structure. *)
  val construct_dfa : regex_expr -> char_expr list -> dfa_structure
end

(** Implementation of the Impl interface using NFA to DFA conversion. *)
module RE1 : Impl

(** Implementation of the Impl interface using Brzozowski derivatives for DFA construction. *)
module RE2 : Impl
