(* Parses a string into a regex_expr. 
   @param string The input string representing the regular expression.
   @return A regex_expr corresponding to the parsed input. *)
val parse_expression : string -> RegE.regex_expr

(* Defines the interface for regular expression matching engines. *)
module type REEngine = sig
  (* Represents the internal structure of a matching engine. *)
  type machine

  (* Creates a matching engine from a regular expression string.
     @param ?alphabet An optional list of char_expr representing the alphabet (defaults to ASCII if not provided).
     @param string The input regular expression as a string.
     @return A machine ready for matching operations. *)
  val create_machine : ?alphabet:RegE.char_expr list -> string -> machine

  (* Matches a string against a regular expression using a matching engine.
     @param machine The matching engine created from a regular expression.
     @param string The input string to match against the regular expression.
     @return true if the string matches the regular expression, false otherwise. *)
  val match_expression : machine -> string -> bool
end

(* Functor that creates a matching engine using a given DFA implementation.
   @param R A module implementing the RegE.Impl interface.
   @return A module implementing the REEngine interface. *)
module MakeEngine : functor (R : RegE.Impl) -> REEngine

(* A matching engine using the RE1 DFA implementation. *)
module Engine1 : REEngine

(* A matching engine using the RE2 DFA implementation. *)
module Engine2 : REEngine
