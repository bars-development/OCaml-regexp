val parse_expression : string -> Rege.regex_expr
module type REEngine =
  sig
    type machine
    val create_machine : ?alphabet:Rege.char_expr list -> string -> machine
    val match_expression : machine -> string -> bool
  end
module MakeEngine : functor (R : Rege.Impl) -> REEngine
module Engine1 : REEngine
module Engine2 : REEngine
