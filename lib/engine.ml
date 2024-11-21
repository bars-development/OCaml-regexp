open Utils
open RegE

(** Defines a token type for parsing regular expressions from strings *)
type token = 
  Symbol of char 
  | Bar
  | Kleene_star
  | Plus 
  | Question_mark 
  | Left_paren
  | Right_paren 
  | Dot
  | Dash
  | Left_Sbracket (*To be implemented later*)
  | Right_Sbracket
  | Backslash 
(** Defines a char-> token mapping *)
let get_token= function 
  | '|'-> Bar
  | '*'-> Kleene_star
  | '+'-> Plus
  | '?'-> Question_mark
  | '('-> Left_paren
  | ')'-> Right_paren 
  | '-'->Dash
  | '.'-> Dot
  | '['-> Left_Sbracket
  | ']'-> Right_Sbracket
  | '\\'-> Backslash
  | c -> Symbol c

(** [tokenize s] returns a list of tokens to represent the string [s] defining a regular expression*)
let tokenize s = 
  let rec aux i acc  =
    if(i=String.length s) then acc
    else
      let char = String.get s i in 
      let token = get_token char in 
      match token with
        | Backslash -> aux (i+2) ((Symbol (String.get s (i+1)))::acc)
        | _-> aux (i+1) (token::acc)
  in List.rev (aux 0 [])

(** [rdp tokens] given token list representing the regexp string [tokens] returns a regexp_expr representation of the regular expression *)
let rdp tokens= 
  let mtch t l = (List.hd l)= t
  in
  let union lst= 
    let rec aux left acc  =
      match left with
      | []->acc
      | h::t -> aux t (Union (acc, h))
    in aux (List.tl lst) (List.hd lst)
  in
  let symbol2character s = match s with
    |Symbol s-> Character (C s)
    |Dot -> union (List.map (fun x-> Character x) ascii)
    |_ -> failwith "Error: Invalid input"
  in
  let rec main t= 
    let before, rest = expr t in 
    if(List.is_empty rest || (List.hd rest) <>  Bar) then (before, rest)
    else
      let new_expr, new_rest = main (List.tl rest )in 
      (Union (before, new_expr), new_rest)  
  and expr t= 
    let before, rest = factor t in 
    if(List.is_empty rest || mtch Bar rest|| mtch Right_paren rest|| mtch Kleene_star rest) then (before, rest)
    else 
        let new_expr, new_rest = expr rest in 
        (Product (before, new_expr), new_rest)
  and factor t= 
      let before, rest = simple t in 
      if(List.is_empty rest) then (before, rest) else
      if mtch Kleene_star rest then
          (Star before, List.tl rest)
      else if mtch Plus rest then
        (Product(before, Star before), List.tl rest)
      else if mtch Question_mark rest then 
        (Union(before, Eps), List.tl rest)
      else (before, rest)
  and simple t= 
      if(t=[]) then (Eps, [])else
      if(List.hd t)=Left_Sbracket then
        begin
          let expression, rest = range (List.tl t) in
          if not (mtch Right_Sbracket rest)
            then 
              let expr2, nrest = simple (Left_Sbracket::rest)in
              Union (expression, expr2), nrest
          else (expression, List.tl rest)
        end
      else
      if(List.hd t)=Left_paren then 
        begin
          let expression, rest = main (List.tl t) in 
          if not (mtch Right_paren rest)
            then failwith "Error: invalid parentheses"
          else 
            (expression, List.tl rest)
        end
      else
        let res = symbol2character (List.hd t) in 
        (res, List.tl t)
  and range t = 
    match t with 
      |Symbol a::tail -> 
        begin
          if(List.hd tail <> Dash) then 
            failwith "Wrong structure"
          else
          match List.tl tail with 
            Symbol b::tail2 -> union (List.init (Char.code b - Char.code a+1) (fun x->Character (C (Char.chr (x+ Char.code a))))), tail2
            |_->failwith "Error: invalid expression"
        end
      |_-> failwith "Unexpected character"

    
  in 
  let res, rest= main tokens in
  if(rest<>[]) then 
    failwith "Parsing error" 
  else
  res

let parse_expression exp = rdp (tokenize exp)

module type REEngine = sig
  type machine
  val create_machine:  ?alphabet:char_expr list -> string ->machine
  val match_expression: machine->string->bool
  val longest_match: machine->string->int
end

module MakeEngine (R : Impl) : REEngine= struct
  open R
  type machine = dfa_structure
  let create_machine ?(alphabet=ascii) s = construct_dfa (parse_expression s) alphabet

  let match_expression machine input = 
    let split_string_chars s  = List.init (String.length s) (String.get s) in

    let next_dfa_state state_ind trigger = 
      let trigger_ind = List.find_index (fun x -> x=trigger) machine.dfa_alphabet  in
      if(Option.is_none trigger_ind) then  
        failwith "Character not in the alphabet" 
      else
        LookupTable.get machine.table (state_ind) (Option.get trigger_ind)
    in 
    let rec aux state_ind = function
      | _ when state_ind=machine.empty -> false
      | []-> search state_ind machine.accept (=)
      | h::t -> aux (next_dfa_state state_ind (C h)) t
    in aux machine.start (split_string_chars input) 

  let longest_match machine input = 
    let split_string_chars s  = List.init (String.length s) (String.get s) in

    let next_dfa_state state_ind trigger = 
      let trigger_ind = List.find_index (fun x -> x=trigger) machine.dfa_alphabet  in
      if(Option.is_none trigger_ind) then  
        failwith "Character not in the alphabet" 
      else
        LookupTable.get machine.table (state_ind) (Option.get trigger_ind)
    in 

    let rec aux state_ind count longest = function
      | _ when state_ind=machine.empty -> longest
      | []-> if search state_ind machine.accept (=) then count else longest
      | h::t -> 
        let lngst = if search state_ind machine.accept (=) then count else longest in
        aux (next_dfa_state state_ind (C h)) (count+1) lngst t 
    in aux machine.start 0 0 (split_string_chars input)
  
end


module Engine1:REEngine = MakeEngine(RE1)
module Engine2:REEngine = MakeEngine(RE2)
