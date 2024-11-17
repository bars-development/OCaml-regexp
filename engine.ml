open Regular_expressions
open RE2

let parse_expression exp = failwith "tto"

let create_machine s = construct_dfa (parse_expression s) ascii

let match_expression machine input = 
  let split_string_chars s  = List.init (String.length s) (String.get s) in

  let next_dfa_state state_ind trigger = 
    let trigger_ind = List.find_index (fun x -> x=trigger) machine.dfa_alphabet  in
    if(Option.is_none trigger_ind) then  
      failwith "Character not in alphabet" 
    else
      machine.table.(state_ind).(Option.get trigger_ind)
  in 
  let rec aux state_ind = function
    | _ when machine.dfa_states.(state_ind)=empty -> false
    | []-> Mylist.search (machine.dfa_states.(state_ind)) machine.accept eq
    | h::t -> aux (next_dfa_state state_ind (C h)) t
  in aux (Array.length machine.dfa_states-1) (split_string_chars input) 