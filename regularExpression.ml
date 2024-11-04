

module Types= struct 
  type char_expr = Epsilon | C of char
  (* Ordinary regular expression *)
  type regex_expr = 
    | Empty (*Denotes the empty set*)
    | Eps (*Denotes empty string*)
    | Character of char_expr
    | Union of regex_expr*regex_expr
    | Product of regex_expr*regex_expr
    | Star of regex_expr
  (* Intersection and Not are part of generalized regular expressions, will not be implemented *)
  (* | Intersection of regex_expr*regex_expr *)
  (* | Not of regex_expr *)
  let parse_expression  = failwith "TODO"
  
  

end

module Common = struct
  include Types
  let merge_alphabets a b = 
    let chr2int = function (C c)->Char.code c| Epsilon->0 in
    let aa = Mylist.myMap chr2int a in
    let ba = Mylist.myMap chr2int b in
    Mylist.myMap (function 
      |0->Epsilon
      |c ->C (Char.chr c)) (Mylist.mergeSorted aa ba )
  let rec is_in_states st sn comp = 
      match sn with
        | []->false
        | h::_ when comp st h -> true
        | _::t-> is_in_states st t comp
  let toInt = function
    | None -> -1
    | Some x -> x 
  let get_option = function
    | Some x -> x
    | None   -> raise (Invalid_argument "Option.get")
 
  let split_string_chars s  = List.init (String.length s) (String.get s);;
end



module type REGEXP = sig 
  include module type of Types
  type dfa_state
  type dfa_structure 
  
  (* val parse_expression: string -> regex_expr *)
  val construct_dfa : regex_expr -> char_expr list option -> dfa_structure

  val match_expression : dfa_structure -> string -> bool

end

(* Implementation 2: Brzozowski derivatives *https://en.wikipedia.org/wiki/Brzozowski_derivative* *)
module RE2: REGEXP = struct
  include Common
  (* Derivatives of regular expressions *)
  let rec simplify expr =
    match expr with
    | Product (a, b) when a=Eps -> simplify b
    | Product (a, b) when b=Eps -> simplify a
    | Product (a, b) when a=Empty || b=Empty -> Empty
    | Union (a, b) -> 
      let na = simplify a in 
      let nb = simplify b in 
      if(na=nb) then na else if(na=Empty) then nb else if (nb=Empty) then na else Union (na, nb)
    | Product (a, Product (b, c)) -> 
        simplify (Product (Product (a, b), c)) (* Flatten left *)
    | Product (a, b) -> Product (simplify a, simplify b)
    (* | Not a -> Not (reduce a) *)
    | Star a -> Star (simplify a)
    | _ -> expr
  let rec eq exp1 exp2 = match simplify exp1 with
    | Eps -> begin 
          match  exp2 with
            | Eps -> true
            | _ -> false
        end
      | Empty -> 
        begin
          match exp2 with
          | Empty -> true
          | _ -> false
        end
    | Character a-> begin 
        match exp2 with
          |Character b when a = b -> true
          | _ -> false
        end
    | Union (a, b)-> 
        begin
          match simplify exp2 with
          | Union (c, d) when (eq c a && eq b d) || (eq c b && eq a d) -> true
          | _ -> false 
        end
    | Star a -> 
      begin
        match simplify exp2 with
        | Star b when eq a b -> true
        | _ -> false
      end
    | Product (a, b) -> 
      begin
        match simplify exp2 with
          | Product (c, d) when eq a c && eq b d->true
          | _ -> false
      end
    (* | Intersection _ | Not _ -> failwith "Unimplemented" *)

  let rec derivate exp char= 
    let rec nullable r =
      match r with
      | Empty -> false
      | Eps -> true
      | Character _ -> false
      | Union (r1, r2) -> nullable r1 || nullable r2
      | Product (r1, r2) -> nullable r1 && nullable r2
      (* | Intersection (r1, r2) ->  nullable r1 && nullable r2 *)
      (* | Not a -> false *)
      | Star _ -> true 
  in 
    match exp with
    | Empty -> Empty
    | Eps -> Empty
    | Character c -> 
      if char = c then Eps 
      else Empty
      | Union (a, b) -> simplify (Union(derivate a char, derivate b char))
      | Product (a, b) -> 
        if nullable a then 
          simplify (Union(derivate b char, simplify (Product (derivate a char, b)))) 
      else 
        simplify (Product (derivate a char, b))
    (* | Intersection (a, b) -> Intersection (derivate a char, derivate b char) *)
    (* | Not a -> Not (derivate a char) *)
    | Star a -> Product(derivate a char, Star a)
  
  (* DFA construction *) 
  type dfa_state = regex_expr

  type dfa_structure = {
    dfa_alphabet: char_expr list;
    dfa_states: dfa_state array;
    dfa_start: dfa_state;
    accept: dfa_state list;
    table: int array array;
  }

 

  let get_alphabet exp = 
    let rec aux l acc = match l with 
      | Empty -> []
      | Eps -> acc
      | Character c -> c::acc
      | Product (a, b) | Union (a, b) -> aux a (aux b acc)
      | Star a -> aux a acc 
  in aux exp []

  let construct_dfa exp alph = 
    let alphabet = match alph with 
      |Some a -> a 
      |None -> get_alphabet exp 
    in
    let rec compute_dfa_states st seen result = 
      if is_in_states st seen eq
        then  (seen, result)
      else 
        let to_alph = List.map (fun l -> derivate st l) alphabet in
        List.fold_left (fun (sn, rt) x -> compute_dfa_states x sn rt) (st::seen, to_alph::result) to_alph
    in
    let (states, table) = compute_dfa_states exp [] [] in
    let state_array = Array.of_list states in 
    let simplified_table = Array.of_list (
      List.map (fun sl-> 
        Array.map (fun x -> 
          toInt (Array.find_index (fun y -> eq y x) 
          state_array)) 
        (Array.of_list sl)) 
      table)  
    in
    let rec aux_filter = function 
      | Empty -> false
      | Star _ -> true
      | Eps -> true
      | Union (a, b) -> if(aux_filter a || aux_filter b) then true else false
      | Product (a, b) -> if(aux_filter a && aux_filter b) then true else false
      (* | Not _ -> false *) 
      (* | Intersection _ -> failwith "Unimplemented" *)
      | _ ->false
    in
    {
      dfa_alphabet = alphabet;
      dfa_states = state_array;
      dfa_start = exp;
      accept = List.filter aux_filter states;
      table = simplified_table
    }


(* Splits a string into characters *) 
  let split_string_chars s = List.init (String.length s) (String.get s);;

(* Matches the given string with the expression *)
  let match_expression machine input = 
    let next_dfa_state state_ind trigger = 
      (* let state_ind = Array.find_index (fun x ->x=state) machine.states  in *)
      let trigger_ind = List.find_index (fun x -> x=trigger)  machine.dfa_alphabet  in
      let next_hop = machine.table.(state_ind).(get_option trigger_ind) in
      if(machine.dfa_states.(next_hop)=Empty) then -1 else
      next_hop
    in 
    let rec aux state_ind = function
      | _ when state_ind = -1 -> false
      | []-> is_in_states (machine.dfa_states.(state_ind)) machine.accept eq
      | h::[] -> 
        let next = next_dfa_state state_ind (C h) in 
        if next = -1 then  false 
        else is_in_states (machine.dfa_states.(next)) machine.accept eq
      | h::t -> aux (next_dfa_state state_ind (C h)) t

    in aux (Array.length machine.dfa_states-1) (split_string_chars input) 
  end

module RE1: REGEXP = struct 
  include Common
  
  (* First step: Expression -> NFA *)
  type state = int
  type transition = state*char_expr*state
  type nfa_struct = {
    alphabet: char_expr list;
    start: state;
    last: state;
    transitions: transition list;
  }
  
  let regex_to_nfa = 
    let empty = {
        alphabet = Epsilon::[];
        start = 0;
        last = 0;
        transitions = [];
    } in
    let rec convert_expr_to_nfa shift= function
      | Empty -> {empty with last=1} 
      | Eps -> empty
      | Character a -> 
        { alphabet = Epsilon::a::[];
          start=shift;
          last=shift+1; 
          transitions = [(shift, a, shift+1)]
        }
      | Union (a, b) ->  
        let nfa1 = convert_expr_to_nfa (shift+1) a in 
        let nfa2 = convert_expr_to_nfa (nfa1.last+1) b in
        {
          alphabet = merge_alphabets nfa1.alphabet nfa2.alphabet;
          start = shift;
          last =nfa2.last+1;
          transitions = (shift, Epsilon, shift+1)::(shift, Epsilon, nfa1.last+1)::(nfa2.last, Epsilon, nfa2.last+1)::(nfa1.last, Epsilon, nfa2.last)::nfa1.transitions@nfa2.transitions
        }
      | Product (a, b) ->
        let nfa1 = convert_expr_to_nfa (shift) a in 
        let nfa2 = convert_expr_to_nfa (nfa1.last) b in
        {
            alphabet = merge_alphabets nfa1.alphabet nfa2.alphabet;
            start = shift;
            last =nfa2.last;
            transitions = nfa1.transitions@nfa2.transitions
          }
      | Star a ->
        let nfa1 = convert_expr_to_nfa (shift+1) a in 
        {
          alphabet = nfa1.alphabet;
          start = shift;
          last = nfa1.last+1;
          transitions = (shift,Epsilon, shift+1)::(shift, Epsilon, nfa1.last+1)::(nfa1.last, Epsilon, shift)::nfa1.transitions  
        }
      in convert_expr_to_nfa 0

  (* Second step helper functions *)
  let rec find_direct_transition n trigger lst acc= 
    match lst with
      | [] -> acc
      | (a, b, c)::t when a=n && b=trigger-> find_direct_transition n trigger t (Mylist.add c acc)
      | _::t -> find_direct_transition n trigger t acc 

  (* Calculates the epsilon closure of the node (All possible states that could be reached by using only epsilon moves from the given node) *)
  let epsilon_closure node transitions = 
    let delete_all_from a b= 
      let rec aux lst acc = 
        match lst with 
          |[] -> acc
          |h::t -> if (Mylist.isInSorted h a) then aux t acc else aux t (h::acc)
      in aux b [] in
    let rec aux tbc acc = 
      if(tbc=[]) then acc else
      let d = find_direct_transition (List.hd tbc) Epsilon transitions [] in
      let d2 = delete_all_from acc d in 
      let new_tbc = Mylist.mergeSorted (List.tl tbc) d2 in
      aux new_tbc (Mylist.add (List.hd tbc) acc)
      
    in (aux [node] [])

  let apply_transition_for_nfa n trigger transitions = 
    let enc = epsilon_closure n transitions in 
    let dir = List.fold_left (fun acc x -> Mylist.mergeSorted acc (find_direct_transition x trigger transitions [])) [] enc in
    List.fold_left (fun acc x -> Mylist.mergeSorted acc (epsilon_closure x transitions)) [] dir
  
  (* Second step nfa -> dfa *)
  type dfa_state = state list
    
  type dfa_structure = {
    dfa_alphabet : char_expr list;
    dfa_states : dfa_state array;
    dfa_start : dfa_state;
    accept : dfa_state list;
    table : int array array;
  }
  let apply_for_dfa s trigger nfa = 
    let rec res list acc = match list with
      | [] -> acc
      | h::t -> res t (Mylist.mergeSorted (apply_transition_for_nfa h trigger nfa.transitions) acc)
    in List.fold_left (fun acc v->  Mylist.mergeSorted (epsilon_closure v nfa.transitions) acc) [] (res s [])

  let construct_dfa expr al=
    let nfa = regex_to_nfa expr in
    let alphabet = match al with
      |Some a -> a
      |None -> nfa.alphabet
    in
    
    let rec compute_dfa_states st seen result = 
      if is_in_states st seen (Mylist.eq) then (seen, result)
      else 
        let to_alph = List.map (fun a -> apply_for_dfa st a nfa ) alphabet in
        List.fold_left  (fun (seen, result) x ->  compute_dfa_states x seen result) (st::seen, to_alph::result) to_alph
      in
    let (states, table) = compute_dfa_states (epsilon_closure nfa.start nfa.transitions) [] [] in
    let state_array = Array.of_list states in 
    let rec contains_state l e = match l with
      |[] -> false 
      |h::_ when h=e -> true
      |_::t -> contains_state t e
    in
    let simplified_table = Array.of_list (
      List.map (fun sl-> 
        Array.map (fun x -> 
          toInt (Array.find_index (fun y -> y=x) 
          state_array)) 
        (Array.of_list sl)) 
      table)  in  
    {
      dfa_alphabet=nfa.alphabet;
      dfa_states = state_array;
      dfa_start= epsilon_closure nfa.start nfa.transitions;
      accept=List.filter (fun l-> contains_state l nfa.last) states;
      table=simplified_table
    }

  (* Match with a string *)
  let match_expression machine input= 
    let next_dfa_state state_ind trigger = 
      (* let state_ind = Array.find_index (fun x ->x=state) machine.states  in *)
      let trigger_ind = List.find_index (fun x -> x=trigger)  machine.dfa_alphabet  in
      if trigger_ind=None then -1 (*No transition*)
      else machine.table.(state_ind).(toInt trigger_ind)
    in 
    let rec aux state_ind =function
      | _ when state_ind= -1 -> false
      | []-> failwith "Something went wrong with the logic, this state should not reached"
      | h::[] -> let next = next_dfa_state state_ind (C h) in if next= -1 then false else is_in_states (machine.dfa_states.(next)) machine.accept Mylist.eq
      | h::t -> aux (next_dfa_state state_ind (C h)) t

    in aux (Array.length machine.dfa_states-1) (split_string_chars input) 

end