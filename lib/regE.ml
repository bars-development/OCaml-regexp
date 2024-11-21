open Utils

type char_expr = Epsilon | C of char

type regex_expr = 
  | Empty                            
  | Eps                               
  | Character of char_expr           
  | Union of regex_expr*regex_expr    
  | Product of regex_expr*regex_expr  
  | Star of regex_expr                

let ascii = List.init 255 (fun c -> C (Char.chr c))

module type Impl = sig

  type dfa_structure = {
    dfa_alphabet: char_expr list;
    start: int;
    accept: int list;
    empty: int;
    table: LookupTable.lookupTable;
  }
  val construct_dfa: regex_expr -> char_expr list -> dfa_structure
end

module RE2:Impl= struct
  (** [simplify expr] simplifyes the [expr] expression, and flattens right the product expressions*)
  let rec simplify expr =
    match expr with
    | Product (a, b) when a=Eps -> simplify b
    | Product (a, b) when b=Eps -> simplify a
    | Product (a, b) when a=Empty || b=Empty -> Empty
    | Union (a, b) -> 
      let na = simplify a in 
      let nb = simplify b in 
      if(na=nb) then na else if(na=Empty) then nb else if (nb=Empty) then na else Union (na, nb)
    | Product (Product (a, b), c) -> 
        simplify (Product(a, Product (b, c)) ) (* Flatten right *)
    | Product (a, b) -> Product (simplify a, simplify b)
    | Star a -> Star (simplify a)
    | _ -> expr
 
  (** [derivate expr char] Calculates the Brzozowski derivative of [expr] with respect to [char] *)
  let rec derivate exp char= 
    let rec nullable r =
      match r with
      | Empty -> false
      | Eps -> true
      | Character _ -> false
      | Union (r1, r2) -> nullable r1 || nullable r2
      | Product (r1, r2) -> nullable r1 && nullable r2
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
    | Star a -> simplify (Product(derivate a char, Star a))
  
  type dfa_structure = {
    dfa_alphabet: char_expr list;
    start: int;
    accept: int list;
    empty: int;
    table: LookupTable.lookupTable;
  }
  
  (** [eq expr1 expr2] returns true if [expr1] and [expr2] are equivalent regular expressions*)
  let rec eq exp1 exp2 = match simplify exp1 with
    | Eps -> begin 
      match  exp2 with
        | Eps -> true
        | _ -> false
      end
    | Empty -> begin
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
  
  let construct_dfa exp alphabet = 
    let rec compute_dfa_states st seen result = 
      if search st seen eq
        then  (seen, result)
      else 
        let to_alph = List.map (fun l -> derivate st l) alphabet in
        List.fold_left (fun (sn, rt) x -> compute_dfa_states x sn rt) (st::seen, to_alph::result) to_alph
    in
    let (states, table) = compute_dfa_states exp [] [] in
    let state_array = Array.of_list states in 
    let simplified_table = LookupTable.construct_table table state_array eq in
    let rec aux_filter = function 
      | Empty -> false
      | Character _-> false
      | Star _ -> true
      | Eps -> true
      | Union (a, b) -> if(aux_filter a || aux_filter b) then true else false
      | Product (a, b) -> if(aux_filter a && aux_filter b) then true else false
      
    in
    {
      dfa_alphabet = alphabet;
      start = Option.get (Array.find_index (fun x-> eq x exp) state_array);
      accept = List.map (fun y->Option.get (Array.find_index (fun x-> eq x y) state_array) ) ( List.filter aux_filter states);
      empty = Option.get (Array.find_index (fun x->eq x Empty) state_array);
      table = simplified_table
    }

end

module RE1:Impl= struct 

  (** Defines a type a state in NFA *)
  type state = int
  
  (** Defines a type to denote a NFA transition *)
  type transition = state*char_expr*state
  
  (** Defines a structure for storing NFA representation of a regular expression *)
  type nfa_struct = {
    start: state;
    last: state;
    transitions: transition list;
  }
  
  (** [regex_to_nfa expr] converts [expr] to a NFA *)
  let regex_to_nfa expr= 
    let empty = {
        start = 0;
        last = 0;
        transitions = [];
    } in
    let rec convert_expr_to_nfa shift= function
      | Empty -> {empty with last=1} 
      | Eps -> {empty with start=shift; last=shift}
      | Character a -> 
        { 
          start=shift;
          last=shift+1; 
          transitions = [(shift, a, shift+1)]
        }
      | Union (a, b) ->  
        let nfa1 = convert_expr_to_nfa (shift+1) a in 
        let nfa2 = convert_expr_to_nfa (nfa1.last+1) b in
        {
          start = shift;
          last =nfa2.last+1;
          transitions = (shift, Epsilon, shift+1)::(shift, Epsilon, nfa1.last+1)::(nfa2.last, Epsilon, nfa2.last+1)::(nfa1.last, Epsilon, nfa2.last)::nfa1.transitions@nfa2.transitions
        }
      | Product (a, b) ->
        let nfa1 = convert_expr_to_nfa (shift) a in 
        let nfa2 = convert_expr_to_nfa (nfa1.last) b in
        {
            start = shift;
            last =nfa2.last;
            transitions = nfa1.transitions@nfa2.transitions
          }
      | Star a ->
        let nfa1 = convert_expr_to_nfa (shift+1) a in 
        {
          start = shift;
          last = nfa1.last+1;
          transitions = (shift,Epsilon, shift+1)::(shift, Epsilon, nfa1.last+1)::(nfa1.last, Epsilon, shift)::nfa1.transitions  
        }
      in convert_expr_to_nfa 0 expr
  
  (** [find_direct_transition n trigger lst acc] calculates the set of all states that can be reached from the given node [n] with the trigger [trigger] given the transition list [lst] and stores value in [acc]*)
  let rec find_direct_transition n trigger lst acc= 
    match lst with
      | [] -> ()
      | (a, b, c)::t when a=n && b=trigger-> 
        Bitarray.add c acc;
        find_direct_transition n trigger t acc
      | _::t -> find_direct_transition n trigger t acc 

  (** [epsilon_closure node transitions l] calculates the epsilon closure of the [node] given the transition table [transitions] and the number of states in the NFA [l] *)
  let epsilon_closure node transitions l= 
    let rec aux tbc acc = 
      if(Bitarray.is_empty tbc) then ()
      else
        let res = Bitarray.empty l in
        find_direct_transition (Bitarray.hd tbc) Epsilon transitions res;
        Bitarray.exclude_from res acc;
        let n = Bitarray.pop_hd tbc in
        let new_tbc = Bitarray.merge tbc res in
        Bitarray.add n acc;
        aux new_tbc acc
    in 
      let tbc = Bitarray.empty l in 
      Bitarray.add node tbc;
      let acc = Bitarray.empty l in 
      aux tbc acc;
      acc
  
  (** [apply_transition_for_nfa n trigger transitions l] returns the list of states that can be reached from [n] with trigger [trigger] given the transitions table [transitions] and the number of states in the NFA [l]*)
  let apply_transition_for_nfa n trigger transitions l= 
    let enc = epsilon_closure n transitions l in 
    let dir = Bitarray.fold_left (fun acc x -> 
      let res = Bitarray.empty l in 
      find_direct_transition x trigger transitions res;
      Bitarray.merge acc res) (Bitarray.empty l) enc in
    Bitarray.fold_left (fun acc x -> Bitarray.merge acc (epsilon_closure x transitions l)) (Bitarray.empty l) dir
  
  type dfa_structure = {
    dfa_alphabet : char_expr list;
    start : int;
    accept : int list;
    empty: int;
    table : LookupTable.lookupTable;
  }

  (** [apply_for_dfa s trigger nfa] calculates the state reached from state [s] with trigger [trigger] given the NFA [nfa] *)
  let apply_for_dfa s trigger nfa = 
    let res = Bitarray.fold_left (fun acc x -> (Bitarray.merge (apply_transition_for_nfa x trigger nfa.transitions (nfa.last+1)) acc)) (Bitarray.empty (nfa.last+1)) s in
    Bitarray.fold_left (fun acc v->  Bitarray.merge (epsilon_closure v nfa.transitions (nfa.last+1)) acc) (Bitarray.empty (nfa.last+1)) res

  let construct_dfa expr alphabet=
    let nfa = regex_to_nfa expr in
    let rec compute_dfa_states st seen result = 
      if search st seen Bitarray.eq then (seen, result)
      else 
        let to_alph = List.map (fun a -> apply_for_dfa st a nfa ) alphabet in
        List.fold_left  (fun (seen, result) x ->  compute_dfa_states x seen result) (st::seen, to_alph::result) to_alph
      in
    let (states, table) = compute_dfa_states (epsilon_closure nfa.start nfa.transitions (nfa.last+1)) [] [] in
    let state_array = Array.of_list states in 
    let simplified_table = LookupTable.construct_table table state_array Bitarray.eq in

    {
      dfa_alphabet=alphabet;
      start= Option.get (Array.find_index (fun x->Bitarray.eq x (epsilon_closure nfa.start nfa.transitions (nfa.last+1))) state_array );
      accept=List.map (fun y -> Option.get (Array.find_index (fun x-> Bitarray.eq x y) state_array)) (List.filter (fun l-> Bitarray.contains nfa.last l) states);
      empty = Option.get (Array.find_index (fun x->Bitarray.is_empty x) state_array);
      table=simplified_table
    }

end