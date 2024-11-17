type char_expr = Epsilon | C of char
  (* Ordinary regular expression *)
type regex_expr = 
  | Empty                             (*Denotes the empty set*)
  | Eps                               (*Denotes empty string*)
  | Character of char_expr            (*Denotes a single character*)
  | Union of regex_expr*regex_expr    (*Denotes a union of regular expressions i.e. first or the second*)
  | Product of regex_expr*regex_expr  (*Denotes a product of regular expressions i.e. first followed by the second *)
  | Star of regex_expr                

let ascii = List.init 255 (fun c -> C (Char.chr c))

module type Impl = sig
  type dfa_state
  type dfa_structure = {
    dfa_alphabet: char_expr list;
    dfa_states: dfa_state array;
    dfa_start: dfa_state;
    accept: dfa_state list;
    table: int array array;
  }
  val empty: dfa_state
  val eq: dfa_state->dfa_state->bool
  val construct_dfa: regex_expr -> char_expr list -> dfa_structure
end

module RE2: Impl= struct
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

  let empty = Empty
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
      if Mylist.search st seen eq
        then  (seen, result)
      else 
        let to_alph = List.map (fun l -> derivate st l) alphabet in
        List.fold_left (fun (sn, rt) x -> compute_dfa_states x sn rt) (st::seen, to_alph::result) to_alph
    in
    let (states, table) = compute_dfa_states exp [] [] in
    let state_array = Array.of_list states in 
    let simplified_table = Array.map 
      (fun sl-> 
        Array.map (fun x -> 
          Option.get (Array.find_index (fun y -> eq y x) 
          state_array)) 
        (Array.of_list sl)) 
      (Array.of_list table)  
    in
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
      dfa_states = state_array;
      dfa_start = exp;
      accept = List.filter aux_filter states;
      table = simplified_table
    }

end

module RE1:Impl = struct 
  (* First step: Expression -> NFA *)
  type state = int
  type transition = state*char_expr*state
  type nfa_struct = {
    start: state;
    last: state;
    transitions: transition list;
  }
  let regex_to_nfa = 
    let empty = {
        start = 0;
        last = 0;
        transitions = [];
    } in
    let rec convert_expr_to_nfa shift= function
      | Empty -> {empty with last=1} 
      | Eps -> empty
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
  let empty = []
  let eq = Mylist.eq
  let apply_for_dfa s trigger nfa = 
    let rec res list acc = match list with
      | [] -> acc
      | h::t -> res t (Mylist.mergeSorted (apply_transition_for_nfa h trigger nfa.transitions) acc)
    in List.fold_left (fun acc v->  Mylist.mergeSorted (epsilon_closure v nfa.transitions) acc) [] (res s [])

  let construct_dfa expr alphabet=
    let nfa = regex_to_nfa expr in
    let rec compute_dfa_states st seen result = 
      if Mylist.search st seen eq then (seen, result)
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
          Option.get (Array.find_index (fun y -> y=x) 
          state_array)) 
        (Array.of_list sl)) 
      table)  in  
    {
      dfa_alphabet=alphabet;
      dfa_states = state_array;
      dfa_start= epsilon_closure nfa.start nfa.transitions;
      accept=List.filter (fun l-> contains_state l nfa.last) states;
      table=simplified_table
    }

end