let rec add v lst = 
  match lst with 
    |[] -> v::[]
    |h::t -> if(v<h) then v::lst else if (v=h) then lst else h::(add v t)

let rec eq a b = match a, b with
  |[], [] -> true
  |[], _::_ ->false
  |_::_, [] -> false
  |h1::_, h2::_  when h1<>h2 -> false
  |_::t1, _::t2 -> eq t1 t2

let rec isInSorted x lst = match lst with
  | [] -> false
  | h::t -> if(x=h) then true else if (x>h) then false else isInSorted x t

let rec mergeSorted lst1 lst2 = match lst1, lst2 with
  | [], l -> l
  | l, [] -> l
  | h1::t1, h2::t2 -> 
      if(h1>h2) then add h2 (mergeSorted t2 lst1) else add h1 (mergeSorted lst2 t1)

let rec myFilter f lst = match lst with
    |[]->[]
    |h::t-> if(f h) then add h (myFilter f t) else myFilter f t

let rec myMap f lst = match lst with
    |[] ->[]
    |h::t -> add (f h) (myMap f t)

let rec search state state_list equality= 
  match state_list with
    | []->false
    | h::_ when equality state h -> true
    | _::t-> search state t equality
