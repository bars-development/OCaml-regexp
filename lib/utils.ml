module Bitarray = struct
  let empty n = 
    let size =(n+7)/8 in  
    Bytes.create size
  let is_empty array = 
    let rec nth_empty index =
      if index >= Bytes.length array then true 
      else if Bytes.get_uint8 array index <> 0 then false 
      else nth_empty (index + 1)
    in
    nth_empty 0
  
  let add value array = 
    let byte_index = value / 8 in
    let bit_index = value mod 8 in
    if(byte_index>=Bytes.length array) then failwith ((string_of_int (Bytes.length array))^"is the length of the array, got "^(string_of_int value)) else
    let byte = Bytes.get_uint8 array byte_index in
    Bytes.set_uint8 array byte_index (byte lor (1 lsl bit_index))

  let delete value array = 
    let byte_index = value / 8 in
    let bit_index = value mod 8 in
    let byte = Bytes.get_uint8 array byte_index in
    Bytes.set_uint8 array byte_index (byte land (lnot (1 lsl bit_index)))
  let eq = Bytes.equal

  let hd array= 

    let rec aux index =
      if index >= Bytes.length array then 
        failwith "Empty bit array"
      else if Bytes.get_uint8 array index <> 0 then 
        begin
          let byte = Bytes.get_uint8 array index in 
          let rec loop i = 
            if(i=8) then failwith "ERROR!" else
            if (byte land (1 lsl i)) <> 0 then (index*8 + i )
            else loop (i+1) 
          in
          loop 0
        end

      else aux (index + 1)

    in aux 0
  let pop_hd array= 
    let r = hd array in
    delete r array;
    r


  let contains value array =
    let byte_index = value / 8 in
    let bit_index = value mod 8 in
    let byte = Bytes.get_uint8 array byte_index in
    (byte land (1 lsl bit_index)) <> 0


  let merge a b = 
    let s = Bytes.length a in
    let result = Bytes.create s in 
    for i=0 to s-1 do
      Bytes.set_uint8 result i ((Bytes.get_uint8 a i) lor (Bytes.get_uint8 b i))
    done;
    result
  
  let exclude_from a b = 
    let s = Bytes.length a in
    for i=0 to s-1 do
      Bytes.set_uint8 a i (((Bytes.get_uint8 a i) lxor (Bytes.get_uint8 b i)) land (Bytes.get_uint8 a i))
    done;
    ()
  
  let fold_left f init array = 
    let acc = ref init in
    for i=0 to (Bytes.length array)-1 do
      if(Bytes.get_uint8 array i <> 0) then
      for j=0 to 7 do 
        if(contains (i*8+j) array) then acc := f !acc (i*8 +j);
      done;
    done;
    !acc
  let from_list l len= 
    let res = empty len in
    let rec aux lst=
      match lst with
        |[]->()
        |h::t -> add h res;aux t
    in aux l;
    res
  let to_list array =  
    (* let acc = ref init in *)
    let l = ref [] in
    for i=0 to (Bytes.length array)-1 do
      if(Bytes.get_uint8 array i <> 0) then
      for j=0 to 7 do 
        if(contains (i*8+j) array) then l := (i*8+j)::!l;
      done;
    done;
    List.rev !l

end

module MyList = struct
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


end

let rec search state state_list equality= 
  match state_list with
    | []->false
    | h::_ when equality state h -> true
    | _::t-> search state t equality
