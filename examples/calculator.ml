open Lib.Engine

(* Lexing *)

type token = 
  | Number of float
  | Comment 
  | WhiteSpace
  | Equal
  | Star
  | Divide
  | Plus
  | Minus
  | LParen
  | RParen



let token_mapping = [
  ("\\-?[0-9]+(\\.[0-9]*)?", (fun lexeme->Number (float_of_string lexeme)));
  ("//.*//", (fun _ -> Comment));
  ("( |\t|\n)*", (fun _->WhiteSpace));
  ("=", (fun _->Equal));
  ("\\+", (fun _->Plus));
  ("\\*", (fun _->Star));
  ("\\/", (fun _->Divide));
  ("\\-", (fun _->Minus));
  ("\\(", (fun _->LParen));
  ("\\)", (fun _->RParen))
  ]

let machines = List.map (fun (x,y) ->(Engine2.create_machine x, y)) token_mapping

let lex s = 
  let rec aux s t = match t with
    []->failwith "No matching construct"
    |(machine, fn)::tail -> let i = Engine2.longest_match machine s in
        if i<>0 then  begin
          fn (String.sub s 0 i), (String.sub s i (String.length s - i)) 
        end
      else aux s tail
  in 

  let rec loop st acc=
    if String.length st = 0 then acc else
    let tkn, next = aux st machines in
    match tkn with
    | Comment|WhiteSpace -> loop next acc
    | _->loop next (tkn::acc)

  in List.rev (loop s [])

(* Parsing *)

(* Defining the CFG 

main-> factor+main | factor-main | factor
factor-> simple*factor  | simple/factor | simple
simple-> (main) | Number

*)

type ast = Product of ast*ast| Quotient of ast*ast|Sum of ast*ast | Value of float

let parse s= 
  let rec main s= 
    let (expr, rest) = factor s in
    match rest with 
      |[] -> (expr, [])
      |Plus::t-> 
        let (new_expr, new_rest) = main t in 
        Sum (expr, new_expr), new_rest
      |Minus::t->
        let (new_expr, new_rest) = main t in 
        Sum (Product (Value (-1.), expr), new_expr), new_rest
      |_->expr, rest
  and factor s= 
    let (expr, rest) = simple s in
    match rest with
        |[]->(expr, [])
        |Star::t->
          let (new_expr, new_rest) = factor t in 
          Product (expr, new_expr), new_rest
        |Divide::t->
          let (new_expr, new_rest) = factor t in 
          Quotient (expr, new_expr), new_rest
        |_->expr, rest
  and simple s= 
    match s with 
      |[]->failwith "Invalid Expression"
      |LParen::t ->
        let expr, rest = main t in 
        if List.hd rest <>RParen then failwith "Invalid Expression" else 
          expr, List.tl rest
      |Number n::t-> Value n, t
      |_->failwith "Invalid Expression"
in 
let result, rest = main s in
if(rest<>[]) then failwith "Error in parsing"
else result

(* Evaluation *)

let rec evaluate expression = match expression with
  | Value v->v
  | Product (a, b)-> (evaluate a) *. (evaluate b)
  | Sum (a, b) -> (evaluate a) +. (evaluate b)
  | Quotient (a, b) -> (evaluate a) /. (evaluate b)


let expression = "85* //this is a comment yeee// (36.8+90*7)"


let () = Printf.printf "%f\n" (evaluate (parse (lex (expression))))
