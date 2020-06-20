exception Stack_error of string;;

let binop op = function
  | b::a::r -> (op a b)::r
  | _ -> failwith "invalid expression"   
(* interp : float list -> string -> string * float list *)
let interp s = function
  | "+" -> "add",    binop ( +. ) s
  | "-" -> "subtr",  binop ( -. ) s
  | "*" -> "mult",   binop ( *. ) s
  | "/" -> "divide", binop ( /. ) s
  | "^" -> "exp",    binop ( ** ) s
  | str -> "push", (float_of_string str) :: s
 
(* interp_and_show : float list -> string -> float list *)
let interp_and_show s inp =
  let op,s' = interp s inp in
  Printf.printf "%s\t%s\t" inp op;
  List.(iter (Printf.printf "%F ") (rev s'));
  print_newline ();
  s'
 
    let rec go value = 
    if value then (
    try
    print_string "Enter a postfix notation to calculate: ";
    let input = read_line() in
    if input="exit" then go false else
    let commandList = Str.split (Str.regexp " +") input in
    print_string "\n";
    go true;
    with Stack_error s -> print_endline s; go true;
    )
    else ();;
    
    print_endline "Type 'exit' (lowercase) to quit the program";
    go true;;