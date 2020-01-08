(** -*- coding: utf-8 -*- *)
(**
Problem 79

A common security method used for online banking is to ask the user for 
three random characters from a passcode. For example, if the passcode was 531278, 
they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.

The text file, keylog.txt, contains fifty successful login attempts.

Given that the three characters are always asked for in order, 
analyse the file so as to determine the shortest possible secret passcode of unknown length.
*)


module A : sig

  type order
    (** (former, latter) *)

  val keylog_file : string
    (** keylog file name *)

  val load_keylog : string -> string list
    (** [load_keylog filename] returns all keylogs stored in [filename] *)

  val keylog_to_order : string -> order * order
    (** [keylog_to_order keylog] returns a pair of order tuples *)

  val accumulate_order : string list -> order list
    (** [accumulate_order keylogs] returns all unique order *)

  val find_leftest : order list -> (char * int) list
    (** [find_leftest orders] returns list of (character, frequency) *)

  val test : unit -> unit

end = struct

  type order = Order of char * char


  let keylog_file = "keylog.txt"


  let load_keylog filename =
    let cin = open_in filename in
    let keylogs = ref [] in
    try
      while true; 
      do
        let line = input_line cin in
        keylogs := line :: !keylogs
      done; []
    with
      End_of_file ->
        close_in cin;
        List.rev !keylogs
  ;;


  let keylog_to_order = function
    | keylog when String.length keylog = 3 -> 
        let c0 = keylog.[0]
        and c1 = keylog.[1]
        and c2 = keylog.[2] in
        (Order(c0, c1), Order(c1, c2))
    | _ -> 
        failwith("not valid keylog")
  ;;
    
  
  let accumulate_order list =
    let rec aux accu = function
      | [] -> accu
      | k::ks ->
          let o1, o2 = keylog_to_order k in
          aux (o1::o2::accu) ks
    in
    let order_list = aux [] list in
    let sift o l = List.filter (fun x -> x <> o) l in
    let rec sieve accu = function
      | [] -> accu
      | o::os ->
          sieve (o::accu) (sift o os)
    in
    sieve [] order_list
  ;;


  let find_leftest order_list =
    let rec find_leftest accu = function
      | [] -> accu
      | (Order(c1, _) :: os) as list ->
          let target = List.filter (fun (Order (x, _)) -> x == c1) list
          and rest = List.filter (fun (Order (x, _)) -> x <> c1) list in
          find_leftest ( (c1, List.length target) :: accu ) rest
    in
    find_leftest [] order_list
  ;;
          

  let print_orders order_list =
    let print_order = function
      | Order(c1, c2) -> Printf.printf "(%c, %c)\n%!" c1 c2
    in
    let _ = List.map print_order order_list in
    ()
  ;;

  let test () =
    let keylogs = load_keylog (keylog_file) in
    let orders = (accumulate_order keylogs) in
    let _ = print_orders orders in
    let _ = List.map 
      (fun (c,n) -> Printf.printf "max : %c --> %d\n%!" c n) (find_leftest orders) in
    ()
  ;;

end 


let _ = A.test ()
