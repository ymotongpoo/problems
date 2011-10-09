module A : sig
  
  val filename : string

  val split_to_num : string -> int list

  val load_triangle : string -> int list list

  val test : unit -> unit

end = struct

  let filename = "triangle.txt"

  let split_to_num line = 
    let items = Str.split (Str.regexp "[ \t]") line in
    List.map (fun x -> int_of_string x) items
  ;;

  let load_triangle filename = 
    let cin = open_in filename in
    let triangle = ref [] in
    try
      while true; 
      do
        let line = input_line cin in
        triangle := (split_to_num line) :: !triangle
      done; []
    with
      End_of_file ->
        close_in cin;
        List.rev !triangle
  ;;

  let traverse_all_path triangle =
    let add list1 list2 = List.map2 (fun x y -> x+y) list1 list2 in
    let merge upper lower =
      let lalign = add (0::upper) lower
      and ralign = add (List.rev (0::(List.rev upper))) lower
      in
      List.map2 (fun x y -> if x > y then x else y) lalign ralign
    in
    let rec succ_step = function
      | x::[] -> x
      | x1::x2::xs -> succ_step ((merge x1 x2)::xs)
      | [] -> failwith "succ_step"
    in
    succ_step triangle
  ;;

  let max list = 
    let rec max curr = function
      | [] -> curr
      | x::xs -> if x > curr then max x xs else max curr xs
    in
    max 0 list
  ;;
      
  let solver filename = 
    let triangle = load_triangle filename in
    let list_str list = List.fold_left (fun x y -> x ^ string_of_int y ^ " ") "" list in
    let last_line = traverse_all_path triangle in
    let _ = Printf.printf "%s\n" (list_str last_line) in
    let _ = Printf.printf "%d\n" (max last_line) in
    ()
  ;;

  let test () = solver filename;;

end


let _ = A.test ()
