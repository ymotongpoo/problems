(* -*- coding: utf-8 -*- *)
(**
   Problem 89

   Romans numerals: http://projecteuler.net/faq=roman_numerals
   I = 1
   V = 5
   X = 10
   L = 50
   C = 100
   D = 500
   M = 1000
*)


module A : sig

  val roman_chars : (char * int) list
    (** [(char, num);..] is a map between roman chars and numbers *)

  val limit_nums : (char * int) list
    (** [(char, num);..] is a map of roman chars and limit appearance *)

  val load_roman : string -> string list

  val roman_to_num : string -> int
    (** [roman_to_num roman] returns corresponding integer of 
        roman numerals *)

  val min_length : int -> int
    (** [min_length num] returns length of minimum form of Roman numerals *)

  val count_over_chars : string -> int

  val test : unit -> unit

end = struct

  let filename = "roman.txt"

  let roman_chars = 
    [('I', 1); ('V', 5); ('X', 10); ('L', 50); ('C', 100); ('D', 500); ('M', 1000)];;

  let limit_nums =
    [('I', 3); ('V', 1); ('X', 3); ('L', 1); ('C', 3); ('D', 1); ('M', 99)];;

  let load_roman filename = 
    let cin = open_in filename in
    let romans = ref [] in
    try
      while true; 
      do
        let line = input_line cin in
        romans := line :: !romans
      done; []
    with
      End_of_file ->
        close_in cin;
        List.rev !romans
  ;;

  let roman_to_num roman =
    let num = ref 0 in
    let prev = ref 'I' in
    try
      for i = 0 to String.length roman - 1 do
        if i = 0 then
          begin
            num := !num + List.assoc roman.[i] roman_chars;
            prev := roman.[i]
          end
        else
          let p = List.assoc !prev roman_chars
          and c = List.assoc roman.[i] roman_chars in
          if p >= c 
          then num := !num + c
          else num := !num - p * 2 + c;
          prev := roman.[i]
      done;
      !num
    with
    | Not_found ->
        Printf.printf "  convert (%d) %s\n%!" !num roman;
        !num
  ;;
  
  let min_length num =
    let min_form_len = function
      | n when n = 9 -> 2
      | n when n = 4 -> 2
      | n -> (n mod 5) + (n / 5)
    in
    let m = num / 1000 
    and c = min_form_len ((num mod 1000) / 100)
    and x = min_form_len ((num mod 100) / 10)
    and i = min_form_len (num mod 10) in
    (* let () = Printf.printf "   %d -> %d\n%!" num (m+c+x+i) in *)
    m + c + x + i
  ;;

  (** confirm appearance of each chars is under max number *)
  let count_over_chars roman =
    let len = String.length roman in
    let ret = len - min_length (roman_to_num roman) in
    if ret != 0 then Printf.printf "%s -> %d (%d over)\n%!" roman len ret
    else ();
    ret
  ;;

  let test () = 
    let romans = load_roman filename in 
    let ret = List.map count_over_chars romans in
    Printf.printf "%d\n%!" (List.fold_left (+) 0 ret)

end;;

A.test ()
