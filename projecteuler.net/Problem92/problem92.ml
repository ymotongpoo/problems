(**
Problem 89

A number chain is created by continuously adding the square of the digits
in a number to form a new number until it has been seen before.

For example,

44->32->13->10->1->1
85->89->145->42->20->4->16->37->58->89

Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop. 
What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.

How many starting numbers below ten million will arrive at 89?
*)

module A = struct

  (** pow : int -> int *)
  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (pred n))
  ;;


  (** sum_sq_digits : int -> int *)
  let sum_sq_digits n =
    let char_to_int c = (int_of_char c) - (int_of_char '0') in
    let num_to_digits n = 
      let str_n = string_of_int n in
      let len = String.length str_n
      and ds = ref [] in
      for i = len - 1 downto 0 do
        ds := (char_to_int str_n.[i])::(!ds);
      done;
      !ds
    in
    List.fold_left (fun x y -> x + y*y) 0 (num_to_digits n)
  ;;


  (** is_89_loop : int -> bool *)
  let rec is_89_loop n =
    let s = sum_sq_digits n in
    match s with
    | 89 -> true
    | 1 -> false
    | _ -> is_89_loop s
  ;;


  (** base : int -> int list *)
  let base limit =
    let rec base accu = function
      | n when n > limit -> accu
      | n -> 
          if is_89_loop n
          then base (n::accu) (succ n)
          else base accu (succ n)
    in
    base [] 1
  ;;


  let solver limit =
    let max = 9 * 9 * (int_of_float (log10 (float_of_int limit))) in
    let result = base max in
    let rec finder accu = function
      | n when n > limit -> accu
      | n -> 
          if List.mem (sum_sq_digits n) result
          then finder (n::accu) (succ n)
          else finder accu (succ n)
    in
    List.rev_append (finder [] (max+1)) result
  ;;

  let print_bool b = if b then prerr_endline "true" else prerr_endline "false";;

  let test () = Printf.printf "%d\n%!" (List.length (solver (pow 10 7)));;
  (* 
  let test () = 
    begin
      print_bool (is_89_loop 145);
      print_bool (is_89_loop 130);
    end
  ;;
  *)
  

end

let _ = A.test ();;
