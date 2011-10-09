module A = struct
  
  open Big_int

  let square x = mult_big_int x x;;
  let is_even x = eq_big_int (mod_big_int x (big_int_of_int 2)) zero_big_int;;
  let half_of x = div_big_int x (big_int_of_int 2);;

  let rec pow x n =
    match n with
    | n when eq_big_int n zero_big_int -> unit_big_int
    | n when eq_big_int n unit_big_int -> x
    | n when is_even n -> pow (square x) (half_of n)
    | n -> mult_big_int x (pow (square x) (half_of n))
  ;;

  let takeN str n =
    let rec takeN ret = function
      | 0 -> (str.[0] :: ret)
      | n -> takeN (str.[n] :: ret) (n-1)
    in
    takeN [] (n-1)
  ;;

  let clist_of_string str =
    let len = String.length str in
    takeN str len
  ;;

  let test () = 
    let czero = int_of_char '0' in
    let numbers = List.map (fun c -> (int_of_char c) - czero) in
    let str = string_of_big_int (pow (big_int_of_int 2) (big_int_of_int 1000)) in
    begin
      Printf.printf "%s\n" (string_of_big_int (pow (big_int_of_int 2) (big_int_of_int 5)));
      Printf.printf "%s\n" str;
      Printf.printf "%d\n" (List.fold_left (+) 0 (numbers (clist_of_string str)));
    end
  ;;
end


let () = A.test ();;
