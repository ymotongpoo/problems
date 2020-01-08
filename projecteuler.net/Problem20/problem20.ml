module A = struct

  open Big_int

  let fact n =
    let rec fact ret = function
      | n when eq_big_int n zero_big_int -> ret
      | n -> fact (mult_big_int n ret) (pred_big_int n)
    in
    fact unit_big_int n
  ;;

  let sum_all_digits n =
    let zero = int_of_char '0' in
    let str = string_of_big_int n in
    let rec sum ret = function
      | 0 -> ((int_of_char str.[0]) - zero) + ret
      | i -> sum (((int_of_char str.[i]) - zero) + ret) (i-1)
    in
    sum 0 (String.length str - 1)
  ;;

  let test () =
    Printf.printf "%d\n" (sum_all_digits (fact (big_int_of_int 100)))
  ;;

end

let () = A.test ();;
