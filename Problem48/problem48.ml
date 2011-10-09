module A = struct

  open Big_int

  let pow_self x =
    let rec pow ret = function
      | n when eq_big_int n zero_big_int -> ret
      | n -> pow (mult_big_int x ret) (sub_big_int n unit_big_int)
    in
    pow unit_big_int x
  ;;

  let numbers n =
    let rec nums ret = function
      | n when eq_big_int n zero_big_int -> ret
      | n -> nums (n::ret) (sub_big_int n unit_big_int)
    in
    nums [] n
  ;;

  let sum_pows xs =
    let pows = List.map (fun x -> pow_self x) xs in
    List.fold_left add_big_int zero_big_int pows
  ;;

  let test () =
    Printf.printf "%s\n" (string_of_big_int (sum_pows (numbers (big_int_of_int 1000))))
  ;;

end

let () = A.test ();;
