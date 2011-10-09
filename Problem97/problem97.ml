module A = struct

  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( - ) = sub_big_int;;
  let ( = ) = eq_big_int;;
  let bi = big_int_of_int;;

  let pow x n =
    let x' = bi x
    and n' = bi n in
    let rec pow ret = function
      | n when n = zero_big_int -> ret
      | n -> pow (x'*ret) (n - unit_big_int)
    in
    pow unit_big_int n'
  ;;

  let test () = 
    begin
      Printf.printf "%s\n" (string_of_big_int ((bi 28433)*(pow 2 7830457)+unit_big_int));
      ();
    end
  ;;

end

let () = A.test ();;
