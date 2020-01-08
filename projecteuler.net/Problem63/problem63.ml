module A = struct

  open Big_int

  let pow = power_int_positive_int;;

  let is_ndigit n num =
    ge_big_int num (pow 10 (n-1))
    && lt_big_int num (pow 10 n)
  ;;

  let rec solver ret n i =
    match n, i with
    | n, 9 when lt_big_int (pow 9 n) (pow 10 (n-1)) -> ret
    | _, i when i > 9 -> solver ret (succ n) 2
    | n, i ->
        if is_ndigit n (pow i n)
        then solver ((pow i n)::ret) n (succ i)
        else solver ret n (succ i)
  ;;

  let test () =
    let _ = 
      begin
        let result = solver [] 1 1 in
        Printf.printf "%d\n" (List.length result);
      end
    in
    ()
  ;;

end

let _ = A.test ();;
