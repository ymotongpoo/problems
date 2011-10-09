module A = struct

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;

  let num_to_digits num =
    let digits = String.length (string_of_int num) in
    let rec num_to_digits ret n num =
      match n with
      | n when n < 0 -> ret
      | n -> 
          let d = num / (pow 10 n) in
          num_to_digits (d::ret) (n-1) (num - d * (pow 10 n))
    in
    num_to_digits [] digits num
  ;;

  let sum_digits_nth_pow n num =
    let digits = num_to_digits num in
    List.fold_left (+) 0 (List.map (fun x -> pow x n) digits)
  ;;

  let find_fun_numbers limit n =
    let rec find ret = function
      | x when x = limit -> ret
      | x -> 
          if x = sum_digits_nth_pow n x 
          then find (x::ret) (x+1)
          else find ret (x+1)
    in
    find [] 2
  ;;

  let test () = 
    begin
      List.map (fun x -> Printf.printf "%d " x) (find_fun_numbers (pow 10 7) 5);
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left (+) 0 (find_fun_numbers (pow 10 7) 5));
      ();
    end
  ;;

end

let () = A.test ();;
