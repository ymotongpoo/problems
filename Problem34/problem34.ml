module A = struct

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;
  
  let fact n =
    let rec fact ret = function
      | n when n <= 0 -> ret
      | n -> fact (n*ret) (n-1)
    in
    fact 1 n
  ;;

  let num_to_digits num =
    let digits = (String.length (string_of_int num)) - 1 in
    let rec num_to_digits ret n num =
      match n with
      | n when n < 0 -> ret
      | n -> 
          let d = num / (pow 10 n) in
          num_to_digits (d::ret) (n-1) (num - d * (pow 10 n))
    in
    num_to_digits [] digits num
  ;;

  let sum_of_facts num =
    List.fold_left (+) 0 (List.map (fun x -> fact x) (num_to_digits num))
  ;;

  let find_fun_numbers limit =
    let rec find ret = function
      | x when x = limit -> ret
      | x -> 
          if x = sum_of_facts x
          then find (x::ret) (x+1)
          else find ret (x+1)
    in
    find [] 3
  ;;

  let limit = pow 10 (List.length (num_to_digits (fact 9)) + 2);;

  let test () = 
    begin
      Printf.printf "%d\n" limit;
      let result = find_fun_numbers limit in
      List.map (fun x -> Printf.printf "%d " x) result;
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left (+) 0 result);
      ();
    end
  ;;

end

let () = A.test ();;

