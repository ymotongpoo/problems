module A = struct

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;

  let fraction limit =
    let rec f ret = function
      | n when n > limit -> ret
      | n -> f (ret ^ (string_of_int n)) (n+1)
    in
    f "" 1
  ;;

  let nth_num str n = (int_of_char str.[n-1]) - (int_of_char '0');;

  let bases n =
    let rec b ret = function
      | (-1) -> ret
      | n -> b ((pow 10 n)::ret) (n-1)
    in
    b [] n
  ;;

  let test () =
    begin
      let depth = 6 in
      let digits = 
        let frac_str = fraction (pow 10 depth) in
        List.map (fun x -> nth_num frac_str x) (bases depth)
      in
      let _ = List.map (fun x -> Printf.printf "%d " x) digits in
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left ( * ) 1 digits);
    end
  ;;
    
end

let () = A.test ();;
