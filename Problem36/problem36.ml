module A = struct

  let limit = 1000000;;

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;

  let decimal2binary num =
    let rec bits ret = function
      | 0 -> 0::ret
      | 1 -> 1::ret
      | n -> bits ((n mod 2)::ret) (n/2)
    in
    bits [] num
  ;;

  let bin_palindromic b =
    let revb = List.rev b in
    List.for_all2 (fun x y -> x = y) b revb
  ;;

  let dec_palindromic d =
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
    in
    let dfwd = num_to_digits d in
    let drev = List.rev dfwd in
    List.for_all2 (fun x y -> x = y) dfwd drev
  ;;

  let find_palindromic limit =
    let rec find ret = function
      | n when n=limit -> ret
      | n when dec_palindromic n ->
          if bin_palindromic (decimal2binary n) 
          then find (n::ret) (n+1)
          else find ret (n+1)
      | n -> find ret (n+1)
    in
    find [] 1
  ;;

  let test () = 
    begin
      List.map (fun x -> Printf.printf "%d " x) (decimal2binary 8);
      Printf.printf "\n";
      List.map (fun x -> Printf.printf "%d " x) (decimal2binary 15);
      Printf.printf "\n";
      let palindromics = find_palindromic limit in
      List.map (fun x -> Printf.printf "%d " x) palindromics;
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left (+) 0 palindromics);
      ();
    end
  ;;

end

let () = A.test ();;
