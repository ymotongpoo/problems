module A = struct

  let numbers limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret l =
      match ret, l with
      | _, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, x::xs ->
          if (r*r) > (List.hd (List.rev (x::xs)))
          then List.rev_append (r::rs) (x::xs)
          else sieve (x::r::rs) (sift x xs)
    in
    sieve [] (List.tl (numbers limit))
  ;;

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let num2digits n =
    let max = String.length (string_of_int n) - 1 in
    let rec n2d ret rest = function
      | (-1) -> List.rev ret
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d * (pow 10 n)) (n-1)
    in
    n2d [] n max
  ;;

  let is_pandigital num =
    let digits = String.length (string_of_int num) in
    let jadge source target =
      List.for_all (fun x -> List.mem x target) source
    in
    jadge (numbers digits) (num2digits num)
  ;;

  let collect_pand_primes limit =
    List.filter is_pandigital (primes limit)
  ;;

  let test () =
    begin
      if is_pandigital 2143 then Printf.printf "true\n" else Printf.printf "false\n";
      if is_pandigital 2133 then Printf.printf "true\n" else Printf.printf "false\n";
      Printf.printf "%d\n" (List.hd (List.rev (collect_pand_primes (pow 10 9))));
      ();
    end
  ;;

end

let () = A.test ();;
