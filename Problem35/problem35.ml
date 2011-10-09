module A = struct

  let limit = 1000000;;

  let numbers n = 
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] n
  ;;

  let primes n =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret ps =
      match ret, ps with
      | _, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, ps when (r*r) > (List.hd (List.rev ps)) ->
          List.rev_append (r::rs) ps
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (numbers n))
  ;;

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;

  let circulate n =
    let len = String.length (string_of_int n) - 1 in
    let rotate n =
      let base = pow 10 len in
      let top = n / base 
      and rest = n mod base in
      rest * 10 + top
    in
    let rec circulate ret n = 
      match ret with
      | r when (List.length r) >= (len+1) -> r
      | _ -> 
          circulate (n::ret) (rotate n)
    in
    circulate [] n
  ;;

  let find_circ_prime ps =
    let rec check_all ps = function
      | [] -> true
      | c::cs ->
          if not (List.exists (fun x -> x=c) ps) then false
          else check_all ps cs
    in
    let rec exclude ps = function
      | [] -> ps
      | c::cs -> exclude (List.filter (fun x -> x <> c) ps) cs
    in
    let rec find ret = function
      | [] -> ret
      | p::ps ->
          let circ = circulate p in
          if check_all (p::ps) circ
          then find (List.append circ ret) (exclude ps circ)
          else find ret (exclude ps circ)
    in
    find [] ps
  ;;

  let test () =
    begin
      List.map (fun x -> Printf.printf "%d " x) (circulate 12345);
      Printf.printf "\n";
      List.map (fun x -> Printf.printf "%d " x) (find_circ_prime (primes 1000));
      Printf.printf "\n";
      Printf.printf "%d\n" (List.length (List.map (fun x -> Printf.printf "%d " x) (find_circ_prime (primes limit))) - 1);
      ();
    end
  ;;

end

let () = A.test ();;
