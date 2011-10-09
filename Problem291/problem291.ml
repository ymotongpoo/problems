module A = struct

  (**
     x^4 - y^4 
     = (x^2 + y^2) * (x^2 - y^2)
     = (x^2 + y^2) * (x - y) * (x + y)

     x^3 + y^3
     = (x^2 + y^2 - x*y) * (x + y)

     here, define t as y/x then
     (x^4 - y^4) / (x^3 + y^3)
     = y * (t^2 + 1) * (t - 1) / (t^2 - t + 1)
  *)

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret ns =
      match ret, ns with
      | _, [] -> List.rev ret
      | r::rs, xs when (r*r) > List.hd (List.rev xs) ->
          List.rev_append ret ns
      | ret, x::xs ->
          sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let pf ps num =
    let rec pf ret rest ps =
      match rest, ps with
      | _, [] | 1, _ -> ret
      | r, p::ps when r mod p = 0 ->
          pf (p::ret) (r/p) (p::ps)
      | r, p::ps ->
          pf ret r ps
    in
    pf [] num ps
  ;;

  let find_panaitopol ps =
    let rec check num fs =
      match num, fs with
      | n, [m] when List.mem n ps -> true
      | n, p::ps when n mod p = 0 -> check (n/p) ps
      | n, [] when List.mem n ps -> true
      | n, _ -> false
    in
    let max = List.hd (List.rev ps) in
    let rec roller ret n =
      let num1 = 4*n*n+1
      and num2 = 2*n-1
      and den = 2*n*(2*n-1)+1 in
      match num1, num2, den with
      | n1, n2, d when n1*n2/d > max -> ret
      | n1, n2, d when d mod n2 = 0 ->
          if check n1 (pf ps (d/n2))
          then roller (n::ret) (n+1)
          else roller ret (n+1)
      | n1, n2, d when not (List.mem n2 ps) ->
          roller ret (n+1)
      | _, _, _ -> ret
    in
    roller [] 1
  ;;

  let test () = 
    begin
      let limit = 5 * (pow 10 15) in
      let ps = primes limit in
      Printf.printf "*** primes collected ***\n";
      let result = find_panaitopol ps in
      let _ = List.map (fun x -> Printf.printf "%d " x) result in
      Printf.printf "\n";
      Printf.printf "%d\n" (List.length result);
    end
  ;;

end

module B = struct

  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( - ) = sub_big_int;;
  let ( / ) = div_big_int;;
  let (mod) = mod_big_int;;
  let ( = ) = eq_big_int;;
  let ( < ) = lt_big_int;;
  let ( > ) = gt_big_int;;

  let bi = big_int_of_int;;
  let ib = int_of_big_int;;

  let nums limit =
    let rec nums ret = function
      | n when n = zero_big_int -> ret
      | n -> nums (n::ret) (n - unit_big_int)
    in
    nums [] limit
  ;;

  let primes limit = 
    let sift n l = List.filter (fun x -> (x mod n) <> zero_big_int) l in
    let rec sieve ret ns =
      match ret, ns with
      | _, [] -> List.rev ret
      | r::rs, xs when (r*r) > List.hd (List.rev xs) ->
          List.rev_append ret xs
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let test () = ();;

end

let _ = A.test ();;



















