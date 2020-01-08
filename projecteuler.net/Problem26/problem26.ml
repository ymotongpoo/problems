(**
   referred about nature of cyclic number
   
   http://en.wikipedia.org/wiki/Midy's_theorem
   http://en.wikipedia.org/wiki/Carmichael_function
   http://en.wikipedia.org/wiki/Repeating_decimal
   http://en.wikipedia.org/wiki/Cyclic_number

   Carmichael function lambda(n) gives smallest m which a^m mod n = 1.
   
   for prime p:
     lambda(p^k) = p^(k-1)*(p - 1)

   and similarly:
     lambda(p1^k1 * p2^k2 * ...) = lcm( lambda(p1^k1), lambda(p2^k2), ... )


   and length of repeating digits are given by smallest "a" which satisfy following equation.
   http://web2.incl.ne.jp/yaoki/jyunnkan.htm

     10^a mod p = 1
*)

module A = struct

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret list =
      match ret, list with
      | _, [] -> List.rev ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, xs when (r*r) > (List.hd (List.rev xs)) -> List.rev_append ret xs
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let pf n =
    let ps = primes 1000 in
    let rec pf ret n rest divs =
      match rest, divs with
      | _, [] -> ret
      | 1, p::ps -> (p, n)::ret
      | r, p::ps ->
          if (r mod p) = 0 then pf ret (n+1) (rest/p) (p::ps)
          else if n <> 0 then pf ((p, n)::ret) 0 rest ps
          else pf ret 0 rest ps
    in
    pf [] 0 n ps
  ;;


  let lcm x1 x2 =
    let xs1 = pf x1 and xs2 = pf x2 in
    let rec lcm ret xs1 xs2 =
      match xs1, xs2 with
      | xs, [] | [], xs -> xs @ ret
      | (p1, k1)::xss1, xs2 when List.mem_assoc p1 xs2 ->
          let k2 = List.assoc p1 xs2 in
          if k1 > k2 
          then lcm ((p1, k1) :: ret) xss1 (List.remove_assoc p1 xs2)
          else lcm ((p1, k2) :: ret) xss1 (List.remove_assoc p1 xs2)
      | x1::xss1, _ -> lcm (x1::ret) xss1 xs2
    in
    List.fold_left (fun x (p,k) -> x * (pow p k)) 1 (lcm [] xs1 xs2)
  ;;
  
  let rec lcm_all = function 
    | [] -> 0
    | [x] -> x
    | x1::x2::xs -> lcm_all ((lcm x1 x2)::xs)
  ;;

  let carmichael n =
    let carmichael' (p, k) = 
      match p, k with
      | p, k when p >= 3 || k <= 2 -> (pow p (k-1))*(p-1) 
      | p, k when p = 2 && k >= 3 -> pow 2 (k-2)
      | _, _ -> 0
    in
    (n, lcm_all (List.map (fun (p, k) -> carmichael' (p, k)) (pf n)))
  ;;

  let roller limit =
    let rec roller max maxcyc = function
      | n when n = limit -> (max, maxcyc)
      | n ->
          let (num, cycle) = carmichael n in
          if cycle > maxcyc
          then roller num cycle (n+1)
          else roller max maxcyc (n+1)
    in
    roller 0 0 3
  ;;

  let print_tuple (a, b) =
    Printf.printf "(%d, %d)\n" a b
  ;;

  let test () = 
    begin
      print_tuple (carmichael 983);
      print_tuple (carmichael 997);
      print_tuple (roller 1000);
    end
  ;;

end


module B = struct
  
  open Big_int

  let ( * ) = mult_big_int;;
  let ( + ) = add_big_int;;
  let ( / ) = div_big_int;;
  let ( - ) = sub_big_int;;
  let (mod) = mod_big_int;;
  let ( = ) = eq_big_int;;

  let bi = big_int_of_int;;
  let ib = int_of_big_int;;
  let sb = string_of_big_int;;

  let pow x n =
    let rec pow ret = function
      | n when n = zero_big_int -> ret
      | n -> pow (x*ret) (n-unit_big_int)
    in
    pow unit_big_int n
  ;;

  let repeating_dig n =
    let rec repeating_dig a =
      if (pow (bi 10) a) mod n = unit_big_int then ib a
      else repeating_dig (a+unit_big_int)
    in
    repeating_dig unit_big_int
  ;;

  let test () =
    begin
      Printf.printf "%d\n" (repeating_dig (bi 7));
      Printf.printf "%d\n" (repeating_dig (bi 983));
      Printf.printf "%d\n" (repeating_dig (bi 997));
    end
  ;;

end


let _ = B.test ();;
