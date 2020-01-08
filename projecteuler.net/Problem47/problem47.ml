module A = struct

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret list =
      match ret, list with
      | [], x::xs -> sieve [x] (sift x xs)
      | _, [] -> List.rev ret
      | r::rt, xs when (r*r) > List.hd (List.rev xs) ->
          List.rev_append ret list
      | ret, x::xs ->
          sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  
  let count_prime_factor ps n =
    let rec count ctr ps num =
      match ps, num with
      | [], _ | _, 0 | _, 1 -> ctr
      | p::ps, n when (n mod p) = 0 ->
          if not (List.mem p ctr)
          then count (p::ctr) (p::ps) (n/p)
          else count ctr (p::ps) (n/p)
      | p::ps, n ->
          count ctr ps n
    in
    List.length (count [] ps n)
  ;;


  let solver ps cons_num pf_num =
    let rec find ans ctr n = 
      match ctr, n with
      | ctr, _ when ctr = cons_num -> ans
      | _, n when (count_prime_factor ps n) = pf_num ->
          if ctr = 0 then find n 1 (n+1)
          else find ans (ctr+1) (n+1)
      | _, n ->
          find ans 0 (n+1)
    in
    find 0 0 2
  ;;

  let test () =
    begin
      let ps = primes 10000 in
      Printf.printf "%d\n" (solver ps 4 4);
    end
  ;;

end

let _ = A.test ();;
