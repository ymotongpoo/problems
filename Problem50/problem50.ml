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
    let rec sieve ret num =
      match ret, num with
      | [], x::xs -> sieve [x] (sift x xs)
      | _, [] -> ret
      | r::rs, xs when (r*r) > List.hd (List.rev xs) ->
          List.rev_append (r::rs) xs
      | ret, x::xs ->
          sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let rec ps_mem e = function
    | [] -> false
    | x::xs when x > e -> false
    | x::xs -> compare e x = 0 || ps_mem e xs
  ;;


  let rec roller (ctr, max) xs limit =
    let rec cons_sum_prime (ctr, max) c ret limit = function
      | x::xs when ps_mem ret xs -> cons_sum_prime (c, ret) (c+1) (x+ret) limit xs
      | x::xs when (x+ret) < limit -> cons_sum_prime (ctr, max) (c+1) (x+ret) limit xs
      | _ -> (ctr, max)
    in
    match xs with
    | [] -> (ctr, max)
    | ps -> 
        let (c, m) = cons_sum_prime (0, 0) 0 0 limit ps in
        if c > ctr 
        then roller (c, m) (List.tl ps) limit
        else roller (ctr, max) (List.tl ps) limit
  ;;

  let print_tuple (x, y) = Printf.printf "(%d, %d)\n" x y;;

  let test () = 
    begin
      (* let _ = List.map (fun x -> Printf.printf "%d " x) (primes 100) in *)
      Printf.printf "\n";
      let ps = primes 1000000 in
      let limit = List.hd (List.rev ps) in
      print_tuple (roller (0, 0) ps limit);
    end
  ;;

end

let _ = A.test ();;
