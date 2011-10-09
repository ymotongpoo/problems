module A = struct 
    
  let numbers n =
    let rec numbers ret = function
      | 0 -> ret
      | n -> numbers (n::ret) (n-1)
    in
    numbers [] n
  ;;

  let primes n =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret l =
      match ret, l with
      | ret, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, xs when (r*r) > (List.hd (List.rev xs)) ->
          (List.rev (r::rs)) @ xs
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (numbers n))
  ;;

  let calc_qd a b n = (n+a)*n + b;;

  let is_prime result ps =
    let rec is_prime = function
      | [] -> false
      | x::xs -> if x=result then true else is_prime xs
    in
    is_prime ps
  ;;
  
  let count_qd a b ps =
    match b with
    | b when b < 0 -> (a, b, 0)
    | _ -> 
        let rec count ret n =
          let result = calc_qd a b n in
          if (result < 0) || not (is_prime result ps)
          then ret
          else count (ret+1) (n+1)
        in
        (a, b, count 0 0)
  ;;
  
  let limit = 1000;;

  let search_qd ps =
    let rec search_qd ret a b =
      match a, b with
      | a, _ when a = limit -> ret
      | _, b when b = limit -> search_qd ret (a+1) (1-limit)
      | a, b ->
          search_qd ((count_qd a b ps)::ret) a (b+1)
    in
    search_qd [] (1-limit) (1-limit)
  ;;
  
  let rec find_max ret l =
    match ret, l with
    | _, [] -> ret
    | (_, _, mc), (a, b, c)::xs ->
        if c > mc then find_max (a, b, c) xs
        else find_max ret xs
  ;;
  
  let prod_ab (a, b, c) = a*b;;

  (* let max_num = limit*limit*2 + limit;; *)
  let max_num = 100000;;

  let test () =
    begin 
      Printf.printf "%d\n" (prod_ab (find_max (0, 0, 0) (search_qd (primes max_num))));
      ();
    end
  ;;

end

let () = A.test ();;
