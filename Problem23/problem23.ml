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
    let rec sieve ret nums =
      match ret, nums with
      | [], x::xs -> sieve [x] (sift x xs)
      | _, [] -> ret
      | r::rs, xs when (r*r) > List.hd (List.rev xs) ->
           List.rev_append (r::rs) xs
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let dividors ps n =
    let rec n_div ret d x =
      if (x mod d) = 0 then n_div (List.map (fun x -> x+1) (0::ret)) d (x/d)
      else (x, List.map (fun n -> pow d n) (0::ret))
    in
    let rec div_list ret ps n =
      match ps, n with
      | [], _ | _, 0 | _, 1 -> ret
      | p::ps, n ->
          let (rest, divs) = n_div [] p n in
          div_list (divs::ret) ps rest
    in
    div_list [] ps n
  ;;

  let rec prod_divs divs =
    let rec cross_divs ret xs1 xs2 =
      match xs1 with
      | [] -> ret
      | x::xs -> cross_divs ((List.map (fun n -> n*x) xs2) @ ret) xs xs2
    in
    match divs with
    | [] -> []
    | [ds] -> List.tl (List.sort (fun x y -> y - x) ds)
    | ds1::ds2::dss -> prod_divs ((cross_divs [] ds1 ds2)::dss)
  ;;

  let limit = 28123;;
  let prime = primes limit;;

  let abundunts limit = 
    let is_abundant num = (List.fold_left (+) 0 (prod_divs (dividors prime num))) > num in
    List.filter (fun x -> is_abundant x) (nums limit)
  ;;

  let roller limit = 
    let abds = abundunts limit in
    let rec mem n = function
      | [] -> false
      | x::xs when x > n -> false
      | x::xs -> if x = n then true else mem n xs
    in
    let rec roller ret = function
      | [] -> ret
      | x::xs -> 
          if List.exists (fun e -> mem (x - e) abds) abds
          then roller ret xs
          else roller (x::ret) xs
    in
    roller [] (nums limit)
  ;;
  
  let test () =
    begin
      let result = roller limit in
      let _ = List.map (fun x -> Printf.printf "%d " x) result in
      Printf.printf "\n";
      let _ = Printf.printf "%d\n" (List.fold_left (+) 0 result) in
      ();
    end
  ;;

end

let _ = A.test ();;
