module A = struct

  let limit = 10000;;

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
      | _, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, xs when (r*r) > (List.hd (List.rev xs)) ->
          List.rev_append (r::rs) xs
      | ret, x::xs ->
          sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (numbers n))
  ;;

  let pow_int x n = int_of_float ((float_of_int x) ** (float_of_int n));;
          
  let dividors ps n =
    let rec n_div ret d x =
      if (x mod d) = 0 then n_div (List.map (fun x -> x+1) (0::ret)) d (x/d)
      else (x, List.map (fun n -> pow_int d n) (0::ret))
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
    | [] -> [1]
    | [ds] -> List.tl (List.sort (fun x y -> y - x) ds)
    | ds1::ds2::dss -> prod_divs ((cross_divs [] ds1 ds2)::dss)
  ;;

  let print_list l = List.map (fun x -> Printf.printf "%d " x) l;;

  

  let rec find_amicable ret nums =
    let sum_ds n = List.fold_left (+) 0 (prod_divs (dividors (primes limit) n)) in
    match nums with
    | [] -> ret
    | n::ns ->
        let sum = sum_ds n in
        if sum_ds sum = n && sum <> n
        then find_amicable (n::ret) (List.filter (fun x -> x <> n) ns)
        else find_amicable ret ns
  ;;

  let test () = 
    begin
      (* print_list (List.flatten (dividors (primes 1000) 220)); *)
      let _ = print_list (find_amicable [] (numbers limit)) in
      let _ = Printf.printf "\n" in
      let _ = Printf.printf "%d\n" (List.fold_left (+) 0 (find_amicable [] (numbers limit))) in
      ();
    end

end

let () = A.test ();;
