module A = struct

  let pow x n =
    let rec pow ret = function
      | 0 -> ret
      | n -> pow (x*ret) (n-1)
    in
    pow 1 n
  ;;

  let num2digits num =
    let digits = (String.length (string_of_int num)) - 1 in
    let rec num2digits ret n num =
      match n with
      | n when n < 0 -> ret
      | n -> 
          let d = num / (pow 10 n) in
          num2digits (d::ret) (n-1) (num - d * (pow 10 n))
    in
    num2digits [] digits num
  ;;

  let numbers limit =
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
      | [], n::ns -> sieve [n] (sift n ns)
      | r::rs, ns when (r*r) > (List.hd (List.rev ns)) ->
          List.rev_append (r::rs) ns
      | _, n::ns -> sieve (n::ret) (sift n ns)
    in
    sieve [] (List.tl (numbers limit))
  ;;

  let digits2num ds =
    let len = (List.length ds) - 1 in
    let rec create_bases ret = function
      | (-1) -> List.rev ret
      | n -> create_bases ((pow 10 n)::ret) (n-1)
    in
    let bases = create_bases [] len in
    List.fold_left (+) 0 (List.map2 (fun x y -> x*y) ds bases)
  ;;

  let truncateds num =
    let ds = num2digits num in
    let rec trunc_left ret = function
      | [] -> ret
      | ds -> trunc_left ((digits2num (List.rev ds))::ret) (List.tl ds)
    in
    let rec trunc_right ret = function
      | [] -> ret
      | ds -> 
          trunc_right 
            ((digits2num (List.rev ds))::ret) (List.rev (List.tl (List.rev ds)))
    in
    List.append (trunc_right [] ds) (trunc_left [] (List.tl ds))
  ;;

  let find_trancatables ps source =
    let has_zero x =
      let ds = num2digits x in
      List.mem 0 ds
    in
    let rec find ret = function
      | [] -> ret
      | x::xs when has_zero x -> find ret xs
      | x::xs ->
          let ts = truncateds x in
          if List.for_all (fun n -> List.mem n ps) ts
          then find (x::ret) xs
          else find ret xs
    in
    find [] source
  ;;

  let over_ten ps = List.filter (fun x -> x > 10) ps;;

  let test () =
    begin
      let limit = 1000000 in
      let ps = primes limit in
      let source = over_ten ps in
      let result = find_trancatables ps source in
      Printf.printf "%d\n" (List.length source);
      List.map (fun x -> Printf.printf "%d " x) result;
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left (+) 0 result);
      ();
    end
  ;;

end

let () = A.test ();;
