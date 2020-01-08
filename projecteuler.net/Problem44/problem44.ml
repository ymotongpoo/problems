module A = struct

  let pantagonal n = n*(3*n-1)/2 ;;

  let pantagonals limit =
    let rec ps ret = function
      | 0 -> ret
      | n -> ps ((pantagonal n)::ret) (n-1)
    in
    ps [] limit
  ;;

  let pairs l =
    let rec comb l c =
      if (List.length c) = 2 then [c] else
        match l with
        | [] -> []
        | (h::t) -> List.rev_append (comb t (h::c)) (comb t c)
    in
    comb l []
  ;;

  let solver pantas ps =
    let check_pair pair = 
      let sum = List.fold_left (+) 0 pair in
      let diff = abs (List.fold_left (-) (List.hd pair) (List.tl pair)) in
      List.mem sum pantas && List.mem diff pantas
    in
    let rec roller min = function
      | [] -> min
      | p::ps when check_pair p ->
          let diff = abs (List.fold_left (-) (List.hd p) (List.tl p)) in
          if diff < min
          then roller diff ps
          else roller min ps
      | p::ps ->
          roller min ps
    in
    roller 10000000 ps
  ;;

  let test () =
    begin
      let pantas = pantagonals 10000 in
      let ps = pairs pantas in
      Printf.printf "%d\n" (solver pantas ps);
    end
  ;;

end

let _ = A.test ();;
