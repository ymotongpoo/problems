module A = struct
  
  open Big_int

  let ( ** ) = power_int_positive_int;;
  
  let pows low high =
    let rec pows ret a b =
      match a, b with
      | a, _ when a=high+1 -> ret
      | _, b when b=high+1 -> pows ret (a+1) low
      | a, b -> 
          let v = a**b in
          if List.exists (fun x -> eq_big_int x v) ret
          then pows ret a (b+1)
          else pows ((a**b)::ret) a (b+1)
    in
    pows [] low low
  ;;

  let test () =
    Printf.printf "%d\n" (List.length (pows 2 100));;

end

let () = A.test ();;
