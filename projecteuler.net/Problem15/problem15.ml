module A = struct

  open Big_int

  let (-) = minus_big_int;;
  let (+) = add_big_int;;
  let ( * ) = mult_big_int;;
  let (/) = div_big_int;;
  let (mod) = mod_big_int;;
  
  let fact' m n =
    let rec fact' ret x y = 
      if eq_big_int y zero_big_int then ret
      else fact' (x * ret) (pred_big_int x) (pred_big_int y)
    in
    fact' unit_big_int m n
  ;;
      
  let combination l r = (fact' l r) / (fact' r r);;

  let test () = 
    let forty = big_int_of_int 40 and
        twenty = big_int_of_int 20 in
    begin
      Printf.printf "%s\n" (string_of_big_int (fact' forty twenty));
      Printf.printf "%s\n" (string_of_big_int (fact' twenty twenty));
      Printf.printf "%s\n" (string_of_big_int (combination forty twenty));
    end
  ;;

end

let () = A.test ();;
