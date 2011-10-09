module A = struct

  let max_edge = 1001;

  let e_in_edge = function
    | 0 -> 1
    | n -> 8*(n-1)
  ;;
  
  let mergin_from_prev = function
    | 1 -> 0
    | n -> 2*(n-1)
  ;;

  let largest_in_edge n =
    let rec largest_in_edge ret = function
      | 1 -> 1 + ret
      | n -> largest_in_edge ((e_in_edge n) + ret) (n-1)
    in
    largest_in_edge 0 n
  ;;

  let sum_in_edge n =
    match n with
    | 1 -> 1
    | n -> 
        2*((largest_in_edge n)+(largest_in_edge (n-1))+(mergin_from_prev n))
  ;;

  let depth = (max_edge + 1) / 2;;

  let sum_total n =
    let rec sums ret = function
      | 0 -> ret
      | n -> sums ((sum_in_edge n) + ret) (n-1)
    in
    sums 0 n
  ;;

  let test () =
    begin
      Printf.printf "%d\n" (sum_in_edge 3);
      Printf.printf "%d\n" (largest_in_edge 4);
      Printf.printf "%d\n" (sum_total depth);
    end
  ;;

end

let () = A.test ();;
