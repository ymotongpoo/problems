module A = struct
  
  let singles = [
    (1, "one"); (2, "two"); (3, "three"); (4, "four"); (5, "five");
    (6, "six"); (7, "seven"); (8, "eight"); (9, "nine");
  ];;

  let teens = [
    (10, "ten"); (11, "eleven"); (12, "twelve"); (13, "thirteen");
    (14, "fourteen"); (15, "fifteen"); (16, "sixteen");
    (17, "seventeen"); (18, "eighteen"); (19, "nineteen");
  ];;

  let ties = [
    (20, "twenty"); (30, "thirty"); (40, "forty"); (50, "fifty");
    (60, "sixty"); (70, "seventy"); (80, "eighty"); (90, "ninety");
  ];;
  
  let find_single n =
    let rec find_single n = function
      | [] -> ""
      | (n', w') :: xs -> if n = n' then w' else find_single n xs
    in
    find_single n singles
  ;;

  let find_double n = 
    let rec find_ties n = function
      | [] -> ""
      | (n', w') :: xs ->
          if (n / n') = 1 && (n mod n') < 10 then w' ^ (find_single (n mod n'))
          else find_ties n xs
    in
    let rec find_teens n = function
      | [] -> ""
      | (n', w') :: xs -> if n = n' then w' else find_teens n xs
    in
    match n with
    | n when n >= 20 -> find_ties n ties
    | n when n >= 10 -> find_teens n teens
    | _ -> find_single n
  ;;

  let find_larges n =
    if n = 1000 then "onethousand"
    else (find_single (n / 100)) ^ "hundred"

  let mkword = function
    | n when n >= 100 -> 
        if (n mod 100) = 0 then find_larges n
        else (find_larges n)  ^ "and" ^ (find_double (n mod 100))
    | n when n < 100 -> find_double n
    | _ -> ""
  ;;

  let mkwordlist numbers =
    let rec mkwordlist ret = function
      | [] -> ret
      | x::xs -> mkwordlist ((mkword x)::ret) xs
    in
    mkwordlist [] numbers
  ;;

  let numbers n =
    let rec numbers ret = function
      | 0 -> ret
      | n -> numbers (n::ret) (n-1)
    in
    numbers [] n
  ;;
  
  let test () =
    begin
      let list = mkwordlist (numbers 1000) in
      let wordlengths = List.map (fun w -> String.length w) list in
      Printf.printf "%d\n" (List.fold_left (+) 0 wordlengths);
      ();
    end
  ;;

end

let () = A.test ();;
