module A = struct
  let grid =
    [[08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08];
    [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00];
    [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65];
    [52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91];
    [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80];
    [24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50];
    [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70];
    [67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21];
    [24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72];
    [21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95];
    [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92];
    [16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57];
    [86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58];
    [19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40];
    [04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66];
    [88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69];
    [04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36];
    [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16];
    [20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54];
    [01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48]];;

  let width = 4;;

  let max l =
    let rec max r = function
      | [] -> r
      | x::xs -> if x > r then max x xs else max r xs
    in
    max 0 l
  ;;


  let max_row_prod grid width =
    let splitNth n l =
      let rec splitNth fst snd n =
        match snd, n with
        | [], _ -> (List.rev fst, [])
        | _, 0 -> (List.rev fst, snd)
        | x::xs, n -> (x::fst, xs)
      in
      splitNth [] l n
    in
    let takeN n l =
      let rec takeN ret n l =
        match n, l with
        | 0, _ | _, [] -> List.rev (ret)
        | _, x::xs -> takeN (x::ret) (n-1) xs
      in
      takeN [] n l
    in
    let rec row_prods ret row col =
      match row, col with
      | r, _ when r > ((List.length grid) - 1)-> ret
      | r, c when (c+width) > (List.length (List.nth grid r)) ->
          row_prods ret (r+1) 0
      | r, c ->
          let (_, tail) = splitNth c (List.nth grid r) in
          let p = List.fold_left ( * ) 1 (takeN width tail) in
          row_prods (p::ret) r (c+1)
    in
    max (row_prods [] 0 0)
  ;;

  let max_col_prod grid width =
    let takeNcol n r c gd =
      let rec takeNcol ret n r c =
        match n, r, c with
        | 0, _, _ -> ret
        | _, r, c when (r+n) > ((List.length gd) - 1) -> []
        | _, r, c when c > ((List.length (List.nth gd r)) - 1) -> []
        | n, r, c ->
            let v = List.nth (List.nth gd r) c in
            takeNcol (v::ret) (n-1) (r+1) c
      in
      takeNcol [] n r c
    in      
    let rec col_prods ret row col =
      match row, col with
      | r, c when c > (List.length (List.nth grid r)) -> ret
      | r, c when (r+width) > ((List.length grid) - 1) -> 
          col_prods ret 0 (c+1)
      | r, c ->
          let p = List.fold_left ( * ) 1 (takeNcol width r c grid) in
          col_prods (p::ret) (r+1) c
    in
    max (col_prods [] 0 0)
  ;;

  let max_rcross_prod grid width =
    let takeNrcross n r c gd =
      let rec takeNrcross ret n r c =
        match n, r, c with
        | 0, _, _ -> ret
        | _, r, c when (r+n) > ((List.length gd) - 1) -> []
        | _, r, c when (c+n) > ((List.length (List.nth gd r)) - 1) -> []
        | n, r, c ->
            let v = List.nth (List.nth gd r) c in
            takeNrcross (v::ret) (n-1) (r+1) (c+1)
      in
      takeNrcross [] n r c
    in
    let rec rcross_prods ret row col =
      match row, col with
      | r, c when (c+width) > (List.length (List.nth grid r) - 1) -> ret
      | r, c when (r+width) > ((List.length grid) - 1) ->
          rcross_prods ret 0 (c+1)
      | r, c ->
          let p = List.fold_left ( * ) 1 (takeNrcross width r c grid) in
          rcross_prods (p::ret) (r+1) c
    in
    max (rcross_prods [] 0 0)
  ;;

  let max_lcross_prod grid width=
    let grid' = List.rev grid in
    max_rcross_prod grid' width
  ;;

  let test () = 
    begin
      Printf.printf "max in row_prod is %d\n" (max_row_prod grid width);
      Printf.printf "max in col_prod is %d\n" (max_col_prod grid width);
      Printf.printf "max in rcross_prod is %d\n" (max_rcross_prod grid width);
      Printf.printf "max in lcross_prod is %d\n" (max_lcross_prod grid width);
    end
  ;;

end

let () = A.test ();;
