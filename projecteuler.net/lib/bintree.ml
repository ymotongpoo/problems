(****************************************************************
 * Binary Tree
 *
 * created by Yoshifumi YAMAGUCHI, 2010
 ***************************************************************)


type 'a tree = Lf | Bt of 'a * 'a tree * 'a tree

let rec size = function
  | Lf -> 0
  | Bt (_, l, r) -> 1 + (size l) + (size r)

let rec depth = function
  | Lf -> 0
  | Bt (_, l, r) -> 1 + max (depth l) (depth r)

let rec mem e = function
  | Lf -> false
  | Bt (x, l, r) -> x = e || mem e l || mem e r

let rec count e bt =
  match bt with
  | Lf -> 0
  | Bt (x, l, r) -> 
      if x = e then 1 + count e l + count e r
      else count e l + count e r

let rec map f = function
  | Lf -> Lf
  | Bt (x, l, r) -> Bt (f x, map f l, map f r)

let rec insert e = function
  | Lf -> Bt (e, Lf, Lf)
  | Bt (x, l, r) -> 
      match l, r with
      | Lf, _ -> Bt (x, insert e l, r)
      | _, Lf -> Bt (x, l, insert e r)
      | _, _ -> Bt (x, insert e l, insert e r)

let rec update s t = function
  | Lf -> Lf
  | Bt (x, l, r) -> 
      if x = s 
      then Bt (t, update s t l, update s t r)
      else Bt (x, update s t l, update s t r)


  

