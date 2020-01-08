(****************************************************************
 * Binary Tree
 *
 * created by Yoshifumi YAMAGUCHI, 2010
 ***************************************************************)

type 'a tree
(** [Bintree.('a tree) *)

val size : 'a tree -> int
(** [Bintree.size bt] returns the number of elements in the 
    binary tree [bt]. *)

val depth : 'a tree -> int
(** [Bintree.depth bt] returns the max depth of binary tree [bt]. *)

val mem : 'a -> 'a tree -> bool
(** [Bintree.mem bt n] confirm [n] is in the binary tree [bt] or not. *)

val count : 'a -> 'a tree  -> int
(** [Bintree.count bt e] counts the number of node which has element [e] 
    in the binary tree [bt]. *)

val insert : 'a -> 'a tree -> 'a tree
(** [Bintree.insert bt e] inserts the element [e] into the binary tree [bt]. *)

val update : 'a -> 'a -> 'a tree -> 'a tree
(** [Bintree.update bt s t] updates all elements [s] in the binary tree [bt]
    to assigned element [t]. *)


