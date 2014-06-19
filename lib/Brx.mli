(* modules *)
module IntSet : Set.S with type elt = int
type ('a,'b) alternative = Left of 'a | Right of 'b

(* regular expressions *)
type t

(* constants *)
val epsilon : t 
val empty : t
val ascii_set : t

(* constructors *)
val mk_cset : (int * int) list -> t
val mk_neg_cset : (int * int) list -> t
val mk_string : string -> t
val mk_alt : t -> t -> t
val mk_seq : t -> t -> t
val mk_star : t -> t
val mk_iter : t -> int -> int -> t
val mk_diff : t -> t -> t
val mk_complement: t -> t
val mk_inter : t -> t -> t
val mk_reverse : t -> t

(* pretty printing *)
(* ranks *)
type r = 
  | Urnk (* union *)
  | Drnk (* diff *)
  | Irnk (* inter *)
  | Crnk (* concat *)
  | Srnk (* star *)
  | Arnk (* atomic *)
val rank : t -> r
val lpar : r -> r -> bool
val rpar : r -> r -> bool

val format_t : t -> unit
val string_of_char_code : int -> string
val string_of_t : t -> string

(* operations *)
val is_empty : t -> bool
val is_final : t -> bool
val is_singleton : t -> bool
val disjoint_cex : t -> t -> string option
val disjoint : t -> t -> bool
val equiv : t -> t -> bool
val representative : t -> string option

(* string matching *)
val match_sub_string : t -> string -> int -> int -> bool
val match_string : t -> string -> bool
val match_string_positions : t -> string -> IntSet.t
val match_string_reverse_positions : t -> string -> IntSet.t

(* ambiguity *)
val derivative : t -> string -> t
val mk_reverse : t -> t
val splittable_cex : t -> t -> ((string * string * string * string),t) alternative
val splittable : t -> t -> bool
val iterable_cex : t -> ((string * string * string * string),t) alternative
val iterable : t -> bool

(* splitting *)
val split_positions : t -> t -> string -> IntSet.t
val bad_prefix_position : t -> string -> int
val split_bad_prefix : t -> string -> string * string
val seq_split : t -> t -> string -> (string * string) option
val star_split : t -> string -> string list

