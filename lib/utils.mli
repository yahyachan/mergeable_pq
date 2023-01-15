(** Useful signatures. *)

module type Ord = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] returns [0] if [x] is equal to
      [y], a negative integer if [x] is less
      than [y], and a positive integer is [x] is
      greater than [y]. *)
end
(** Totally ordered relations. *)