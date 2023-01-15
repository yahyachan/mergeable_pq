module type S = sig
  type elt
  (** Type of elements in heap. *)

  type t
  (** Type of heap *)

  type iter
  (** Iterator type, used to modify an element in heap. *)

  val emtpy : t
  (** [empty] returns an empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] returns whether [h] is empty. *)

  val build : elt list -> t
  (** [build l] builds a new heap from the element list [l]. *)

  val push : t -> elt -> iter
  (** [push h e] pushes the element [e] into heap [h], and
      returns the iterator of [e] in [h]. *)

  val top : t -> elt option
  (** [top h] returns the least element [e] in form
      [Some e] if [h] is not empty, and returns [None]
      otherwise. *)
  
  val pop : t -> elt option
  (** [pop h] pops the least element [e] in heap [h] and
      returns [Some e] if [h] is not empty. Otherwise, it
      returns [None]. *)
  
  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2] and
      returns the resulting heap. *)

  val modify : t -> iter -> elt -> t
  (** [modify h r v] changes the iterator [r]'s value
      to [v] in heap [h] and returns the resulting 
      heap. *)
end
(** Signature of mutable mergeable heaps. *)




(* Implements. *)