module type S = sig
  type elt
  (** Type of elements in heap. *)

  type t
  (** Type of heap *)

  type iter
  (** Iterator type, used to modify an element in heap. *)

  val empty : t
  (** [empty] returns an empty heap. *)

  val is_empty : t -> bool
  (** [is_empty h] returns whether [h] is empty. *)

  val build : elt list -> t
  (** [build l] builds a new heap from the element list [l]. *)

  val push : t -> elt -> t * iter
  (** [push h e] pushes the element [e] into heap [h], and
      returns the resulting heap as well as the iterator of 
      [e] in [h]. *)

  val top : t -> elt option
  (** [top h] returns the least element [e] in form
      [Some e] if [h] is not empty, and returns [None]
      otherwise. *)
  
  val pop : t -> (t * elt) option
  (** [pop h] pops the least element [e] in heap [h] and
      returns [Some (h', e)] where [h'] is the resulting
      heap if [h] is not empty. Otherwise, it
      returns [None]. *)
  
  val merge : t -> t -> t
  (** [merge h1 h2] merges the two heaps [h1] and [h2] and
      returns the resulting heap. *)

  val decrease_key : t -> iter -> elt -> t
  (** [decrease_key h r v] changes the iterator [r]'s 
      value to a smaller value [v] in heap [h] and returns 
      the resulting heap. *)
end
(** Signature of mutable mergeable heaps. *)




(* Implements. *)
module Pairing (M : Utils.Ord) : S with type elt = M.t = struct
  type elt = M.t
  type t =
    | Nil
    | Tree of elt ref * t ref * t list ref
  type iter = t

  let empty = Nil
  let is_empty x = (x = Nil)
  
  let merge a b =
    match a, b with
    | Nil, b -> b
    | a, Nil -> a
    | Tree (x, fa, ca), Tree (y, fb, cb) ->
        if compare !x !y < 0 then begin
            fb := a; ca := b :: !ca; a
        end else begin
            fa := b; cb := a :: !cb; b
        end

  let push h v =
    let np = Tree (ref v, ref Nil, ref []) in
    let nh = merge np h in
    (nh, np)
  let rec build_aux res_h = function
    | [] -> res_h
    | x :: xs ->
        let (nh, _) = push res_h x in
        build_aux nh xs
  let build = build_aux Nil

  let top = function
    | Nil -> None
    | Tree (xr, _, _) -> Some (!xr)

  let rec merge_list = function
    | [] -> Nil
    | [Tree (_, fa, _) as t] -> fa := Nil; t
    | (Tree (_, fa, _) as a) :: (Tree (_, fb, _) as b) :: xs ->
        fa := Nil; fb := Nil; merge (merge_list xs) (merge a b)
    | _ -> assert false
  let pop = function
    | Nil -> None
    | Tree (xr, _, cl) ->
        Some (merge_list (!cl), !xr)
  
  let decrease_key h r v =
    match r with
    | Nil -> assert false
    | Tree (x, f, _) ->
      x := v; f := Nil; merge h r
end