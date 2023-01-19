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
(** Signature of mutable mergeable min-heaps. *)




(* Implements. *)
module Pairing (M : Utils.Ord) : S with type elt = M.t = struct
  type elt = M.t
  type tree = { mutable value : elt;
             mutable fa : t;
             mutable lc : t;
             mutable lb : t;
             mutable rb : t }
  and t = tree option
  type iter = tree

  let empty = None
  let is_empty x = (x = None)
  
  let merge a b =
    match a, b with
    | None, b -> b
    | a, None -> a
    | Some o1, Some o2 ->
        if compare o1.value o2.value < 0 then begin
            o2.fa <- a;
            Option.iter (fun x -> x.lb <- b) o1.lc;
            o2.rb <- o1.lc;
            o1.lc <- b;
            a
        end else begin
            o1.fa <- b;
            Option.iter (fun x -> x.lb <- a) o2.lc;
            o1.rb <- o2.lc;
            o2.lc <- a;
            b
        end

  let push h v =
    let it = { value = v; fa = None; lc = None; rb = None; lb = None } in
    let np = Some it in
    let nh = merge np h in
    (nh, it)
  let rec build_aux res_h = function
    | [] -> res_h
    | x :: xs ->
        let (nh, _) = push res_h x in
        build_aux nh xs
  let build = build_aux empty

  let top = Option.map (fun o -> o.value)

  let reset x =
    x.fa <- None;
    x.lb <- None;
    x.rb <- None
  let rec merge_list = function
    | None -> None
    | Some ({rb; _} as o1) as x ->
      match rb with
      | None -> x
      | Some o2 as y ->
        let next = o2.rb in
        reset o1;
        reset o2;
        merge (merge_list next) (merge x y)
  let pop = function
    | None -> None
    | Some {value; lc; _} ->
      Some (merge_list lc, value)
  
  let decrease_key h o v =
    o.value <- v;
    match o.fa with
    | None -> Some o
    | Some oo ->
      if is_empty o.lb then
        (oo.lc <- o.rb;
        Option.iter (fun e -> e.lb <- None) o.rb)
      else
        (Option.iter (fun x -> x.rb <- o.rb) o.lb;
         Option.iter (fun x -> x.lb <- o.lb) o.rb);
      reset o;
      merge h (Some o)
end

module Binomial (M : Utils.Ord) : S with type elt = M.t = struct
  type elt = M.t
  type tree = 
  { mutable rank: int;
    mutable value: elt;
    mutable fa : tree option;
    mutable children : tree list;
    mutable pointer : tree ref }
  type t = tree list * tree option
  type iter = tree ref

  let empty = ([], None)
  let is_empty (x, _) = (x = [])

  let rec recalc = function
  | ([], res) -> res
  | (x :: xs, None) -> recalc (xs, Some x)
  | (x :: xs, Some y) when M.compare x.value y.value < 0 ->
    recalc (xs, Some x)
  | (_ :: xs, th) -> recalc (xs, th)

  let merge_tree tree1 tree2 =
    if M.compare tree1.value tree2.value < 0 then begin
      tree1.rank <- tree1.rank + 1;
      tree2.fa <- Some tree1;
      tree1.children <- tree2 :: tree1.children;
      tree1
    end else begin
      tree2.rank <- tree2.rank + 1;
      tree1.fa <- Some tree2;
      tree2.children <- tree1 :: tree2.children;
      tree2
    end
  let rec merge_aux h1 h2 res =
    match res with
    | t1 :: t2 :: xs when t1.rank = t2.rank ->
      merge_aux h1 h2 (merge_tree t1 t2 :: xs)
    | _ ->
      match h1, h2 with
      | [], [] -> res
      | x1 :: xs1, [] -> merge_aux xs1 [] (x1 :: res)
      | [], x2 :: xs2 -> merge_aux [] xs2 (x2 :: res)
      | x1 :: xs1, x2 :: xs2 ->
        match Int.compare x1.rank x2.rank with
        | 0 -> merge_aux xs1 xs2 (merge_tree x1 x2 :: res)
        | v when v < 0 -> merge_aux xs1 h2 (x1 :: res)
        | _ -> merge_aux h1 xs2 (x2 :: res)
  let merge (h1, _) (h2, _) =
    let th = List.rev @@ merge_aux h1 h2 [] in
    (th, recalc (th, None))
  let singleton v =
    let rec ret = { rank = 0; value = v; fa = None; children = []; pointer = r }
    and r = ref ret in
    (ret, r)
  let push h v =
    let (th, r) = singleton v in
    (merge ([th], None) h, r)
  let push_simple h v =
    let (ret, _) = push h v in ret
  let build =
    List.fold_left push_simple empty
  let ( let+ ) o f = Option.map f o
  let top (_, p) =
    let+ {value; _} = p in value
  let pop (l, p) =
    let+ tr = p in
    let nl = List.filter (fun t -> t.rank != tr.rank) l in
    List.iter (fun t -> t.fa <- None) tr.children;
    (merge (List.rev tr.children, None) (nl, None), tr.value)
  let rec swim_up t =
    match (!t).fa with
    | None -> ()
    | Some f when M.compare f.value (!t).value <= 0 -> ()
    | Some f ->
      let fv = f.value in
      f.value <- (!t).value;
      (!t).value <- fv;
      let c = !t
      and fp = f.pointer in
      f.pointer <- c.pointer;
      c.pointer <- fp;
      f.pointer := f;
      c.pointer := c;
      swim_up t
  let decrease_key (l, p) it v =
    (!it).value <- v;
    swim_up it;
    let cpf ({value; _} as old) =
      if M.compare v value < 0 then
        !it
      else
        old
    in
    (l, Option.map cpf p)
end