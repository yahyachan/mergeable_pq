open OUnit2
open Mergeable_pq

let heap_of_list (type t) (type ht) 
  (module M : Mutable.S with type elt = t and type t = ht) (h : ht) =
  let rec aux h tot =
    if M.is_empty h then
      List.rev tot
    else begin
      let (nh, v) = Option.get (M.pop h) in
      aux nh (v :: tot)
    end
  in
  aux h []

module type TEST = sig
  val name : string
  val test : 'a -> unit
end

module Heapsort (M : Mutable.S with type elt = int) : TEST = struct
  let name = "heapsort"
  let bound = 1000000000
  let n = 500000

  let gen () = List.init n (fun _ -> Random.int bound)
  let heapsort l =
    let h = (M.build l) in
    heap_of_list (module M) h
  
  let test _ =
    let l = gen () in
    assert_equal (heapsort l) (List.sort Int.compare l)
end

module Merge2 (M : Mutable.S with type elt = int) : TEST = struct
  let name = "merge2"
  let bound = 1000000000
  let n = 500000

  let gen () = List.init n (fun _ -> Random.int bound)
  let test _ =
    let a = gen()
    and b = gen() in
    let h = M.merge (M.build a) (M.build b) in
    assert_equal (heap_of_list (module M) h) 
                 (List.merge Int.compare
                   (List.sort Int.compare a) (List.sort Int.compare b))
end

let gen_test (module T : TEST) =
  T.name >:: T.test

module type Heap = functor (M : Utils.Ord) -> Mutable.S with type elt = M.t
let gen_suite (grp, (m : (module Heap))) =
  ("test " ^ grp) >::: [
    gen_test (module Heapsort((val m)(Int)));
    gen_test (module Merge2((val m)(Int)))
  ] |> run_test_tt_main

let h_list : (string * (module Heap)) list =
  let open Mutable in
  [
    ("pairing heap", (module Pairing))
  ]

let _ =
  Random.init 114514;
  List.map gen_suite h_list