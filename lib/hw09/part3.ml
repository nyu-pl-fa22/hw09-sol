module type OrderedMultisetType = sig
  include Part2.MultisetType

  val compare: t -> t -> int
end

module Make(O: Map.OrderedType) : OrderedMultisetType with type u = O.t = struct
  include Part2.Make(O)

  (* The following implementation uses the fact that for total
     orderings on the base set, the Dershowitz-Manna ordering is
     equivalent to: m1 < m2 iff
     - m1 != m2 and
     - exists y in U, m1 y < m2 y and for all x in U, m2 x < m1 x implies x < y
   *)
  let compare m1 m2 =
    let m1_diff_m2 = diff m1 m2 in
    let m2_diff_m1 = diff m2 m1 in
    match max_opt m1_diff_m2, max_opt m2_diff_m1 with
    | Some x, Some y -> O.compare x y
    | None, Some _ -> -1
    | Some _, None -> 1
    | None, None -> 0
end

