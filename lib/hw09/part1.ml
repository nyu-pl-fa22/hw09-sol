module type MultisetType = sig
  (** represents MSet(U) for arbitrary base sets U *)
  type 'u t 

  val count: 'u t -> 'u -> int
  val empty: 'u t
  val add: 'u t -> 'u -> 'u t
  val remove: 'u t -> 'u -> 'u t
  val union: 'u t -> 'u t -> 'u t
  val inter: 'u t -> 'u t -> 'u t
  val diff: 'u t -> 'u t -> 'u t
  val union: 'u t -> 'u t -> 'u t
  val sum: 'u t -> 'u t -> 'u t
  val equals: 'u t -> 'u t -> bool

  (** Convert a list of base elements to a multiset *)
  val of_list: 'u list -> 'u t

  (** Return a string representation of the multiset *)
  val to_string: ('u -> string) -> 'u t -> string
end

module Multiset : MultisetType = struct
  (* We implement 'u t as list of pairs of base elements and their
     multiplicity.  The invariant maintained by the implementation is:

     - the pairs are sorted in increasing order of their first
       component according to the generic compare function on 'u

     - the lists only track non-zero multiplicities

     These invariants guarantee that each multiset is represented by a
     unique list and we can simply use equality on lists to test
     equality of multisets.
   *)
  type 'u t = ('u * int) list

  let upsert op m x =
    let rec upsert res = function
      | (y, c) :: m1 as m ->
          let cmp = compare x y in
          if cmp = 0
          then List.rev res @ op c @ m1
          else if cmp < 0
          then List.rev res @ op 0 @ m
          else upsert ((y, c) :: res) m1
      | [] -> List.rev (op 0 @ res)
    in
    upsert [] m
      
  let merge op m1 m2 =
    let rec merge res m1 m2 = match m1, m2 with
    | (x1, c1) :: m11, (x2, c2) :: m21 ->
        let cmp = compare x1 x2 in
        if cmp = 0
        then merge (op x1 c1 c2 @ res) m11 m21
        else if cmp < 0
        then merge (op x1 c1 0 @ res) m11 m2 
        else merge (op x2 0 c2 @ res) m1 m21
    | [], (x2, c2) :: m21 ->
        merge (op x2 0 c2 @ res) [] m21
    | (x1, c1) :: m11, [] ->
        merge (op x1 c1 0 @ res) m11 []
    | [], [] -> List.rev res
    in
    merge [] m1 m2 

  let rec count m x = match m with
  | (y, c) :: m1 ->
      let cmp = compare x y in
      if cmp = 0 then c else
      if cmp < 0 then 0 else
      count m1 x
  | [] -> 0

  let empty = []

  let add m x =
    upsert (fun c -> [(x, c + 1)]) m x

  let remove m x =
    upsert (fun c -> if c > 1 then [(x, c - 1)] else []) m x

  let union m1 m2 =
    merge (fun x c1 c2 ->
      if c1 + c2 > 0
      then [(x, max c1 c2)]
      else []) m1 m2
      
  let inter m1 m2 =
    merge (fun x c1 c2 ->
      if c1 > 0 && c2 > 0
      then [(x, min c1 c2)]
      else []) m1 m2

  let diff m1 m2 =
    merge (fun x c1 c2 ->
      if c1 > c2
      then [(x, c1 - c2)]
      else []) m1 m2

  let sum m1 m2 =
    merge (fun x c1 c2 ->
      if c1 + c2 > 0
      then [(x, c1 + c2)]
      else []) m1 m2

  let equals m1 m2 = m1 = m2

  let of_list xs = List.fold_left add empty xs    

  let to_string string_of_u m =
    m |>
    List.map (fun (x, c) -> Printf.sprintf "%s -> %d" (string_of_u x) c) |>
    String.concat ", " |>
    fun s -> "{" ^ s ^ "}"
end

