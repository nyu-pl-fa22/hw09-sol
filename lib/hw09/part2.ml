module type MultisetType = sig
  (** represents base set U *)
  type u

  (** represents MSet(U) *)
  type t 

  val count: t -> u -> int
  val empty: t
  val add: t -> u -> t  
  val remove: t -> u -> t
  val union: t -> t -> t
  val inter: t -> t -> t
  val diff: t -> t -> t
  val union: t -> t -> t
  val sum: t -> t -> t
  val equals: t -> t -> bool

  (** Convert a list of base elements to a multiset *)
  val of_list: u list -> t

  (** Return a string representation of the multiset *)
  val to_string: (u -> string) -> t -> string
      
  (** Return the maximal element contained in the multiset if it exists. *)
  val max_opt: t -> u option
end

module Make(O: Map.OrderedType) : MultisetType with type u = O.t = struct
  (* We use the type int Map.Make(O).t to represnt multisets over O.t.
   * The implementation maintains the invariant that base elements
   * with 0 multiplicity are not explicitly represented in maps. This
   * way, we can use Map.S.equal to implement the equality test on
   * multisets *)
  type u = O.t

  module M = Map.Make(O)

  type t = int M.t

  let count m x = match M.find_opt x m with
  | None -> 0
  | Some c -> c

  let empty = M.empty
      
  let add m x =
    M.update x (function None -> Some 1 | Some c -> Some (c + 1)) m

  let remove m x =
    M.update x (function Some c when c > 1 -> Some (c - 1) | _ -> None) m

  let union m1 m2 =
    M.merge (fun x a1 a2 -> match a1, a2 with
    | None, Some c | Some c, None -> Some c
    | Some c1, Some c2 -> Some (max c1 c2)
    | _ -> None) m1 m2
      
  let inter m1 m2 =
    M.merge (fun x a1 a2 -> match a1, a2 with
    | Some c1, Some c2 -> Some (min c1 c2)
    | _ -> None) m1 m2

  let diff m1 m2 =
    M.merge (fun x a1 a2 -> match a1, a2 with
    | Some c, None -> Some c
    | Some c1, Some c2 when c1 > c2 -> Some (c1 - c2)
    | _ -> None) m1 m2

  let sum m1 m2 =
    M.merge (fun x a1 a2 -> match a1, a2 with
    | Some c, None | None, Some c -> Some c
    | Some c1, Some c2 -> Some (c1 + c2)
    | _ -> None) m1 m2

  let equals m1 m2 = M.equal (=) m1 m2

  let of_list xs = List.fold_left add empty xs
      
  let to_string string_of_u m =
    M.bindings m |>
    List.map (fun (x, c) -> Printf.sprintf "%s -> %d" (string_of_u x) c) |>
    String.concat ", " |>
    fun s -> "{" ^ s ^ "}"

  let max_opt m = match M.max_binding_opt m with
  | Some (x, _) -> Some x
  | None -> None

end
