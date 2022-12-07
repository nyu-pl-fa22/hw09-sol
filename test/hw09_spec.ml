open OUnit2
open Hw09
open Part1
  
type my_u = A | B | C

let string_of_my_u = function
  | A -> "a"
  | B -> "b"
  | C -> "c"
  
let add_count_tests =
  (* multiset, base element, expected result for count *)
  [[], A, 0;
   [], B, 0;
   [], C, 0;
   [C], A, 0;
   [C], B, 0;
   [C], C, 1;
   [C; A], A, 1;
   [C; A], B, 0;
   [C; A], C, 1;
   [A; C; A], A, 2;
   [A; C; A], B, 0;
   [A; C; A], C, 1;
   [A; C; A; B; B; B], A, 2;
   [A; C; A; B; B; B], B, 3;
   [A; C; A], A, 2;
   [A; C; A], C, 1]

let remove_count_tests =
  (* multiset, base elements to be removed, expected result for count *)
  [[], [A], A, 0;
   [], [A; B], B, 0;
   [], [A; A; B; C], C, 0;
   [C], [A], A, 0;
   [C], [A; A], B, 0;
   [C], [A], C, 1;
   [C], [C], C, 0;
   [C], [C; C], C, 0;
   [A; B; B; A], [A], A, 1;
   [A; B; B; A], [A], B, 2;
   [A; B; B; A], [A], C, 0;
   [A; B; B; A], [A; A], A, 0]

let add_remove_equals_tests =
  (* m1, m2, xs, expected result for "equals m1 (m2 - xs)" *)
  [[], [], [], true;
   [A], [], [], false;
   [], [A], [], false;
   [A; A], [A], [], false;
   [A], [A; A], [], false;
   [A; B], [A; C], [], false;
   [A; B; A], [A; B], [], false;
   [A], [A], [], true;
   [A; B], [B; A], [], true;
   [A; B; A], [A; A; B], [], true;
   [C; A; B; C], [C; C; B; A], [], true;
   [A; B; C], [C; A], [], false;
   [A; C], [A; C; C], [], false;
   [], [A], [A], true;
   [], [], [B], true;
   [A], [A; A; A], [A; A; B], true;
   [A; B], [A; A], [A; B], false;
   [A; A], [A; A], [A], false;
 ]
    
let union_tests =
  (* m1, m2, expected_result of union m1 m2 *)
  [[], [], [];
   [], [A], [A];
   [A], [], [A];
   [A], [A], [A];
   [A; B], [A], [A; B];
   [A; A; B], [A; B; B], [A; A; B; B];
   [A; B; C], [A], [A; B; C];
   [A; B; C], [], [A; B; C];
   [A; B; C], [A; A; C], [A; A; B; C];
   [A; B; B], [A; A; B; C], [A; A; B; B; C]]

let inter_tests =
  (* m1, m2, expected_result of inter m1 m2*)
  [[], [], [];
   [], [A], [];
   [A], [], [];
   [A], [A], [A];
   [A; B], [A], [A];
   [A; A; B], [A; B; B], [A; B];
   [A; B; C], [A], [A];
   [A; B; C], [], [];
   [A; B; C], [A; A; C], [A; C];
   [A; B; B], [A; A; B; C], [A; B];
   [A; B; B], [A; A; B; B; C], [A; B; B]]

let diff_tests =
  (* m1, m2, expected_result of diff m1 m2 *)
  [[], [], [];
   [], [A], [];
   [A], [], [A];
   [A], [A], [];
   [A; B], [A], [B];
   [A; A; B], [A; B; B], [A];
   [A; B; C], [A], [B; C];
   [A; B; C], [], [A; B; C];
   [A; B; C], [A; A; C], [B];
   [A; B; B], [A; A; B; C], [B];
   [A; B; B], [A; A; B; B; C], [];
   [A; A; B; B], [A; C], [A; B; B]]
    
let sum_tests =
  (* m1, m2, expected_result of sum m1 m2 *)
  [[], [], [];
   [], [A], [A];
   [A], [], [A];
   [A], [A], [A; A];
   [A; B], [A], [A; A; B];
   [A; A; B], [A; B; B], [A; A; A; B; B; B];
   [A; B; C], [A], [A; A; B; C];
   [A; B; C], [], [A; B; C];
   [A; B; C], [A; A; C], [A; A; A; B; C; C];
   [A; B; B], [A; A; B; C], [A; A; A; B; B; B; C];
   [A; B; B], [A; A; B; B; C], [A; A; A; B; B; B; B; C];
   [A; A; B; B], [A; C], [A; A; A; B; B; C]]
    
let add_count_suite multiset_of_list count =
  List.map (fun (m, x, expected_result) ->
    let m = multiset_of_list m in
    let name = Printf.sprintf "add/count test" in
    name >::
    fun tc ->
      assert_equal ~printer:string_of_int expected_result (count m x))
    add_count_tests

let remove_count_suite multiset_of_list remove count =
  List.map (fun (m1, m2, x, expected_result) ->
    let m1 = multiset_of_list m1 in
    let m = List.fold_left remove m1 m2 in
    let name = Printf.sprintf "remove/count test" in
    name >::
    fun tc ->
      assert_equal ~printer:string_of_int expected_result (count m x))
    remove_count_tests    

let add_remove_equals_suite multiset_of_list remove equals =
  List.map (fun (m1, m2, xs, expected_result) ->
    let m1 = multiset_of_list m1 in
    let m2 = multiset_of_list m2 in
    let m2 = List.fold_left remove m2 xs in
    let name = Printf.sprintf "remove/add/equals test" in
    name >::
    fun tc ->
      assert_equal ~printer:string_of_bool expected_result (equals m1 m2))
    add_remove_equals_tests

let bin_suite op op_name tests multiset_of_list equals =
  List.map (fun (m1, m2, expected_result) ->
    let m1 = multiset_of_list m1 in
    let m2 = multiset_of_list m2 in
    let expected_result = multiset_of_list expected_result in
    let name = Printf.sprintf "%s test" op_name in
    name >::
    fun tc ->
      assert_equal ~cmp:equals expected_result (op m1 m2))
    tests    

(** Part 1 tests *)

(*let part1_string_of_multiset m = Multiset.to_string string_of_my_u m*)
let part1_multiset_of_list = List.fold_left Multiset.add Multiset.empty
let part1_equals m1 m2 = List.for_all (fun x -> Multiset.count m1 x = Multiset.count m2 x) [A; B; C]
    
    
let part1_suite =
  "Part 1 suite" >:::
  add_count_suite part1_multiset_of_list Multiset.count @
  remove_count_suite part1_multiset_of_list Multiset.remove Multiset.count @
  add_remove_equals_suite part1_multiset_of_list Multiset.remove Multiset.equals @
  bin_suite Multiset.union "union" union_tests part1_multiset_of_list part1_equals @
  bin_suite Multiset.inter "inter" inter_tests part1_multiset_of_list part1_equals @
  bin_suite Multiset.diff "diff" diff_tests part1_multiset_of_list part1_equals @
  bin_suite Multiset.sum "sum" sum_tests part1_multiset_of_list part1_equals

let _ = run_test_tt_main part1_suite

(** Part 2 tests *)

module MyMultiset = Part2.Make(struct
  type t = my_u
  let compare x y = match x, y with
  | A, (B | C)
  | B, C -> -1
  | C, (A | B)
  | B, A -> 1
  | _ -> assert (x = y); 0
end)

(*let part2_string_of_multiset m = MyMultiset.to_string string_of_my_u m*)
let part2_multiset_of_list = List.fold_left MyMultiset.add MyMultiset.empty
let part2_equals m1 m2 = List.for_all (fun x -> MyMultiset.count m1 x = MyMultiset.count m2 x) [A; B; C]

let part2_suite =
  "Part 2 suite" >:::
  add_count_suite part2_multiset_of_list MyMultiset.count @
  remove_count_suite part2_multiset_of_list MyMultiset.remove MyMultiset.count @
  add_remove_equals_suite part2_multiset_of_list MyMultiset.remove MyMultiset.equals @
  bin_suite MyMultiset.union "union" union_tests part2_multiset_of_list part2_equals @
  bin_suite MyMultiset.inter "inter" inter_tests part2_multiset_of_list part2_equals @
  bin_suite MyMultiset.diff "diff" diff_tests part2_multiset_of_list part2_equals @
  bin_suite MyMultiset.sum "sum" sum_tests part2_multiset_of_list part2_equals
    
let _ = run_test_tt_main part2_suite


(** Part 3 tests ... *)

module MyOMultiset = Part3.Make(struct
  type t = int
  let compare = compare
end)

let part3_compare_tests =
  (* m1, m2, test for result of compare, description *)
  [[1; 1; 1; 2; 2], [1; 1; 3], (fun cmp -> cmp < 0), "Expected negative integer.";
   [1; 1; 3], [1; 1; 1; 2; 2], (fun cmp -> cmp > 0), "Expected positive integer.";
   [1; 1; 2; 2], [1; 1; 1; 2; 2], (fun cmp -> cmp < 0), "Expected negative integer.";
   [1; 1; 1; 2; 2], [1; 1; 2; 2], (fun cmp -> cmp > 0), "Expected positive integer.";
   [], [], (fun cmp -> cmp = 0), "Expected 0.";
   [1], [1], (fun cmp -> cmp = 0), "Expected 0.";
   [1; 1], [1; 1], (fun cmp -> cmp = 0), "Expected 0.";
   [1; 1; 3], [1; 3; 1], (fun cmp -> cmp = 0), "Expected 0.";
   [1; 1; 3; 2; 2], [2; 1; 3; 1; 2], (fun cmp -> cmp = 0), "Expected 0.";
   [], [1], (fun cmp -> cmp < 0), "Expected negative integer.";
   [1], [], (fun cmp -> cmp > 0), "Expected positive integer.";
   [], [1; 1], (fun cmp -> cmp < 0), "Expected negative integer.";
   [1; 1], [], (fun cmp -> cmp > 0), "Expected positive integer.";
   [], [1; 2; 1], (fun cmp -> cmp < 0), "Expected negative integer.";
   [1; 2; 1], [], (fun cmp -> cmp > 0), "Expected positive integer.";
 ]

let part3_count_suite =
  List.map (fun (m1, m2, p, msg) ->
    let m1 = MyOMultiset.of_list m1 in
    let m2 = MyOMultiset.of_list m2 in
    let name =
      Printf.sprintf "compare test"
    in
    name >::
    fun tc ->
      assert_bool msg (p @@ MyOMultiset.compare m1 m2))
    part3_compare_tests


let part3_suite =
  "Part 2 suite" >:::
  part3_count_suite

    
let _ = run_test_tt_main part3_suite
