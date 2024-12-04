open Z3

type t = {
  name: string;
  expressions: Expr.expr list;
  expected_count: int;
}

(* Helper function to create Z3 context and test different inputs *)
let test_inputs () =
  let ctx = mk_context [] in
  let bin f x y = f ctx [x;y] in 
  let add = bin Arithmetic.mk_add in
  let div = Arithmetic.mk_div ctx in
  let mul = bin Arithmetic.mk_mul in
  let num = Arithmetic.Integer.mk_numeral_i ctx in
  let const ch = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx ch) in
  
  (* Create base variable *)
  let a = const "a" in
  let numeral_4 = num 4 in

  let div_list_by_4 = List.map (fun x -> div x numeral_4) in
  
  (* Test Case 1: Original sequence [a, a+1, a+2, a+3] divided by 4 *)
  let test1 = {
    name = "Original";
    expressions = div_list_by_4 [
      a;
      add a (num 1);
      add a (num 2);
      add a (num 3);];
    expected_count = 1;
  } in

  (* Test Case 2: Sequence designed to maximize overlaps: [4a, 4a, 4a, 4a] divided by 4 *)
  let test2 = {
    name = "All Same";
    expressions = div_list_by_4 [
      mul a numeral_4;
      mul a numeral_4;
      mul a numeral_4;
      mul a numeral_4;
    ];
    expected_count = 1;
  } in
  
  (* Test Case 3: Another attempt with [4a, 4a+4, 4a+8, 4a+12] divided by 4 *)
  let test3 = {
    name = "Multiples of 4";
    expressions = div_list_by_4 [
      mul a numeral_4;
      add (mul a numeral_4) (num 4);
      add (mul a numeral_4) (num 8);
      add (mul a numeral_4) (num 12);
    ];
    expected_count = 4;
  } in
  

  (* Run tests *)
  List.iter (function {name; expressions; expected_count} ->
    let (count, constraints, _) = Overlap_count.count_symbolic_accesses ctx expressions in
    Printf.printf "\nTest %s:\n" name;
    Printf.printf "Unique overlapping expressions: %d\n" count;
    Printf.printf "Expected count: %d\n" expected_count;
    Printf.printf "Final constraints: %s\n" (Expr.to_string constraints)
  ) [test1; test2; test3;]
;;

(* Run the tests *)
let () = test_inputs ()
