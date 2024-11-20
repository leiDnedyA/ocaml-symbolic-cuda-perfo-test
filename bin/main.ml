(* module IntSet = Set.Make(struct *)
(*   type t = int *)
(*   let compare = compare *)
(* end) *)
(**)
(* let count_accesses (enabled_array: bool array) (warp_count: int) (word_size: int): int =  *)
(*   let accesses = ref IntSet.empty in *)
(*   for i = 1 to warp_count do *)
(*     if enabled_array.(i - 1) == true then  *)
(*       accesses := IntSet.add ((i - 1) / word_size) !accesses; *)
(*   done; *)
(*   IntSet.cardinal !accesses *)
(**)

open Z3

(* Example usage with Z3 *)
let () =

  (* let enabled = [|true; false; true; true; true|] *)
  (* let warp_count = Array.length enabled *)
  (* let word = 4 *)
  (* *)
  (* Printf.printf "%d\n" (count_accesses enabled warp_count word)  *)


  (* Create Z3 context *)
  let ctx = mk_context [] in

  let bin f x y = f ctx [x;y] in 
  let listless_bin f x y = f ctx x y in

  (* Define some partially-defined helper functions to simplify z3 syntax *)
  let add = bin Arithmetic.mk_add in
  let div = listless_bin Arithmetic.mk_div in

  let a = Arithmetic.Integer.mk_const ctx (Symbol.mk_string ctx "a")  in

  (* Create expressions for `expression_list = [a, a + 1, a + 2, a + 3]` pointwise divided by 4 *)
  let numeral_4 = Arithmetic.Integer.mk_numeral_i ctx 4 in
  let expression_list = List.map (fun x -> div x numeral_4) [
    a;
    add a (Arithmetic.Integer.mk_numeral_i ctx 1);
    add a (Arithmetic.Integer.mk_numeral_i ctx 2);
    add a (Arithmetic.Integer.mk_numeral_i ctx 3);
  ] in

  (* Call the count_symbolic_accesses function *)
  let (unique_count, final_constraints) = Overlap_count.count_symbolic_accesses ctx expression_list in
  (* Print result *)
  Printf.printf "Unique overlapping expressions: %d\n" unique_count;
  Printf.printf "Final constraints: %s\n" (Expr.to_string final_constraints);
