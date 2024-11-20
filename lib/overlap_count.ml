open Z3

module ExprMap = Map.Make(struct
  type t = Expr.expr
  let compare = Expr.compare
end)

(* Symbolically evaluate the maximum number of overlapping memory accesses *)
let count_symbolic_accesses (ctx: context) (expression_list: Expr.expr list) : int * Expr.expr =
  (* Initial constraints and seen expressions *)
  let constraints = ref (Boolean.mk_true ctx) in
  let seen_expressions = ref ExprMap.empty in

  (* Iterate over each expression in the list *)
  List.iter (fun expr ->
    (* Track if we've found a satisfiable match for this expression *)
    let found_match = ref false in

    (* Check for overlaps with expressions in seen_expressions *)
    ExprMap.iter (fun seen_expr count ->
      (* Create a condition for overlap without contradicting current constraints *)
      let new_constraint = Boolean.mk_and ctx [!constraints; Boolean.mk_eq ctx expr seen_expr] in
      let solver = Solver.mk_solver ctx None in
      Solver.add solver [new_constraint];

      (* Check satisfiability *)
      match Solver.check solver [] with
      | Solver.SATISFIABLE ->
        (* If satisfiable, increment count and update constraints *)
        found_match := true;
        constraints := new_constraint;
        seen_expressions := ExprMap.add seen_expr (count + 1) !seen_expressions
      | _ -> ()
    ) !seen_expressions;

    (* If no match was found, add the new expression to the map *)
    if not !found_match then
      seen_expressions := ExprMap.add expr 1 !seen_expressions;
  ) expression_list;

  (* Return the size of seen_expressions (number of unique overlapping accesses) and final constraints *)
  (ExprMap.cardinal !seen_expressions, !constraints)
;;
