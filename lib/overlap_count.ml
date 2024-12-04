open Z3

type expression_count_pair = {
  expr: Expr.expr;
  count: int;
}

type seen_exprs_and_constraints = {
  seen_expressions: expression_count_pair list;
  constraints: Z3.Expr.expr;
}

(* Either increments the counter for a matching expression or concatonates it to the end of the list *)
let replace_or_append ctx (seen_expressions: expression_count_pair list) constraints (key_expr: Expr.expr): seen_exprs_and_constraints = (*return constraints, then new seen_expressions*)
  let found_match = ref false in
  let final_constraints = ref constraints in
  let mapped_list = List.map (fun expr_count_pair -> 
    if !found_match then expr_count_pair else
    let new_constraints = Boolean.mk_and ctx [constraints; Boolean.mk_eq ctx key_expr expr_count_pair.expr] in
    let solver = Solver.mk_solver ctx None in
    Solver.add solver [new_constraints];
    match Solver.check solver [] with
    | Solver.SATISFIABLE ->
        found_match := true;
        final_constraints := new_constraints;
        {expr = expr_count_pair.expr; count = expr_count_pair.count + 1}
    | _ -> 
        expr_count_pair
    ) seen_expressions in
  if !found_match then {seen_expressions = mapped_list; constraints = !final_constraints}
  else {seen_expressions = seen_expressions @ [{expr = key_expr; count = 1}]; constraints = constraints}


(* Symbolically evaluate the maximum number of overlapping memory accesses *)
let count_symbolic_accesses (ctx: context) (expression_list: Expr.expr list) : int * Expr.expr * (expression_count_pair list) = 
  let result = List.fold_left (fun accum expr -> 
    let seen_exprs = accum.seen_expressions in
    let constraints = accum.constraints in
    replace_or_append ctx seen_exprs constraints expr
  ) {seen_expressions = []; constraints = Boolean.mk_true ctx} expression_list in
  (List.length result.seen_expressions, result.constraints, result.seen_expressions);
;;
