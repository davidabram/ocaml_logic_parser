let rec vars_of_formula = function
  | Formula.Var x -> [x]
  | Formula.Not f -> vars_of_formula f
  | Formula.And (f1, f2) | Formula.Or (f1, f2) | Formula.Imp (f1, f2) ->
      List.sort_uniq compare (vars_of_formula f1 @ vars_of_formula f2)

let rec all_assignments vars =
  match vars with
  | [] -> [ [] ]
  | v :: vs ->
      let rest = all_assignments vs in
      List.concat [
        List.map (fun r -> (v, true) :: r) rest;
        List.map (fun r -> (v, false) :: r) rest
      ]

let rec eval f env =
  let lookup x = List.assoc x env in
  match f with
  | Formula.Var x -> lookup x
  | Formula.Not f -> not (eval f env)
  | Formula.And (f1, f2) -> eval f1 env && eval f2 env
  | Formula.Or  (f1, f2) -> eval f1 env || eval f2 env
  | Formula.Imp (f1, f2) -> not (eval f1 env) || eval f2 env

let print_truth_table f =
  let vars = vars_of_formula f in
  let rows = all_assignments vars in
  let line = String.concat " | " (vars @ ["Result"]) in
  print_endline line;
  print_endline (String.make (String.length line) '-');
  List.iter (fun row ->
    let values = List.map (fun v -> if List.assoc v row then "T" else "F") vars in
    let result = if eval f row then "T" else "F" in
    print_endline (String.concat " | " (values @ [result]))
  ) rows
