let parse tokens =
  let rec parse_expr tokens = parse_implication tokens

  and parse_implication tokens =
    let lhs, rest = parse_or tokens in
    match rest with
    | "->" :: rest' ->
        let rhs, rest'' = parse_implication rest' in
        (Formula.Imp (lhs, rhs), rest'')
    | _ -> (lhs, rest)

  and parse_or tokens =
    let lhs, rest = parse_and tokens in
    match rest with
    | "|" :: rest' ->
        let rhs, rest'' = parse_or rest' in
        (Formula.Or (lhs, rhs), rest'')
    | _ -> (lhs, rest)

  and parse_and tokens =
    let lhs, rest = parse_not tokens in
    match rest with
    | "&" :: rest' ->
        let rhs, rest'' = parse_and rest' in
        (Formula.And (lhs, rhs), rest'')
    | _ -> (lhs, rest)

  and parse_not tokens =
    match tokens with
    | "~" :: rest ->
        let e, rest' = parse_not rest in
        (Formula.Not e, rest')
    | _ -> parse_atom tokens

  and parse_atom tokens =
    let is_var_token tok =
      let first = tok.[0] in
      (Char.code first >= Char.code 'A' && Char.code first <= Char.code 'Z') ||
      (Char.code first >= Char.code 'a' && Char.code first <= Char.code 'z') ||
      (Char.code first >= Char.code '0' && Char.code first <= Char.code '9') ||
      first = '_'
    in
    match tokens with
    | "(" :: rest ->
      let e, rest' = parse_expr rest in
      (match rest' with
       | ")" :: r -> (e, r)
       | _ -> failwith "Expected closing parenthesis")
    | tok :: rest when is_var_token tok ->
      (Formula.Var tok, rest)
    | tok :: _ ->
      failwith ("Unexpected token in atom: " ^ tok)
    | [] ->
      failwith "Unexpected end of input in atom"
  in
  let ast, rest = parse_expr tokens in
  if rest <> [] then failwith "Unexpected tokens after expression";
  ast

