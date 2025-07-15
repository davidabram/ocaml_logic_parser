open Ocaml_logic_parser.Lexer
open Ocaml_logic_parser.Parser
open Ocaml_logic_parser.Formula

let () =
let test name f =
  try
    f ();
    Printf.printf "\027[32m[PASS] %s\027[0m\n%!" name
  with e ->
    Printf.printf "\027[31m[FAIL] %s: %s\027[0m\n%!"
      name
      (Printexc.to_string e);
    exit 1
  in

  let equal_list a b =
    List.equal String.equal a b
  in

  let rec equal_formula a b =
    match a, b with
    | Var x, Var y -> x = y
    | Not x, Not y -> equal_formula x y
    | And (a1, a2), And (b1, b2)
    | Or (a1, a2), Or (b1, b2)
    | Imp (a1, a2), Imp (b1, b2) -> equal_formula a1 b1 && equal_formula a2 b2
    | _ -> false
  in

  test "simple AND" (fun () ->
    assert (equal_list (tokenize "A & B") ["A"; "&"; "B"])
  );

  test "simple AND parse" (fun () ->
    let ast = parse (tokenize "A & B") in
    assert (equal_formula ast (And (Var "A", Var "B")))
  );

  test "implication" (fun () ->
    assert (equal_list (tokenize "A -> B") ["A"; "->"; "B"])
  );

  test "implication parse" (fun () ->
    let ast = parse (tokenize "A -> B") in
    assert (equal_formula ast (Imp (Var "A", Var "B")))
  );

  test "nested parens" (fun () ->
    assert (equal_list (tokenize "(X & Y) -> Z") ["("; "X"; "&"; "Y"; ")"; "->"; "Z"])
  );

  test "nested parens parse" (fun () ->
    let ast = parse (tokenize "(X & Y) -> Z") in
    assert (equal_formula ast (
      Imp (
        And (Var "X", Var "Y"),
        Var "Z"
      )
    ))
  );

  test "not operator" (fun () ->
    assert (equal_list (tokenize "~X") ["~"; "X"])
  );

  test "not operator parse" (fun () ->
    let ast = parse (tokenize "~X") in
    assert (equal_formula ast (Not (Var "X")))
  );

  test "complex expr" (fun () ->
    assert (equal_list (tokenize "~(X | Y) & Z") ["~"; "("; "X"; "|"; "Y"; ")"; "&"; "Z"])
  );

  test "complex expr parse" (fun () ->
    let ast = parse (tokenize "~(X | Y) & Z") in
    assert (equal_formula ast (
      And (
        Not (Or (Var "X", Var "Y")),
        Var "Z"
      )
    ))
  );

  test "var with underscores" (fun () ->
    assert (equal_list (tokenize "x_1 -> Y2") ["x_1"; "->"; "Y2"])
  );

  test "var with underscores parse" (fun () ->
    let ast = parse (tokenize "x_1 -> Y2") in
    assert (equal_formula ast (Imp (Var "x_1", Var "Y2")))
  );

  test "whitespace skipping" (fun () ->
    assert (equal_list (tokenize "  A \t\n &\n B ") ["A"; "&"; "B"])
  );

  test "whitespace skipping parse" (fun () ->
    let ast = parse (tokenize "  A \t\n &\n B ") in
    assert (equal_formula ast (And (Var "A", Var "B")))
  );

  test "invalid char" (fun () ->
    try
      let _ = tokenize "A $ B" in
      assert false
    with Failure msg ->
      assert (String.contains msg '$')
  );

  test "empty input" (fun () ->
    assert (equal_list (tokenize "") [])
  );

  test "empty input parse" (fun () ->
    try
      let _ = parse (tokenize "") in
      assert false
    with Failure _ -> ()
  );


  test "only whitespace" (fun () ->
    assert (equal_list (tokenize "   \t\n  ") [])
  );

  test "only whitespace parse" (fun () ->
    try
      let _ = parse (tokenize "   \t\n  ") in
      assert false
    with Failure _ -> ()
  );

  test "digit-only identifier" (fun () ->
    assert (equal_list (tokenize "12345") ["12345"])
  );

  test "digit-only identifier parse" (fun () ->
    let ast = parse (tokenize "12345") in
    assert (equal_formula ast (Var "12345"))
  );

  test "underscore-leading identifier" (fun () ->
    assert (equal_list (tokenize "_foo_bar") ["_foo_bar"])
  );

  test "underscore-leading identifier parse" (fun () ->
    let ast = parse (tokenize "_foo_bar") in
    assert (equal_formula ast (Var "_foo_bar"))
  );

  test "no spaces around arrow/not" (fun () ->
    assert (equal_list (tokenize "~X->Y") ["~"; "X"; "->"; "Y"])
  );

  test "no spaces around arrow/not parse" (fun () ->
    let ast = parse (tokenize "~X->Y") in
    assert (equal_formula ast (
      Imp (Not (Var "X"), Var "Y")
    ))
  );

  test "chained implications" (fun () ->
    assert (equal_list (tokenize "A->B->C") ["A"; "->"; "B"; "->"; "C"])
  );

  test "chained implications parse" (fun () ->
    let ast = parse (tokenize "A->B->C") in
    assert (equal_formula ast (
      Imp (Var "A", Imp (Var "B", Var "C"))
    ))
  );

  test "OR without spaces" (fun () ->
    assert (equal_list (tokenize "A|B") ["A"; "|"; "B"])
  );

  test "OR without spaces parse" (fun () ->
    let ast = parse (tokenize "A|B") in
    assert (equal_formula ast (Or (Var "A", Var "B")))
  );

  test "double OR" (fun () ->
    assert (equal_list (tokenize "A||B") ["A"; "|"; "|"; "B"])
  );

test "double OR parse" (fun () ->
    try
      let _ = parse (tokenize "A||B") in
      assert false
    with Failure _ -> ()
  );

  test "single paren" (fun () ->
    assert (equal_list (tokenize "(") ["("])
  );

  test "single paren parse" (fun () ->
    try
      let _ = parse (tokenize "(") in
      assert false
    with Failure _ -> ()
  );


  test "mixed parens" (fun () ->
    assert (equal_list (tokenize "((A))") ["("; "("; "A"; ")"; ")"])
  );

  test "mixed parens parse" (fun () ->
    let ast = parse (tokenize "((A))") in
    assert (equal_formula ast (Var "A"))
  );

  test "single dash error" (fun () ->
    try
      let _ = tokenize "A - B" in
      assert false
    with Failure msg ->
      assert (String.contains msg '-')
  );

  test "invalid symbol" (fun () ->
    try
      let _ = tokenize "foo @ bar" in
      assert false
    with Failure msg ->
      assert (String.contains msg '@')
  );


