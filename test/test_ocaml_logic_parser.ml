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

  test "var with underscores" (fun () ->
    assert (equal_list (tokenize "x_1 -> Y2") ["x_1"; "->"; "Y2"])
  );

  test "whitespace skipping" (fun () ->
    assert (equal_list (tokenize "  A \t\n &\n B ") ["A"; "&"; "B"])
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

  test "only whitespace" (fun () ->
    assert (equal_list (tokenize "   \t\n  ") [])
  );

  test "digit-only identifier" (fun () ->
    assert (equal_list (tokenize "12345") ["12345"])
  );
  test "underscore-leading identifier" (fun () ->
    assert (equal_list (tokenize "_foo_bar") ["_foo_bar"])
  );

  test "no spaces around arrow/not" (fun () ->
    assert (equal_list (tokenize "~X->Y") ["~"; "X"; "->"; "Y"])
  );

  test "chained implications" (fun () ->
    assert (equal_list (tokenize "A->B->C") ["A"; "->"; "B"; "->"; "C"])
  );

  test "OR without spaces" (fun () ->
    assert (equal_list (tokenize "A|B") ["A"; "|"; "B"])
  );

  test "double OR" (fun () ->
    assert (equal_list (tokenize "A||B") ["A"; "|"; "|"; "B"])
  );

  test "single paren" (fun () ->
    assert (equal_list (tokenize "(") ["("])
  );

  test "mixed parens" (fun () ->
    assert (equal_list (tokenize "((A))") ["("; "("; "A"; ")"; ")"])
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

  test "implication" (fun () ->
    assert (equal_list (tokenize "A -> B") ["A"; "->"; "B"])
  );

  test "nested parens" (fun () ->
    assert (equal_list (tokenize "(X & Y) -> Z") ["("; "X"; "&"; "Y"; ")"; "->"; "Z"])
  );

  test "not operator" (fun () ->
    assert (equal_list (tokenize "~X") ["~"; "X"])
  );

  test "complex expr" (fun () ->
    assert (equal_list (tokenize "~(X | Y) & Z") ["~"; "("; "X"; "|"; "Y"; ")"; "&"; "Z"])
  );

  test "var with underscores" (fun () ->
    assert (equal_list (tokenize "x_1 -> Y2") ["x_1"; "->"; "Y2"])
  );

  test "whitespace skipping" (fun () ->
    assert (equal_list (tokenize "  A \t\n &\n B ") ["A"; "&"; "B"])
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

  test "only whitespace" (fun () ->
    assert (equal_list (tokenize "   \t\n  ") [])
  );

  test "digit-only identifier" (fun () ->
    assert (equal_list (tokenize "12345") ["12345"])
  );
  test "underscore-leading identifier" (fun () ->
    assert (equal_list (tokenize "_foo_bar") ["_foo_bar"])
  );

  test "no spaces around arrow/not" (fun () ->
    assert (equal_list (tokenize "~X->Y") ["~"; "X"; "->"; "Y"])
  );

  test "chained implications" (fun () ->
    assert (equal_list (tokenize "A->B->C") ["A"; "->"; "B"; "->"; "C"])
  );

  test "OR without spaces" (fun () ->
    assert (equal_list (tokenize "A|B") ["A"; "|"; "B"])
  );

  test "double OR" (fun () ->
    assert (equal_list (tokenize "A||B") ["A"; "|"; "|"; "B"])
  );

  test "single paren" (fun () ->
    assert (equal_list (tokenize "(") ["("])
  );

  test "mixed parens" (fun () ->
    assert (equal_list (tokenize "((A))") ["("; "("; "A"; ")"; ")"])
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

  test "parse simple implication" (fun () ->
    let ast = parse ["A"; "->"; "B"] in
    match ast with
    | Imp (Var "A", Var "B") -> ()
    | _ -> failwith "Incorrect AST"
  );

  test "parse nested precedence" (fun () ->
  let ast = parse ["~"; "A"; "&"; "B"; "->"; "C"] in
  match ast with
  | Imp (
      And (
        Not (Var "A"),
        Var "B"
      ),
      Var "C"
    ) -> ()
  | _ -> failwith "Incorrect AST structure"
);

