open Ocaml_logic_parser.Lexer

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

  test "simple AND" (fun () ->
    assert (equal_list (tokenize "A & B") ["A"; "&"; "B"])
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


