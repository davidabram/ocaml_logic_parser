open Ocaml_logic_parser.Lexer
open Ocaml_logic_parser.Parser
open Ocaml_logic_parser.Print

let () =
  print_string "Enter a logical formula: ";
  let input = read_line () in
  let tokens = tokenize input in
  let ast = parse tokens in
  print_truth_table ast
