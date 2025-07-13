type formula =
  | Var of string
  | Not of formula
  | And of formula * formula
  | Or  of formula * formula
  | Imp of formula * formula
