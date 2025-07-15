let tokenize str =
  let len = String.length str in

  let is_var_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false
  in

  let rec scan_ident_end i =
    if i < len && is_var_char str.[i] then
      scan_ident_end (i + 1)
    else
      i
  in

  let rec aux i acc =
    if i >= len then
      List.rev acc
    else
    match str.[i] with

    | ' ' | '\t' | '\n' -> aux (i + 1) acc

    | '-' when i + 1 < len && str.[i+1] = '>' ->
        aux (i + 2) ("->" :: acc)

    | '(' | ')' | '~' | '&' | '|' as sym ->
      aux (i + 1) (String.make 1 sym :: acc)

    | c when is_var_char c ->
        let j = scan_ident_end i in
        let var = String.sub str i (j - i) in
        aux j (var :: acc)

    | c -> failwith (Printf.sprintf "Unexpected character '%c' at position %d" c i)
  in

  aux 0 []
