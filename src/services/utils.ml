let encode_base62 str =
  let int_to_base62_char n =
    if n < 10 then Char.chr (n + 48) (* 0-9 *)
    else if n < 36 then Char.chr (n + 55) (* A-Z *)
    else Char.chr (n + 61)
  in

  let string_to_bytes str =
    let rec aux i acc =
      if i < 0 then acc else aux (i - 1) (Char.code (String.get str i) :: acc)
    in
    aux (String.length str - 1) []
  in

  let bytes_to_base62_digits bytes =
    let base = 62 in
    let rec divide_all num acc =
      if num = 0 then acc else divide_all (num / base) ((num mod base) :: acc)
    in
    let rec process_bytes nums acc =
      match nums with
      | [] -> acc
      | n :: rest ->
          let new_num = (List.hd acc * 256) + n in
          let digits = divide_all new_num [] in
          process_bytes rest (digits @ List.tl acc)
    in
    match bytes with
    | [] -> [ 0 ]
    | first :: rest -> process_bytes rest [ first ]
  in

  str |> string_to_bytes |> bytes_to_base62_digits
  |> List.map int_to_base62_char
  |> List.to_seq |> String.of_seq

let generate_uid prefix = prefix ^ (Dream.random 8 |> encode_base62)
