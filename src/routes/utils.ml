open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type error_response = { error : string } [@@deriving yojson]

let json_response ?status x = x |> Yojson.Safe.to_string |> Dream.json ?status

let json_receiver json_parser handler request =
  let%lwt body = Dream.body request in
  match try Ok (body |> Yojson.Safe.from_string |> json_parser) with _ -> Error () with
  | Ok doc -> handler doc request
  | Error _ -> { error = "Invalid input" } |> yojson_of_error_response |> json_response ~status:`Bad_Request
