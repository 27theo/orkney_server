open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module Task = struct
  type promise = bool Lwt.t
  type generator = unit -> promise
  type t = promise * generator

  let make (gen : generator) = (gen (), gen)

  let get_promise (task : t) =
    let p, _ = task in
    p

  let get_generator (task : t) =
    let _, g = task in
    g

  let refresh (task : t) =
    let p = get_promise task in
    if Lwt.is_sleeping p then task else get_generator task |> make

  let multitask_websocket (ts : t list) =
    let rec aux ts =
      let ps = List.map get_promise ts in
      match%lwt Lwt.choose ps with
      | true -> aux (List.map refresh ts)
      | false -> Lwt.return_unit
    in
    aux ts
end

type error_response = { error : string } [@@deriving yojson]
type message_response = { message : string } [@@deriving yojson]

let json_response ?status x = x |> Yojson.Safe.to_string |> Dream.json ?status

let make_message_response status message =
  { message } |> yojson_of_message_response |> json_response ~status

let make_error_response status message =
  { error = message } |> yojson_of_error_response |> json_response ~status

let json_receiver json_parser handler request =
  let%lwt body = Dream.body request in
  match
    try Ok (body |> Yojson.Safe.from_string |> json_parser) with _ -> Error ()
  with
  | Ok doc -> handler request doc
  | Error _ ->
      make_error_response `Bad_Request
        "That JSON input is not valid for this request"
