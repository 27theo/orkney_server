(* https://jsthomas.github.io/ocaml-dream-api.html *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type signup_params = { username : string; password : string; email : string }
[@@deriving yojson]

let signup =
  let signup_base request (params : signup_params) =
    match%lwt
      Services.User.create_user ~request ~username:params.username
        ~password:params.password ~email:params.email
    with
    | Ok _ ->
        Utils.make_message_response `OK
          (Printf.sprintf "User signed up: %s" params.username)
    | Error _ ->
        Utils.make_error_response `Bad_Request "Could not create account"
  in
  Utils.json_receiver signup_params_of_yojson signup_base

type login_params = { username : string; password : string } [@@deriving yojson]
type token_response = { token : string } [@@deriving yojson]

let login =
  let login_base request (params : login_params) =
    match%lwt
      Services.User.authenticate_user ~request ~username:params.username
        ~password:params.password
    with
    | Ok user ->
        let token =
          Dream.to_base64url
            (Dream.encrypt request ~associated_data:"uuid" user.uuid)
        in
        let json = { token } |> yojson_of_token_response in
        Utils.json_response ~status:`OK json
    | Error () -> Utils.make_error_response `Unauthorized "Login failed"
  in
  Utils.json_receiver login_params_of_yojson login_base

let routes = [ Dream.post "/signup" signup; Dream.post "/login" login ]
