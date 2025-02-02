(* https://jsthomas.github.io/ocaml-dream-api.html *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Json = Services.Json
module User = Services.User

type token_doc = { token : string } [@@deriving yojson]

type signup_doc =
  { username : string
  ; password : string
  ; email : string
  }
[@@deriving yojson]

type login_doc =
  { username : string
  ; password : string
  }
[@@deriving yojson]

let signup =
  Json.json_receiver signup_doc_of_yojson
  @@ fun request (params : signup_doc) ->
  match%lwt
    User.create_user
      ~request
      ~username:params.username
      ~password:params.password
      ~email:params.email
  with
  | Ok (Some user) ->
    let token = User.token_of_user request user in
    { token } |> yojson_of_token_doc |> Json.json_response ~status:`OK
  | Ok None -> Json.error_response `Internal_Server_Error "Could not create account"
  | Error _ -> Json.error_response `Bad_Request "Could not create account"

let login =
  Json.json_receiver login_doc_of_yojson
  @@ fun request (params : login_doc) ->
  match%lwt
    User.authenticate_user ~request ~username:params.username ~password:params.password
  with
  | Ok user ->
    let token = User.token_of_user request user in
    let json = { token } |> yojson_of_token_doc in
    Json.json_response ~status:`OK json
  | Error () -> Json.error_response `Unauthorized "Login failed"

let routes = [ Dream.post "/signup" signup; Dream.post "/login" login ]
