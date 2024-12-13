(* https://jsthomas.github.io/ocaml-dream-api.html *)

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type signup_params = { username : string; password : string; email : string } [@@deriving yojson]

let signup =
  let signup_base params request =
    match%lwt
      Dream.sql request
        (Services.User.create_user ~username:params.username ~password:params.password ~email:params.email)
    with
    | Ok _ -> Dream.empty `OK
    | Error _ -> Dream.empty `Forbidden
  in
  Utils.json_receiver signup_params_of_yojson signup_base

let login _ = Dream.html "TODO"
let logout _ = Dream.html "TODO"
let routes = [ Dream.post "/signup" signup; Dream.post "/login" login; Dream.post "/logout" logout ]
