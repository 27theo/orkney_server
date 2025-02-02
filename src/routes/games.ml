open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type guid_json = { guid : string } [@@deriving yojson]
type game_list = { games : Services.Game.Game.t list } [@@deriving yojson]

let relevant_games request =
  match%lwt Services.Game.select_relevant_games ~request with
  | Ok glist ->
    yojson_of_game_list { games = glist } |> Yojson.Safe.to_string |> Dream.json
  | Error e ->
    Dream.log "Error fetching games from db: %s" (Caqti_error.show e);
    Json.error_response `Internal_Server_Error "Could not fetch games from the database"

let single_game request =
  let guid = Dream.param request "guid" in
  match%lwt Services.Game.select_game_by_guid ~request ~guid with
  | Ok (Some game) ->
    let%lwt game = Services.Game.parsed_game_of_game request game in
    game |> Services.Game.Game.yojson_of_t |> Yojson.Safe.to_string |> Dream.json
  | Ok None ->
    Json.error_response `Not_Found (Printf.sprintf "No such game %s found" guid)
  | Error e ->
    Dream.log "Error fetching games from db: %s" (Caqti_error.show e);
    Json.error_response
      `Internal_Server_Error
      (Printf.sprintf "Could not fetch game %s from the database" guid)

type create_game_params = { name : string } [@@deriving yojson]

let create_game =
  Json.json_receiver create_game_params_of_yojson
  @@ fun request (params : create_game_params) ->
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.create_inactive_game ~request ~name:params.name ~owner:uuid with
  | Ok () -> Json.message_response `OK (Printf.sprintf "Game created: %s" params.name)
  | Error code -> Json.error_response code "Could not create game"

let activate_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.activate_game ~request ~uuid ~guid with
  | Ok () -> { guid } |> yojson_of_guid_json |> Json.json_response ~status:`OK
  | Error code -> Json.error_response code "Could not activate game"

let join_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.add_user_to_game ~request ~uuid ~guid with
  | Ok () ->
    Json.message_response `OK (Printf.sprintf "User %s joined game: %s" uuid guid)
  | Error code -> Json.error_response code "Could not join game"

let leave_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.remove_user_from_game ~request ~uuid ~guid with
  | Ok () -> Json.message_response `OK (Printf.sprintf "User %s left game: %s" uuid guid)
  | Error code -> Json.error_response code "Could not leave game"

let delete_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.delete_game ~request ~uuid ~guid with
  | Ok () -> Json.message_response `OK (Printf.sprintf "Deleted game: %s" guid)
  | Error code -> Json.error_response code "Could not delete game"

let routes =
  [ Dream.get "/" relevant_games
  ; Dream.post "/create" create_game
  ; Dream.post "/activate/:guid" activate_game
  ; Dream.post "/join/:guid" join_game
  ; Dream.post "/leave/:guid" leave_game
  ; Dream.post "/delete/:guid" delete_game
  ; Dream.get "/:guid" single_game
  ]

let middleware = [ Middleware.authenticate_requests ]
