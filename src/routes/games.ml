open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type guid_param = { guid : string } [@@deriving yojson]
type game_list = { games : Services.Game.game list } [@@deriving yojson]

let relevant_games request =
  match%lwt Services.Game.select_relevant_games ~request with
  | Ok glist ->
      yojson_of_game_list { games = glist }
      |> Yojson.Safe.to_string |> Dream.json
  | Error e ->
      let () =
        Dream.log "Error fetching games from db: %s" (Caqti_error.show e)
      in
      Utils.make_error_response `Internal_Server_Error
        "Could not fetch games from the database"

let single_game request =
  let guid = Dream.param request "guid" in
  match%lwt Services.Game.select_game_by_guid ~request ~guid with
  | Ok (Some game) ->
      let%lwt game = Services.Game.game_with_usernames_of_game ~request game in
      game |> Services.Game.yojson_of_game |> Yojson.Safe.to_string
      |> Dream.json
  | Ok None ->
      Utils.make_error_response `Not_Found
        (Printf.sprintf "No such game %s found" guid)
  | Error e ->
      let () =
        Dream.log "Error fetching games from db: %s" (Caqti_error.show e)
      in
      Utils.make_error_response `Internal_Server_Error
        (Printf.sprintf "Could not fetch game %s from the database" guid)

type create_game_params = { name : string } [@@deriving yojson]

let create_game =
  let create_game_base request (params : create_game_params) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt
      Services.Game.create_inactive_game ~request ~name:params.name ~owner:uuid
    with
    | Ok () ->
        Utils.make_message_response `OK
          (Printf.sprintf "Game created: %s" params.name)
    | Error e ->
        let () = Dream.log "Error creating game: %s" (Caqti_error.show e) in
        Utils.make_error_response `Bad_Request "Could not create game"
  in
  Utils.json_receiver create_game_params_of_yojson create_game_base

let activate_game =
  let activate_game_base request (params : guid_param) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt Services.Game.activate_game ~request ~uuid ~guid:params.guid with
    | Ok () ->
        (* TODO: Return the active game's URL here *)
        Utils.make_message_response `OK
          (Printf.sprintf "Game activated: %s" params.guid)
    | Error code -> Utils.make_error_response code "Could not activate game"
  in
  Utils.json_receiver guid_param_of_yojson activate_game_base

let join_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.add_user_to_game ~request ~uuid ~guid with
  | Ok () ->
      Utils.make_message_response `OK
        (Printf.sprintf "User %s joined game: %s" uuid guid)
  | Error code -> Utils.make_error_response code "Could not join game"

let leave_game request =
  let guid = Dream.param request "guid" in
  let uuid = Dream.field request Middleware.auth_field |> Option.get in
  match%lwt Services.Game.remove_user_from_game ~request ~uuid ~guid with
  | Ok () ->
      Utils.make_message_response `OK
        (Printf.sprintf "User %s left game: %s" uuid guid)
  | Error code -> Utils.make_error_response code "Could not leave game"

let routes =
  [
    Dream.get "/" relevant_games;
    Dream.get "/:guid" single_game;
    Dream.post "/create" create_game;
    Dream.post "/activate" activate_game;
    Dream.post "/join/:guid" join_game;
    Dream.post "/leave/:guid" leave_game;
  ]

let middleware = [ Middleware.authenticate_requests ]
