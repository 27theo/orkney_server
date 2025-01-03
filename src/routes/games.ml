open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type guid_param = { guid : string } [@@deriving yojson]

type game_list_node = {
  guid : string;
  name : string;
  is_active : bool;
  created_at : string;
  players : string list;
  owner : string;
}
[@@deriving yojson]

type game_list_response = { games : game_list_node list } [@@deriving yojson]

let inactive_games request =
  match%lwt Services.Game.select_all_inactive_games ~request with
  | Ok rlist ->
      let rnodes =
        List.map
          (fun (g : Models.Game.Game.t) ->
            let plist = Services.Game.players_list_of_string g.players in
            {
              guid = g.guid;
              name = g.name;
              is_active = g.is_active;
              created_at = g.created_at;
              players = plist;
              owner = g.owner;
            })
          rlist
      in
      yojson_of_game_list_response { games = rnodes }
      |> Yojson.Safe.to_string |> Dream.json
  | Error e ->
      let () =
        Dream.log "Error fetching games from db: %s" (Caqti_error.show e)
      in
      Utils.make_error_response `Internal_Server_Error
        "Could not fetch games from the database"

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

let join_game =
  let join_game_base request (params : guid_param) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt
      Services.Game.add_user_to_game ~request ~uuid ~guid:params.guid
    with
    | Ok () ->
        Utils.make_message_response `OK
          (Printf.sprintf "User %s joined game: %s" uuid params.guid)
    | Error code -> Utils.make_error_response code "Could not join game"
  in
  Utils.json_receiver guid_param_of_yojson join_game_base

let routes =
  [
    Dream.get "/inactive" inactive_games;
    Dream.post "/create" create_game;
    Dream.post "/activate" activate_game;
    Dream.post "/join" join_game;
  ]

let middleware = [ Middleware.authenticate_requests ]
