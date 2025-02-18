open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let string_of_players_list l = String.concat "," l
let players_list_of_string s = String.split_on_char ',' s

module Game = struct
  (* TODO: This is a mess.
     How do we best handle different variations on the game type? *)
  type t =
    { guid : string
    ; name : string
    ; is_active : bool
    ; created_at : string
    ; players : string list
    ; owner : string
    ; state : State.t
    }
  [@@deriving yojson]
end

let get_users_from_game request (game : Models.Game.Game.t) =
  let open Lwt.Infix in
  List.map
    (fun uuid -> Dream.sql request @@ Models.User.select_user_by_uuid ~uuid)
    (players_list_of_string game.players)
  |> Lwt.all
  >|= List.map
      @@ fun user ->
      match user with
      | Ok (Some u) -> u
      | _ -> failwith "This should never happen - could not find a user from uuid in game"

let parsed_game_of_game ?(usernames = true) request (game : Models.Game.Game.t) =
  let open Game in
  let plist = players_list_of_string game.players in
  let get_username uuid =
    let%lwt user = Dream.sql request (Models.User.select_user_by_uuid ~uuid) in
    Lwt.return
    @@
    match user with
    | Ok (Some u) -> u.username
    | _ -> failwith "This should never happen - no user found"
  in
  let%lwt players =
    if usernames then Lwt.all @@ List.map get_username plist else Lwt.return plist
  in
  let%lwt owner_username = get_username game.owner in
  let state = State.parsed_state_of_state game.state in
  Lwt.return
    { guid = game.guid
    ; name = game.name
    ; is_active = game.is_active
    ; created_at = game.created_at
    ; players
    ; owner = owner_username
    ; state
    }

let create_inactive_game ~request ~name ~owner =
  if Int.equal (String.length name) 0
  then Lwt.return_error `Bad_Request
  else (
    (* TODO: Change owner to uuid here and throughout lib *)
    (* TODO: Check if game name is unique against the inactive games *)
    let guid = Uid.generate "game_" in
    let is_active = false in
    let created_at = Unix.time () |> int_of_float |> string_of_int in
    let players = string_of_players_list [ owner ] in
    let state = State.empty in
    match%lwt
      Dream.sql
        request
        (Models.Game.create_game
           ~guid
           ~name
           ~is_active
           ~created_at
           ~players
           ~owner
           ~state)
    with
    | Ok () -> Lwt.return_ok ()
    | Error _ ->
      Dream.log "Could not create game - database create failed";
      Lwt.return_error `Internal_Server_Error)

let select_relevant_games ~request =
  match%lwt Dream.sql request Models.Game.select_relevant_games with
  | Error e -> Lwt.return_error e
  | Ok glist ->
    let%lwt glist_with_usernames =
      Lwt.all @@ List.map (fun g -> parsed_game_of_game request g) glist
    in
    Lwt.return_ok glist_with_usernames

let select_game_by_guid ~request ~guid =
  Dream.sql request @@ Models.Game.select_game_by_guid ~guid

let activate_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some g) ->
    if String.equal g.owner uuid && Bool.not g.is_active
    then (
      let open State in
      let%lwt users = get_users_from_game request g in
      let player_states = new_initial_state users in
      let state = { player_states } |> yojson_of_t |> Yojson.Safe.to_string in
      match%lwt Dream.sql request (Models.Game.activate_game ~guid ~state) with
      | Ok () -> Lwt.return_ok ()
      | Error _ ->
        Dream.log "Could not activate game - database activate failed";
        Lwt.return_error `Internal_Server_Error)
    else Lwt.return_error `Bad_Request
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ ->
    Dream.log "Could not activate game - database select failed";
    Lwt.return_error `Internal_Server_Error

let add_user_to_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some g) ->
    let plist = players_list_of_string g.players in
    if g.is_active || List.exists (String.equal uuid) plist
    then Lwt.return_error `Bad_Request
    else (
      let plist = uuid :: plist in
      let players = string_of_players_list plist in
      match%lwt Dream.sql request (Models.Game.set_game_players ~guid ~players) with
      | Ok () -> Lwt.return_ok ()
      | Error _ ->
        Dream.log "Could not add user to game - database set players failed";
        Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ ->
    Dream.log "Could not add user to game - database select failed";
    Lwt.return_error `Internal_Server_Error

let remove_user_from_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some g) ->
    let plist = players_list_of_string g.players in
    if
      g.is_active
      || String.equal g.owner uuid
      || not (List.exists (String.equal uuid) plist)
    then Lwt.return_error `Bad_Request
    else (
      let plist = List.filter (fun u -> not (String.equal uuid u)) plist in
      let players = string_of_players_list plist in
      match%lwt Dream.sql request (Models.Game.set_game_players ~guid ~players) with
      | Ok () -> Lwt.return_ok ()
      | Error _ ->
        Dream.log "Could not remove user from game - database set players failed";
        Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ ->
    Dream.log "Could not remove user from game - database select failed";
    Lwt.return_error `Internal_Server_Error

let delete_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some g) ->
    if not (String.equal g.owner uuid)
    then Lwt.return_error `Bad_Request
    else (
      match%lwt Dream.sql request (Models.Game.delete_game ~guid) with
      | Ok () -> Lwt.return_ok ()
      | Error _ ->
        Dream.log "Could not delete a game - database delete failed";
        Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ ->
    Dream.log "Could not delete a game - database select failed";
    Lwt.return_error `Internal_Server_Error
