open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let string_of_players_list l = String.concat "," l
let players_list_of_string s = String.split_on_char ',' s

type game = {
  guid : string;
  name : string;
  is_active : bool;
  created_at : string;
  players : string list;
  owner : string;
}
[@@deriving yojson]

let game_with_usernames_of_game ~request (game : Models.Game.Game.t) =
  let plist = players_list_of_string game.players in
  let get_username uuid =
    let%lwt user = Dream.sql request (Models.User.select_user_by_uuid ~uuid) in
    Lwt.return @@ match user with Ok (Some u) -> u.username | _ -> "NOTFOUND"
  in
  let%lwt player_usernames = Lwt.all @@ List.map get_username plist in
  let%lwt owner_username = get_username game.owner in
  Lwt.return
    {
      guid = game.guid;
      name = game.name;
      is_active = game.is_active;
      created_at = game.created_at;
      players = player_usernames;
      owner = owner_username;
    }

let create_inactive_game ~request ~name ~owner =
  if Int.equal (String.length name) 0 then Lwt.return_error `Bad_Request
  else
    (* TODO: Change owner to uuid here and throughout lib *)
    (* TODO: Check if game name is unique against the inactive games *)
    let guid = Utils.generate_uid "game_" in
    let is_active = false in
    let created_at = Unix.time () |> int_of_float |> string_of_int in
    let players = string_of_players_list [ owner ] in
    match%lwt
      Dream.sql request
        (Models.Game.create_game ~guid ~name ~is_active ~created_at ~players
           ~owner)
    with
    | Ok () -> Lwt.return_ok ()
    | Error _ -> Lwt.return_error `Internal_Server_Error

let select_relevant_games ~request =
  match%lwt Dream.sql request Models.Game.select_relevant_games with
  | Error e -> Lwt.return_error e
  | Ok glist ->
      let%lwt glist_with_usernames =
        Lwt.all @@ List.map (game_with_usernames_of_game ~request) glist
      in
      Lwt.return_ok glist_with_usernames

let select_game_by_guid ~request ~guid =
  Dream.sql request @@ Models.Game.select_game_by_guid ~guid

let activate_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some r) ->
      if String.equal r.owner uuid && Bool.not r.is_active then
        match%lwt Dream.sql request (Models.Game.activate_game ~guid) with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error
      else Lwt.return_error `Bad_Request
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error

let add_user_to_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some r) -> (
      let plist = players_list_of_string r.players in
      if r.is_active || List.exists (String.equal uuid) plist then
        Lwt.return_error `Bad_Request
      else
        let plist = uuid :: plist in
        let players = string_of_players_list plist in
        match%lwt
          Dream.sql request (Models.Game.set_game_players ~guid ~players)
        with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error

let remove_user_from_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some r) -> (
      let plist = players_list_of_string r.players in
      if
        r.is_active || String.equal r.owner uuid
        || not (List.exists (String.equal uuid) plist)
      then Lwt.return_error `Bad_Request
      else
        let plist = List.filter (fun u -> not (String.equal uuid u)) plist in
        let players = string_of_players_list plist in
        match%lwt
          Dream.sql request (Models.Game.set_game_players ~guid ~players)
        with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error

let delete_game ~request ~uuid ~guid =
  match%lwt select_game_by_guid ~request ~guid with
  | Ok (Some r) -> (
      if not (String.equal r.owner uuid) then Lwt.return_error `Bad_Request
      else
        match%lwt Dream.sql request (Models.Game.delete_game ~guid) with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error
