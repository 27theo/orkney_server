let string_of_players_list l = String.concat "," l
let players_list_of_string s = String.split_on_char ',' s

let create_inactive_game ~request ~name ~owner =
  Dream.sql request
  @@
  (* TODO: Change owner to uuid here and throughout lib *)
  (* TODO: Check if game name is unique against the inactive games *)
  let guid = Utils.generate_uid "game_" in
  let is_active = false in
  let created_at = Unix.time () |> int_of_float |> string_of_int in
  let players = string_of_players_list [ owner ] in
  Models.Game.create_game ~guid ~name ~is_active ~created_at ~players ~owner

let select_all_active_games ~request =
  Dream.sql request @@ Models.Game.select_all_active_games

let select_all_inactive_games ~request =
  Dream.sql request @@ Models.Game.select_all_inactive_games

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
