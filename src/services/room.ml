let string_of_players_list l = String.concat "," l
let players_list_of_string s = String.split_on_char ',' s

let create_active_room ~request ~name ~owner =
  Dream.sql request
  @@
  (* TODO: Change owner to uuid here and throughout lib *)
  (* TODO: Check if room name is unique against the active rooms *)
  let ruid = Utils.generate_uid "room_" in
  let is_active = 1 in
  let created_at = Unix.time () |> int_of_float |> string_of_int in
  let players = string_of_players_list [ owner ] in
  Models.Room.create_room ~ruid ~name ~is_active ~created_at ~players ~owner

let select_all_active_rooms ~request =
  Dream.sql request @@ Models.Room.select_all_active_rooms

let select_room_by_ruid ~request ~ruid =
  Dream.sql request @@ Models.Room.select_room_by_ruid ~ruid

let deactivate_room ~request ~uuid ~ruid =
  match%lwt select_room_by_ruid ~request ~ruid with
  | Ok (Some r) ->
      if String.equal r.owner uuid && r.is_active then
        match%lwt Dream.sql request (Models.Room.deactivate_room ~ruid) with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error
      else Lwt.return_error `Bad_Request
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error

let add_user_to_room ~request ~uuid ~ruid =
  match%lwt select_room_by_ruid ~request ~ruid with
  | Ok (Some r) -> (
      let plist = players_list_of_string r.players in
      if Bool.not r.is_active || List.exists (String.equal uuid) plist then
        Lwt.return_error `Bad_Request
      else
        let plist = uuid :: plist in
        let players = string_of_players_list plist in
        match%lwt
          Dream.sql request (Models.Room.set_room_players ~ruid ~players)
        with
        | Ok () -> Lwt.return_ok ()
        | Error _ -> Lwt.return_error `Internal_Server_Error)
  | Ok None -> Lwt.return_error `Bad_Request
  | Error _ -> Lwt.return_error `Internal_Server_Error
