open Utils

let update_time = 0.5

let watch_games request =
  Dream.websocket @@ fun websocket ->
  let data = ref "" in
  let check_for_updates () =
    match%lwt Services.Game.select_relevant_games ~request with
    | Ok glist ->
        let serialized =
          Games.yojson_of_game_list { games = glist } |> Yojson.Safe.to_string
        in
        let () =
          if not (String.equal !data serialized) then
            let _ =
              if not (String.equal !data "") then
                Dream.send websocket serialized
              else Lwt.return_unit
            in
            data := serialized
          else ()
        in
        Lwt.return_true
    | Error e ->
        let () =
          Dream.log "Error fetching games from db: %s" (Caqti_error.show e)
        in
        Lwt.return_false
  in
  let updater =
    Task.make @@ fun () ->
    Lwt.bind (Lwt_unix.sleep update_time) check_for_updates
  in
  let socket_handler =
    Task.make @@ fun () ->
    Lwt.bind (Dream.receive websocket) @@ function
    | Some _ -> Lwt.return_true
    | _ ->
        let _ = Dream.close_websocket websocket in
        Lwt.return_false
  in
  [ updater; socket_handler ] |> Task.multitask_websocket

let routes = [ Dream.get "/games" watch_games ]
