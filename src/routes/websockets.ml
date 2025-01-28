open Utils

let watch_games _ =
  let got_websocket websocket got =
    match got with
    | Some msg ->
        let _ = Dream.send websocket ("received: " ^ msg) in
        Lwt.return_true
    | _ ->
        let _ = Dream.close_websocket websocket in
        Lwt.return_false
  in
  let got_update websocket () =
    let _ = Dream.send websocket "(updates!)" in
    Lwt.return_true
  in
  let receiver websocket =
    Task.make @@ fun () ->
    Lwt.bind (Dream.receive websocket) (got_websocket websocket)
  in
  let watcher websocket =
    Task.make @@ fun () -> Lwt.bind (Lwt_unix.sleep 2.) (got_update websocket)
  in
  Dream.websocket @@ fun websocket ->
  [ receiver websocket; watcher websocket ] |> Task.multitask_websocket

let routes = [ Dream.get "/games" watch_games ]
