module Task = struct
  type promise = bool Lwt.t
  type generator = unit -> promise
  type t = promise * generator

  let make (gen : generator) = gen (), gen

  let get_promise (task : t) =
    let p, _ = task in
    p

  let get_generator (task : t) =
    let _, g = task in
    g

  let refresh (task : t) =
    let p = get_promise task in
    if Lwt.is_sleeping p then task else get_generator task |> make

  let multitask_websocket (ts : t list) =
    let rec aux ts =
      let ps = List.map get_promise ts in
      match%lwt Lwt.choose ps with
      | true -> aux (List.map refresh ts)
      | false -> Lwt.return_unit
    in
    aux ts
end

let update_time = 0.5

let watch_games request =
  Dream.websocket
  @@ fun websocket ->
  let data = ref "" in
  let check_for_updates () =
    match%lwt Services.Game.select_relevant_games ~request with
    | Ok glist ->
      let serialized =
        Games.yojson_of_game_list { games = glist } |> Yojson.Safe.to_string
      in
      if not (String.equal !data serialized)
      then (
        if not (String.equal !data "")
        then Lwt.ignore_result @@ Dream.send websocket serialized;
        data := serialized);
      Lwt.return_true
    | Error e ->
      Dream.log "Error fetching games from db: %s" (Caqti_error.show e);
      Lwt.return_false
  in
  let updater =
    Task.make @@ fun () -> Lwt.bind (Lwt_unix.sleep update_time) check_for_updates
  in
  let socket_handler =
    Task.make
    @@ fun () ->
    Lwt.bind (Dream.receive websocket)
    @@ function
    | Some _ -> Lwt.return_true
    | _ ->
      Lwt.ignore_result @@ Dream.close_websocket websocket;
      Lwt.return_false
  in
  [ updater; socket_handler ] |> Task.multitask_websocket

let routes = [ Dream.get "/games" watch_games ]
