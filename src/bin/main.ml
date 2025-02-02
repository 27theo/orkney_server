let get_config key =
  try Sys.getenv key with
  | _ -> failwith (Printf.sprintf "Please define %s in the environment" key)

let routes =
  [ Dream.scope "/" [] Routes.Index.routes
  ; Dream.scope "/ws" [] Routes.Websockets.routes
  ; Dream.scope "/auth" [] Routes.Auth.routes
  ; Dream.scope "/games" Routes.Games.middleware Routes.Games.routes
  ]

let () =
  let () = Dotenv.export () in
  Dream.run
  @@ Dream.logger
  @@ Dream.set_secret (get_config "SECRET_KEY")
  @@ Dream.sql_pool (get_config "DATABASE_URL")
  @@ Routes.Middleware.allow_cross_origins (get_config "UI_URL")
  @@ Dream.router routes
