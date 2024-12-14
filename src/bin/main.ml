let get_config key =
  try Sys.getenv key
  with _ ->
    failwith (Printf.sprintf "Please define %s in the environment" key)

let routes =
  [
    Dream.scope "/" [] Routes.Index.routes;
    Dream.scope "/auth" [] Routes.Auth.routes;
  ]

let () =
  let () = Dotenv.export () in
  Dream.run @@ Dream.logger
  @@ Dream.set_secret (get_config "SECRET_KEY")
  @@ Dream.sql_pool (get_config "DATABASE_URL")
  @@ Dream.sql_sessions @@ Dream.router routes
