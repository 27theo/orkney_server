module Config = struct
  let database_url () =
    try Sys.getenv "DATABASE_URL"
    with _ -> failwith "Please define DATABASE_URL in the environment"

  let secret_key () =
    try Sys.getenv "SECRET_KEY"
    with _ -> failwith "Please define SECRET_KEY in the environment"
end

let () =
  let () = Dotenv.export () in
  Dream.run @@ Dream.logger
  @@ Dream.set_secret (Config.secret_key ())
  @@ Dream.sql_pool (Config.database_url ())
  @@ Dream.sql_sessions
  @@ Dream.router Routes.Router.router
