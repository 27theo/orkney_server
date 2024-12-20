(* TODO: Dream.sql should be moved here! *)

let create_user ~request ~username ~password ~email =
  Dream.sql request
  @@
  let uuid = Utils.generate_uid "user_" in
  let hashed_password = Bcrypt.hash password |> Bcrypt.string_of_hash in
  Models.User.create_user ~uuid ~username ~email ~hashed_password

let select_user_by_username ~request ~username =
  Dream.sql request @@ Models.User.select_user_by_username ~username

let select_user_by_uuid request ~uuid =
  Dream.sql request @@ Models.User.select_user_by_uuid ~uuid

let authenticate_user ~request ~username ~password =
  match%lwt select_user_by_username ~request ~username with
  | Ok (Some user) ->
      if Bcrypt.verify password (Bcrypt.hash_of_string user.hashed_password)
      then Lwt.return_ok user
      else Lwt.return_error ()
  | _ -> Lwt.return_error ()
