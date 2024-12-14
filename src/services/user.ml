let create_user ~username ~password ~email =
  let slug = Dream.to_base64url (Dream.random 8) in
  let uuid = "user_" ^ slug in
  let hashed_password = Bcrypt.hash password |> Bcrypt.string_of_hash in
  Models.User.create_user ~uuid ~username ~email ~hashed_password

let select_user_by_username ~username =
  Models.User.select_user_by_username ~username

let authenticate_user request ~username ~password =
  match%lwt Dream.sql request (select_user_by_username ~username) with
  | Ok (Some user) ->
      Lwt.return
      @@ Bcrypt.verify password (Bcrypt.hash_of_string user.hashed_password)
  | _ -> Lwt.return_false
