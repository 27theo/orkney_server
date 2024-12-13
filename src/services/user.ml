let create_user ~username ~password ~email =
  let slug = Dream.to_base64url (Dream.random 8) in
  let uuid = "user_" ^ slug in
  let hashed_password = Bcrypt.hash password |> Bcrypt.string_of_hash in
  Models.User.create_user ~uuid ~username ~email ~hashed_password
