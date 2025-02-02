let auth_field = Dream.new_field ()

let get_token_from_request request =
  Option.bind (Dream.header request "X-Api-Key")
  @@ fun enc ->
  Option.bind (Dream.from_base64url enc)
  @@ fun key -> Dream.decrypt ~associated_data:"uuid" request key

let authenticate_requests inner_handler request =
  match get_token_from_request request with
  | None -> Dream.empty `Unauthorized
  | Some uuid ->
    (match%lwt Dream.sql request (Models.User.select_user_by_uuid ~uuid) with
     | Ok None | Error _ -> Dream.empty `Unauthorized
     | Ok (Some u) ->
       let () = Dream.set_field request auth_field u.uuid in
       inner_handler request)

let allow_cross_origins url inner_handler request =
  let%lwt response =
    if Dream.methods_equal (Dream.method_ request) `OPTIONS
    then Dream.empty `OK
    else inner_handler request
  in
  let headers =
    [ "Access-Control-Allow-Origin", url
    ; "Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS"
    ; "Access-Control-Allow-Headers", "Content-Type, X-Api-Key"
    ]
  in
  let () = List.iter (fun (h, v) -> Dream.set_header response h v) headers in
  Lwt.return response
