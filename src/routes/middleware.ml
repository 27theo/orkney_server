let field = Dream.new_field ()

let authenticate_requests inner_handler request =
  match Dream.cookie request "uuid" with
  | Some uuid -> (
      match%lwt Dream.sql request (Models.User.select_user_by_uuid ~uuid) with
      | Ok _ ->
          let () = Dream.set_field request field uuid in
          inner_handler request
      | Error _ -> Dream.empty `Unauthorized)
  | None -> Dream.empty `Unauthorized

let allow_cross_origins url inner_handler request =
  let%lwt response =
    if Dream.methods_equal (Dream.method_ request) `OPTIONS then Dream.empty `OK
    else inner_handler request
  in
  let headers =
    [
      ("Access-Control-Allow-Origin", url);
      ("Access-Control-Allow-Methods", "GET, POST, PUT, PATCH, DELETE, OPTIONS");
      ("Access-Control-Allow-Headers", "Content-Type");
    ]
  in
  let () = List.iter (fun (h, v) -> Dream.set_header response h v) headers in
  Lwt.return response
