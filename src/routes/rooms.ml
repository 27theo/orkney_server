open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type ruid_param = { ruid : string } [@@deriving yojson]

type room_list_node = {
  ruid : string;
  name : string;
  is_active : bool;
  created_at : string;
  players : string list;
  owner : string;
}
[@@deriving yojson]

type room_list_response = { rooms : room_list_node list } [@@deriving yojson]

let active_rooms request =
  match%lwt Services.Room.select_all_active_rooms ~request with
  | Ok rlist ->
      let rnodes =
        List.map
          (fun (r : Models.Room.Room.t) ->
            let plist = Services.Room.players_list_of_string r.players in
            {
              ruid = r.ruid;
              name = r.name;
              is_active = r.is_active;
              created_at = r.created_at;
              players = plist;
              owner = r.owner;
            })
          rlist
      in
      yojson_of_room_list_response { rooms = rnodes }
      |> Yojson.Safe.to_string |> Dream.json
  | Error e ->
      let () =
        Dream.log "Error fetching rooms from db: %s" (Caqti_error.show e)
      in
      Utils.make_error_response `Internal_Server_Error
        "Could not fetch rooms from the database"

type create_room_params = { name : string } [@@deriving yojson]

let create_room =
  let create_room_base request (params : create_room_params) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt
      Services.Room.create_active_room ~request ~name:params.name ~owner:uuid
    with
    | Ok () ->
        Utils.make_message_response `OK
          (Printf.sprintf "Room created: %s" params.name)
    | Error e ->
        let () = Dream.log "Error creating room: %s" (Caqti_error.show e) in
        Utils.make_error_response `Bad_Request "Could not create room"
  in
  Utils.json_receiver create_room_params_of_yojson create_room_base

let deactivate_room =
  let deactivate_room_base request (params : ruid_param) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt
      Services.Room.deactivate_room ~request ~uuid ~ruid:params.ruid
    with
    | Ok () ->
        Utils.make_message_response `OK
          (Printf.sprintf "Room deactivated: %s" params.ruid)
    | Error code -> Utils.make_error_response code "Could not deactivate room"
  in
  Utils.json_receiver ruid_param_of_yojson deactivate_room_base

let join_room =
  let join_room_base request (params : ruid_param) =
    let uuid = Dream.field request Middleware.auth_field |> Option.get in
    match%lwt
      Services.Room.add_user_to_room ~request ~uuid ~ruid:params.ruid
    with
    | Ok () ->
        Utils.make_message_response `OK
          (Printf.sprintf "User %s joined room: %s" uuid params.ruid)
    | Error code -> Utils.make_error_response code "Could not join room"
  in
  Utils.json_receiver ruid_param_of_yojson join_room_base

let routes =
  [
    Dream.get "/active" active_rooms;
    Dream.post "/create" create_room;
    Dream.post "/deactivate" deactivate_room;
    Dream.post "/join" join_room;
  ]

let middleware = [ Middleware.authenticate_requests ]
