module type CONN = Caqti_lwt.CONNECTION

module Room = struct
  type t = {
    ruid : string;
    name : string;
    is_active : bool;
    created_at : string;
    players : string;
    owner : string;
  }
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let room =
    let open Room in
    let intro ruid name is_active created_at players owner =
      { ruid; name; is_active; created_at; players; owner }
    in
    product intro
    @@ proj string (fun room -> room.ruid)
    @@ proj string (fun room -> room.name)
    @@ proj bool (fun room -> room.is_active)
    @@ proj string (fun room -> room.created_at)
    @@ proj string (fun room -> room.players)
    @@ proj string (fun room -> room.owner)
    @@ proj_end

  let create_room =
    t6 string string int string string string ->. unit @@
    "INSERT INTO rooms (ruid, name, is_active, created_at, players, owner) VALUES (?, ?, ?, ?, ?, ?)"
  [@@ocamlformat "disable"]

  let select_all_active_rooms =
    unit ->* room @@
    "SELECT ruid, name, is_active, created_at, players, owner FROM rooms WHERE is_active = 1"
  [@@ocamlformat "disable"]

  let select_room_by_ruid =
    string ->! room @@
    "SELECT ruid, name, is_active, created_at, players, owner from ROOMS WHERE ruid = ?"
  [@@ocamlformat "disable"]

  let deactivate_room =
    string ->. unit @@
    "UPDATE rooms SET is_active = 0 WHERE ruid = ?"
  [@@ocamlformat "disable"]

  let set_room_players =
    t2 string string ->. unit @@
    "UPDATE rooms SET players = ? WHERE ruid = ?"
  [@@ocamlformat "disable"]
end

let create_room ~ruid ~name ~is_active ~created_at ~players ~owner
    (module Conn : CONN) =
  Conn.exec Q.create_room (ruid, name, is_active, created_at, players, owner)

let select_all_active_rooms (module Conn : CONN) =
  let req = Q.select_all_active_rooms in
  let f room acc = Lwt.return_ok (room :: acc) in
  Conn.fold_s req f () []

let select_room_by_ruid ~ruid (module Conn : CONN) =
  Conn.find_opt Q.select_room_by_ruid ruid

let deactivate_room ~ruid (module Conn : CONN) =
  Conn.exec Q.deactivate_room ruid

let set_room_players ~ruid ~players (module Conn : CONN) =
  Conn.exec Q.set_room_players (players, ruid)
