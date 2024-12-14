(*
type uuid = Uuid of string

let string_of_uuid (Uuid u) = u

let uuid_of_string_opt (s : string) : uuid option =
  if String.starts_with ~prefix:"user_" s then Some (Uuid s) else None

https://github.com/paurkedal/ocaml-caqti/blob/master/examples/bikereg.ml
*)

module type CONN = Caqti_lwt.CONNECTION

module User = struct
  type t = {
    uuid : string;
    username : string;
    email : string;
    hashed_password : string;
  }
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let user =
    let open User in
    let intro uuid username email hashed_password =
      { uuid; username; email; hashed_password }
    in
    product intro
    @@ proj string (fun user -> user.uuid)
    @@ proj string (fun user -> user.username)
    @@ proj string (fun user -> user.email)
    @@ proj string (fun user -> user.hashed_password)
    @@ proj_end

  let create_user =
    t4 string string string string ->. unit @@
    "INSERT INTO users (uuid, username, email, hashed_password) VALUES (?, ?, ?, ?)"
  [@@ocamlformat "disable"]

  let select_user_by_username =
    string ->? user @@
    "SELECT * FROM users WHERE username = ?"
  [@@ocamlformat "disable"]
end

let create_user ~uuid ~username ~email ~hashed_password (module Conn : CONN) =
  Conn.find_opt Q.create_user (uuid, username, email, hashed_password)

let select_user_by_username ~username (module Conn : CONN) =
  Conn.find_opt Q.select_user_by_username username
