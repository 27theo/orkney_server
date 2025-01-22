module type CONN = Caqti_lwt.CONNECTION

module Game = struct
  type t = {
    guid : string;
    name : string;
    is_active : bool;
    created_at : string;
    players : string;
    owner : string;
    state : string;
  }
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let game =
    let open Game in
    let intro guid name is_active created_at players owner state =
      { guid; name; is_active; created_at; players; owner; state }
    in
    product intro
    @@ proj string (fun game -> game.guid)
    @@ proj string (fun game -> game.name)
    @@ proj bool (fun game -> game.is_active)
    @@ proj string (fun game -> game.created_at)
    @@ proj string (fun game -> game.players)
    @@ proj string (fun game -> game.owner)
    @@ proj string (fun game -> game.state)
    @@ proj_end

  let create_game =
    t7 string string bool string string string string ->. unit @@
    "INSERT INTO games (guid, name, is_active, created_at, players, owner, state) VALUES (?, ?, ?, ?, ?, ?, ?)"

  let select_relevant_games =
    (* TODO: Implement relevance *)
    unit ->* game @@
    "SELECT guid, name, is_active, created_at, players, owner, state FROM games"

  let select_game_by_guid =
    string ->! game @@
    "SELECT guid, name, is_active, created_at, players, owner, state FROM games WHERE guid = ?"

  let activate_game =
    t2 string string ->. unit @@
    "UPDATE games SET is_active = 1, state = ? WHERE guid = ?"

  let deactivate_game =
    string ->. unit @@
    "UPDATE games SET is_active = 0 WHERE guid = ?"

  let set_game_players =
    t2 string string ->. unit @@
    "UPDATE games SET players = ? WHERE guid = ?"

  let delete_game =
    string ->. unit @@
    "DELETE FROM games WHERE guid = ?"
end
[@@ocamlformat "disable"]

let create_game ~guid ~name ~is_active ~created_at ~players ~owner ~state
    (module Conn : CONN) =
  Conn.exec Q.create_game
    (guid, name, is_active, created_at, players, owner, state)

let select_relevant_games (module Conn : CONN) =
  let req = Q.select_relevant_games in
  let f game acc = Lwt.return_ok (game :: acc) in
  Conn.fold_s req f () []

let select_game_by_guid ~guid (module Conn : CONN) =
  Conn.find_opt Q.select_game_by_guid guid

let activate_game ~guid ~state (module Conn : CONN) =
  Conn.exec Q.activate_game (state, guid)

let deactivate_game ~guid (module Conn : CONN) =
  Conn.exec Q.deactivate_game guid

let set_game_players ~guid ~players (module Conn : CONN) =
  Conn.exec Q.set_game_players (players, guid)

let delete_game ~guid (module Conn : CONN) = Conn.exec Q.delete_game guid
