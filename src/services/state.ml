open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type player_state =
  { uuid : string
  ; username : string
  ; gold : int
  }
[@@deriving yojson]

type t = { player_states : player_state list } [@@deriving yojson]

let empty = ""

let parsed_state_of_state state =
  if String.equal "" state
  then { player_states = [] }
  else state |> Yojson.Safe.from_string |> t_of_yojson

let initial_player_state (player : Models.User.User.t) =
  { uuid = player.uuid; username = player.username; gold = 0 }

let new_initial_state plist = List.map initial_player_state plist
