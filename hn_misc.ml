open Lwt
open Cohttp
open Cohttp_lwt_unix
open Printf

let base_url = "https://hacker-news.firebaseio.com"
let api_version = "/v0"

let base_versioned_url = base_url ^ api_version

let lwt_get_json_uri uri =
  Client.get uri >>= fun (resp, body) ->
  body |> Cohttp_lwt_body.to_string >|=
  Yojson.Basic.from_string

let to_list_nullable json =
  match json with
  | `Null      -> []
  | `List item -> item
  | _          -> failwith (sprintf "Neither null nor list: %s"
                                    (Yojson.Basic.to_string json))

open Core.Std

let string_join joiner items =
  match items with
  | [] -> ""
  | [item] -> item
  | first :: items ->
     first ^ (List.fold ~init:""
                        ~f:(fun accum item -> accum ^ joiner ^ item) items)

let rec sorted_stringify_json json =
  let stringify_json = Yojson.Basic.to_string in
  let stringify_pair (key, json) =
    (sorted_stringify_json (`String key)) ^ ":" ^ (sorted_stringify_json json) in
  match json with
  | `Bool   _    -> stringify_json json
  | `Int    _    -> stringify_json json
  | `Float  _    -> stringify_json json
  | `String _    -> stringify_json json
  | `Null        -> stringify_json json
  | `List items  -> stringify_json json
  | `Assoc pairs ->
     pairs |>
       List.sort ~cmp:(fun (key_a, _) (key_b, _) -> compare key_a key_b) |>
       List.map ~f:stringify_pair |> fun lst ->
       "{" ^ (string_join "," lst) ^ "}"

let hash_json json =
  json |>
    sorted_stringify_json |>
    Cryptohash_sha256.bytes |>
    Cryptohash_sha256.to_hex
