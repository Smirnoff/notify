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
