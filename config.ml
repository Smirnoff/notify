open Config_file
open Core.Std

let group = new group
let access_token_timeout_cp =
  new int_cp ~group
      ["access_token_timeout"]
      (* 60 seconds times 60 minutes times 24 hours times 30 days *)
      (60 * 60 * 24 * 30)
      "Access token timeout in seconds"
let update_poll_delay_cp =
  new float_cp ~group
      ["update_poll_delay"]
      1.0
      "Update poll delay in seconds (float)"
let ws_poll_delay_cp =
  new float_cp ~group
      ["ws_poll_delay"]
      1.0
      "websocket stream poll delay in seconds (float)"
let ws_port_cp =
  new int_cp ~group
      ["ws_port"]
      9000
      "websocket stream port"
let max_notify_attempt =
  new int_cp ~group
      ["max_notify_attempt"]
      100
      "Maximum number of attempts that will be made to notify a user of a change"
let mailgun_domain_cp =
  new string_cp ~group ["mailgun_domain"] "" "Mailgun domain"
let mailgun_api_key_cp =
  new string_cp ~group ["mailgun_api_key"] "" "Mailgun API key"
let couchdb_server_url_cp =
  new string_cp ~group ["couchdb_server_url"] "http://127.0.0.1:5984" "Couchdb Server URL"
let couchdb_database_name_cp =
  new string_cp ~group ["couchdb_database_name"] "hnnotify" "Couchdb Database Name"

let streaming_ping_frequency_cp =
  new float_cp ~group ["streaming_ping_frequency"] 10. "Frequency we send out ping frames for websocket"

let reject_outside_connections_cp =
  new bool_cp ~group ["reject_outside_connections"] true "If true, the HTTP API will reject non localhost connections"

let database_url () =
  couchdb_server_url_cp#get ^ "/" ^ (Uri.pct_encode couchdb_database_name_cp#get)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let database_uri () =
  database_url () |> Uri.of_string
