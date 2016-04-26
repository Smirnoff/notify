open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

type command =
  | RemoveChange of string

let command_of_json json =
  let open Yojson.Basic.Util in
  match member "type" json |> to_string with
  | "remove_change" -> RemoveChange (member "change_id" json |> to_string)
  | _ -> failwith "bad command"

let rec ws_callback id req receive send =
  let uri = req |> Request.uri in
  match Uri.path uri with
  | _ -> Lwt.return ()

let thread_run () =
  Websocket_lwt.establish_standard_server
    ~ctx:Conduit_lwt_unix.default_ctx
    ~mode:(`TCP (`Port Config.ws_port_cp#get))
    ws_callback
