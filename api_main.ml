open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

(* path -> callback *)
let api_calls = String.Table.create ()

let server =
  let callback conn req body =
    let uri = req |> Request.uri in
    match Hashtbl.find api_calls (Uri.path uri) with
    | None      -> Server.respond_error ~status:`I_m_a_teapot
                                        ~body:"No such call\n" ()
    | Some func -> func conn req body in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let main () =
  ignore (Lwt_main.run server)
