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

let remove_change user_email id =
  let rec iter count =
    if count > Config.max_notify_attempt#get
    then (printf "Failed to remove change %s for user %s\n%!" id user_email;
          Lwt.return ())
    else (match_lwt Api_user.get_t_by_email_opt user_email with
          | None -> printf "No such user %s\n%!" user_email ;
                    Lwt.return ()
          | Some user ->
             let user = Api_user.remove_user_change user id in
             lwt success = Api_user.put_t user in
             if success
             then Lwt.return ()
             else iter (count + 1)) in
  iter 0

let rec ws_callback id req receive send =
  let uri = req |> Request.uri in
  match Uri.path uri with
  | "/notifications" ->
     (match_lwt Api_main.current_user_with_token false req false with
      | Some (user, token) ->
         (match user.Api_user.changes with
          | next :: rest ->
             let out_frame =
               Websocket_lwt.Frame.create
                 ~content:(Yojson.Basic.to_string
                             (Cached_hn_entry.json_of_change next)) () in
             lwt () = send out_frame in
             lwt in_frame = receive () in
             let content = in_frame.Websocket_lwt.Frame.content in
             (match Yojson.Basic.from_string content |> command_of_json with
              | RemoveChange id ->
                 remove_change user.Api_user.email id) >>= fun () ->
             ws_callback id req receive send
          | [] ->
             Lwt_unix.sleep Config.ws_poll_delay_cp#get >>= fun () ->
             ws_callback id req receive send)
      | _ -> Lwt.return ())
  | _ -> Lwt.return ()

let thread_run () =
  Websocket_lwt.establish_standard_server
    ~ctx:Conduit_lwt_unix.default_ctx
    ~mode:(`TCP (`Port Config.ws_port_cp#get))
    ws_callback
