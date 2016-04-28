open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

type command =
  | RemoveEvent of string

let command_of_json json =
  let open Yojson.Basic.Util in
  match member "type" json |> to_string with
  | "remove_event" -> RemoveEvent (member "event_id" json |> to_string)
  | _ -> failwith "bad command"

let remove_event user_email event_id =
  match_lwt Api_user.get_id_by_email_opt user_email with
  | None -> printf "No such user %s\n%!" user_email ;
            Lwt.return ()
  | Some user_id ->
     let rec iter count =
       if count > Config.max_notify_attempt#get
       then (printf "Failed to remove event %s for user %s\n%!"
                    event_id user_email;
             Lwt.return ())
       else (match_lwt Hn_event.get_t_opt event_id with
             | None -> printf "No such event %s\n%!" event_id ;
                       Lwt.return ()
             | Some event ->
                let event = Hn_event.remove_api_notified user_id event in
                lwt success = Hn_event.put_t_maybe_delete event in
                if success
                then Lwt.return ()
                else iter (count + 1)) in
     iter 0

let stream_events user id req receive send =
  let rec iter last_message =
    (match_lwt Api_main.current_user_with_token false req false with
     | None -> Lwt.return ()
     | Some (user, token) ->
        (match_lwt Hn_event.t_s_by_notified_id (Option.value_exn user.Api_user.id) with
         | next :: rest ->
            let out_frame =
              Websocket_lwt.Frame.create
                ~content:(Yojson.Basic.to_string
                            (Hn_event.public_json_of_t next)) () in
            lwt () = send out_frame in
            lwt in_frame = receive () in
            let content = in_frame.Websocket_lwt.Frame.content in
            (match Yojson.Basic.from_string content |> command_of_json with
             | RemoveEvent id ->
                remove_event user.Api_user.email id) >>= fun () ->
            iter (Core.Time.now ())
        | [] ->
           Lwt_unix.sleep Config.ws_poll_delay_cp#get >>= fun () ->
           if (Core.Time.diff (Core.Time.now ()) last_message |> Core.Span.to_sec) >
                Config.streaming_ping_frequency_cp#get
           then (lwt () = send (Websocket_lwt.Frame.create
                                  ~opcode:Websocket_lwt.Frame.Opcode.Ping ()) in
                 lwt _ = receive () in
                 iter (Core.Time.now ()))
           else iter last_message)) in
  iter (Core.Time.now ())

let ws_callback id req receive send =
  let uri = req |> Request.uri in
  match Uri.path uri with
  | "/notifications" ->
     (match_lwt Api_main.current_user_with_token false req false with
      | Some (user, token) -> stream_events user id req receive send
      | _ -> Lwt.return ())
  | _ -> Lwt.return ()

let thread_run () =
  Websocket_lwt.establish_standard_server
    ~ctx:Conduit_lwt_unix.default_ctx
    ~mode:(`TCP (`Port Config.ws_port_cp#get))
    ws_callback
