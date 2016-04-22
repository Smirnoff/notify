open Core.Std
open Lwt

let handle_updated_item key =
  let open Lwt_io in
  lwt watchers = Api_user.user_ids_watching_item key in
  if watchers = []
  then Lwt.return ()
  else lwt json = Cached_hn_entry.Item.get_json key in
       lwt (potential_existing, item) =
           Cached_hn_entry.Item.process_new_json key json in
       (match potential_existing with
        | None -> printf "debug not watching yet #%d\n" key
        | Some existing ->
           if existing.Cached_hn_entry.hash = item.Cached_hn_entry.hash
           (* no change *)
           then Lwt.return ()
           else printf "debug has changed #%d\n" key) >>= fun () ->
       lwt success = Cached_hn_entry.Item.put_t item in
       if not success
       then printf "Failed to save new item #%d\n" key
       else Lwt.return ()

let handle_updated_profile key =
  Lwt.return ()

let one_cycle () =
  lwt updates = Updates.lwt_get () in
  let { Updates.items; profiles; } = updates in
  Lwt_list.iter_p handle_updated_item items >>= fun () ->
  Lwt_list.iter_p handle_updated_profile profiles

let one_cycle_delay () =
  one_cycle () >>= fun () ->
  Lwt_unix.sleep Config.update_poll_delay_cp#get

let rec test_run () =
  Lwt_main.run (one_cycle_delay ()) ;
  test_run ()

let rec thread_run () =
  lwt updates = Updates.lwt_get () in
  let { Updates.items; profiles; } = updates in
  Lwt_list.iter_p handle_updated_item items >>= fun () ->
  Lwt_list.iter_p handle_updated_profile profiles >>= fun () ->
  Lwt_unix.sleep Config.update_poll_delay_cp#get >>= fun () ->
  thread_run ()
