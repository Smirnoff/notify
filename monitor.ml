open Core.Std
open Lwt

let notify_watcher change id =
  let open Lwt_io in
  let rec attempt count =
    if count > Config.max_notify_attempt#get
    then printf "Error: Failed to notify user %s of a change\n" id
    else (lwt user = Api_user.get_t id in
          let user = Api_user.notify user change in
          lwt success = Api_user.put_t user in
          if success
          then Lwt.return ()
          else attempt (count + 1)) in
  attempt 0

let notify_watchers watchers change_key old_entry new_json =
  let old_json = old_entry.Cached_hn_entry.last_instance in
  let change = Cached_hn_entry.delta_with_key old_json new_json change_key in
  Lwt_list.iter_p (notify_watcher change) watchers

let handle_updated_item key =
  let open Lwt_io in
  lwt watchers = Api_user.user_ids_watching_item key in
  if watchers = []
  then Lwt.return ()
  else lwt json = Cached_hn_entry.Item.get_json key in
       lwt (potential_existing, item) =
           Cached_hn_entry.Item.process_new_json key json in
       let changed = if potential_existing = None
                     then false
                     else ((Option.value_exn potential_existing).Cached_hn_entry.hash) <> item.Cached_hn_entry.hash in
       (match potential_existing with
        | None -> printf "debug not watching yet %d\n" key
        | Some existing ->
           if existing.Cached_hn_entry.hash = item.Cached_hn_entry.hash
           (* no change *)
           then Lwt.return ()
           else printf "debug has changed %d\n" key) >>= fun () ->
       lwt success = Cached_hn_entry.Item.put_t item in
       if not success
       then printf "Failed to save new item %d\n" key
       else (if changed
             then notify_watchers watchers
                                  (Cached_hn_entry.Item.change_key_of_key key)
                                  (Option.value_exn potential_existing)
                                  json
             else Lwt.return ())

let handle_updated_profile key =
  let open Lwt_io in
  lwt watchers = Api_user.user_ids_watching_user key in
  if watchers = []
  then Lwt.return ()
  else lwt json = Cached_hn_entry.User.get_json key in
       lwt (potential_existing, item) =
           Cached_hn_entry.User.process_new_json key json in
       let changed = if potential_existing = None
                     then false
                     else ((Option.value_exn potential_existing).Cached_hn_entry.hash) <> item.Cached_hn_entry.hash in
       (match potential_existing with
        | None -> printf "debug not watching yet %s\n" key
        | Some existing ->
           if existing.Cached_hn_entry.hash = item.Cached_hn_entry.hash
           (* no change *)
           then Lwt.return ()
           else printf "debug has changed %s\n" key) >>= fun () ->
       lwt success = Cached_hn_entry.User.put_t item in
       if not success
       then printf "Failed to save new item %s\n" key
       else (if changed
             then notify_watchers watchers
                                  (Cached_hn_entry.User.change_key_of_key key)
                                  (Option.value_exn potential_existing)
                                  json
             else Lwt.return ())

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
