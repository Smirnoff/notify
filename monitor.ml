open Core.Std
open Lwt

let notify_watchers watchers hn_id old_entry json =
  let old_json = old_entry.Cached_hn_structure.last_instance in
  let event = Cached_hn_structure.change_event ~hn_id ~api_notified:watchers old_json json in
  lwt success = Event.put_t event in
  if success
  then Lwt.return ()
  else Lwt_io.printf "Failed to notify watchers\n"

let handle_updated_item item_id =
  let open Lwt_io in
  let key = `Int item_id in
  lwt watchers = Api_user.user_ids_watching_item item_id in
  if watchers = []
  then Lwt.return ()
  else lwt json = Cached_hn_structure.get_json key in
       lwt (potential_existing, item) =
           Cached_hn_structure.process_new_json key json in
       let changed = if potential_existing = None
                     then false
                     else ((Option.value_exn potential_existing).Cached_hn_structure.hash) <> item.Cached_hn_structure.hash in
       (match potential_existing with
        | None -> printf "debug not watching yet %d\n" item_id
        | Some existing ->
           if existing.Cached_hn_structure.hash = item.Cached_hn_structure.hash
           (* no change *)
           then Lwt.return ()
           else printf "debug has changed %d\n" item_id) >>= fun () ->
       lwt success = Cached_hn_structure.put_t item in
       if not success
       then printf "Failed to save new item %d\n" item_id
       else if changed
       then notify_watchers watchers key (Option.value_exn potential_existing) json
       else Lwt.return ()

let handle_updated_profile profile_id =
  let open Lwt_io in
  let key = `String profile_id in
  lwt watchers = Api_user.user_ids_watching_user profile_id in
  if watchers = []
  then Lwt.return ()
  else lwt json = Cached_hn_structure.get_json key in
       lwt (potential_existing, item) =
           Cached_hn_structure.process_new_json key json in
       let changed = if potential_existing = None
                     then false
                     else ((Option.value_exn potential_existing).Cached_hn_structure.hash) <> item.Cached_hn_structure.hash in
       (match potential_existing with
        | None -> printf "debug not watching yet %s\n" profile_id
        | Some existing ->
           if existing.Cached_hn_structure.hash = item.Cached_hn_structure.hash
           (* no change *)
           then Lwt.return ()
           else printf "debug has changed %s\n" profile_id) >>= fun () ->
       lwt success = Cached_hn_structure.put_t item in
       if not success
       then printf "Failed to save new item %s\n" profile_id
       else if changed
       then notify_watchers watchers key (Option.value_exn potential_existing) json
       else Lwt.return ()

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
