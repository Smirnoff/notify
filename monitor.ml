open Core.Std
open Lwt

let handle_updated_item item_id =
  Lwt.return ()

let handle_updated_profile profile_id =
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
