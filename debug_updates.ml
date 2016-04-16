open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

open Printf

(* just pulled the size out of nowhere *)
let item_table = Int.Table.create () ~size:5

let handle_updated_item id =
  let open Lwt_io in
  Item.lwt_get_by_id id >>= fun item ->
  match Hashtbl.find item_table id with
  | None               -> Hashtbl.replace item_table id item ;
                          printf "New item %d\n%!" id
  | Some existing_item ->
     if item = existing_item
     then Lwt.return ()
     else (Hashtbl.replace item_table id item ;
           printf "Updated item %d\n%!" id)

let rec main () =
  Lwt_main.run
    (Updates.lwt_get () >>= fun { Updates.items=items; _ } ->
     (* note: Lwt_list.iter_p is parallel, versus Lwt_list.iter_s,
        which is sequential*)
     Lwt_list.iter_p handle_updated_item items >>= fun () ->
     Lwt_unix.sleep 1.0) ;
  main ()
  
