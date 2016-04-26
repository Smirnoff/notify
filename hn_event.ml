open Core.Std

let t_type_tag = "event"

type 'id_type change = {
  id           : string option;
  rev          : string option;
  hn_id        : 'id_type;
  api_notified : string list;
  fields       : Yojson.Basic.json;
  time         : Core.Time.t;
}

let json_of_change type_tag id_to_json { id; rev; hn_id; api_notified; fields; time; } =
  `Assoc [
     "type",         `String t_type_tag;
     "sub_type",     `String type_tag;
     "hn_id",        id_to_json hn_id;
     "api_notified", `List (List.map ~f:(fun x -> `String x) api_notified);
     "fields",       fields;
     "time",         `Int (time |> Core.Time.to_epoch |> Float.to_int);
   ] |>
    Couchdb.add_id_rev ~id ~rev

let public_json_of_change type_tag id_to_json { id; rev; hn_id; api_notified; fields; time; } =
  `Assoc [
     "type",         `String t_type_tag;
     "sub_type",     `String type_tag;
     "hn_id",        id_to_json hn_id;
     "fields",       fields;
     "time",         `Int (time |> Core.Time.to_epoch |> Float.to_int);
   ] |>
    Couchdb.add_id_rev ~id ~rev

let change_of_json json_to_id json =
  let open Yojson.Basic.Util in
  {
    id           = member "id" json           |> to_string_option;
    rev          = member "id" json           |> to_string_option;
    hn_id        = member "hn_id" json        |> json_to_id;
    api_notified = member "api_notified" json |> to_list |> List.map ~f:to_string;
    fields       = member "fields" json;
    time         = member "time" json |> to_int |> Float.of_int |> Core.Time.of_float;
  }

type profile_id = string
type item_id = int

let profile_id_of_json = Yojson.Basic.Util.to_string
let item_id_of_json = Yojson.Basic.Util.to_int

let json_of_profile_id x = `String x
let json_of_item_id x = `Int x

type profile_change = profile_id change
type item_change    = item_id change

let item_change_tag = "item_change"
let profile_change_tag = "profile_change"

let json_of_profile_change = json_of_change profile_change_tag json_of_profile_id
let json_of_item_change = json_of_change item_change_tag json_of_item_id

let public_json_of_profile_change = public_json_of_change profile_change_tag json_of_profile_id
let public_json_of_item_change = public_json_of_change item_change_tag json_of_item_id

let profile_change_of_json = change_of_json profile_id_of_json
let item_change_of_json = change_of_json item_id_of_json

type t =
  | ItemChange    of item_change
  | ProfileChange of profile_change

let t_of_json json =
  let open Yojson.Basic.Util in
  let type_tag = member "sub_type" json |> to_string in
  if type_tag = item_change_tag
  then ItemChange (item_change_of_json json)
  else if type_tag = profile_change_tag
  then ProfileChange (profile_change_of_json json)
  else failwith "Unknown type tag"

let json_of_t = function
  | ItemChange change -> json_of_item_change change
  | ProfileChange change -> json_of_profile_change change

let public_json_of_t = function
  | ItemChange change -> public_json_of_item_change change
  | ProfileChange change -> public_json_of_profile_change change

open Lwt

let hnnotify_get = Couchdb.get Config.database_uri
let hnnotify_get_opt = Couchdb.get_opt Config.database_uri

let get_t id =
  hnnotify_get id >|= t_of_json

let get_t_opt id =
  match_lwt hnnotify_get_opt id with
  | None -> Lwt.return None
  | Some item -> Lwt.return (Some (t_of_json item))

let put_hnnotify = Couchdb.put Config.database_uri

let get_id = function
  | ItemChange { id; _ } -> id
  | ProfileChange { id; _ } -> id

let update_with_id id = function
  | ItemChange obj -> ItemChange { obj with id = Some id }
  | ProfileChange obj -> ProfileChange { obj with id = Some id }

let put_t item =
  let id = if (get_id item) = None
           then (Uuid.create () |> Uuid.to_string)
           else Option.value_exn (get_id item) in
  let item = update_with_id id item in
  put_hnnotify id (json_of_t item)

(* views *)

let ensure_map_view = Couchdb.ensure_map_view Config.database_uri t_type_tag

let () =
  Couchdb.append_map_view_init_function
    (fun () ->
     (* Ok, so
        we're going to add a way to search for all event.t's that
        are aimed at a particular user
      *)
     ignore
       (Lwt_main.run
          (ensure_map_view
             "time_and_notified"
             (sprintf "function(doc) { if (doc.type == \"%s\") { var idx; for (idx = 0; idx < doc.api_notified.length; idx++) { emit([doc.api_notified[idx], doc.time], null); } } }"
                      t_type_tag))))

let hnnotify_view = Couchdb.get_raw_view_query Config.database_uri
let api_user_view = hnnotify_view t_type_tag
let by_notified_events = api_user_view "time_and_notified"

let t_s_by_notified_id id =
  by_notified_events
    ~include_docs:(`Bool true)
    ~startkey:(`List [`String id;
                      `Int (Misc_util.some_early_time () |>
                              Misc_util.time_to_int)])
    ~endkey:(`List [`String id;
                    `Int (Misc_util.some_future_time () |>
                            Misc_util.time_to_int)])
    () >|=
    Couchdb.view_results_to_X_alist_with_doc (fun x -> x) >|=
    List.map ~f:(fun (_, _, doc) -> t_of_json doc)

let make_change ~hn_id ~api_notified ?fields:(fields=`Assoc []) () =
  match hn_id with
  | `String value ->
     ProfileChange {
         id = None;
         rev = None;
         hn_id = value;
         api_notified = api_notified;
         fields = fields;
         time = Core.Time.now ();
       }
  | `Int value ->
     ItemChange {
         id = None;
         rev = None;
         hn_id = value;
         api_notified = api_notified;
         fields = fields;
         time = Core.Time.now ();
       }
  | _ -> failwith "Bad HN id"

let list_delete item lst =
  List.filter ~f:(fun x -> x <> item)
              lst

let remove_api_notified id = function
  | ItemChange item ->
     ItemChange { item with api_notified = list_delete id item.api_notified }
  | ProfileChange item ->
     ProfileChange { item with api_notified = list_delete id item.api_notified }
