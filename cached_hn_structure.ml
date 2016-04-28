open Core.Std

let t_type_tag = "cached_hn_structure"

type t = {
  id              : string option;
  rev             : string option;
  hacker_news_key : Yojson.Basic.json;
  last_instance   : Yojson.Basic.json;
  hash            : string;
}

let make ~hacker_news_key ~last_instance ~hash =
  {
    id = None;
    rev = None;
    hacker_news_key = hacker_news_key;
    last_instance = last_instance;
    hash = hash;
  }

let update existing ~last_instance ~hash =
  {
    existing with
    last_instance = last_instance;
    hash = hash;
  }

let json_of_t { id; rev; hacker_news_key; last_instance; hash; } =
  Couchdb.add_rev rev
    (`Assoc
      [
        "_id",             if id = None
                           then `Null
                           else `String (Option.value_exn id);
        "type",            `String t_type_tag;
        "hacker_news_key", hacker_news_key;
        "last_instance",   last_instance;
        "hash",            `String hash
      ])

let t_of_json json =
  let open Yojson.Basic.Util in
  {
    id              = member "_id" json             |> to_string_option;
    rev             = member "_rev" json            |> to_string_option;
    hacker_news_key = member "hacker_news_key" json;
    last_instance   = member "last_instance" json;
    hash            = member "hash" json            |> to_string;
  }

let () =
  let ensure_map_view = Couchdb.ensure_map_view Config.database_uri t_type_tag in
  Couchdb.append_map_view_init_function
    (fun () ->
     ignore
       (Lwt_main.run
          (ensure_map_view
             "by_hacker_news_key"
             (sprintf "function(doc) { if (doc.type == \"%s\") { emit(doc.hacker_news_key, null); }}"
                      t_type_tag))) ;
     ignore
       (Lwt_main.run
          (ensure_map_view
             "all_of_type"
             (Couchdb.all_of_type_view_func t_type_tag))))

open Lwt

let hnnotify_view = Couchdb.get_raw_view_query Config.database_uri
let structure_view = hnnotify_view t_type_tag
let by_hacker_news_key_view = structure_view "by_hacker_news_key"

let all_by_type = structure_view "all_of_type"

let all_t_s () =
  all_by_type ~include_docs:(`Bool true) () >|=
    Couchdb.view_results_to_string_alist_with_doc >|=
    List.map ~f:(fun (_, _, doc) -> t_of_json doc)

let get_by_hacker_news_key_opt key =
  match_lwt (by_hacker_news_key_view
               ~include_docs:(`Bool true)
               ~key
               () >|=
               Couchdb.view_results_to_X_alist_with_doc (fun x -> x)) with
  | [_, _, json] -> Lwt.return (Some (t_of_json json))
  | (_, _, _) :: _ -> failwith "Multiple of hacker news key"
  | _            -> Lwt.return None

let put_hnnotify = Couchdb.put Config.database_uri

let put_t item =
  let id = if item.id = None
           then (Uuid.create () |> Uuid.to_string)
           else Option.value_exn item.id in
  let item = { item with id = Some id } in
  put_hnnotify id (json_of_t item)

let uri = function
  | `String str   ->
     Uri.of_string (sprintf "%s/user/%s.json" Hn_misc.base_versioned_url str)
  | `Int    value ->
     Uri.of_string (sprintf "%s/item/%d.json" Hn_misc.base_versioned_url value)
  | _ -> failwith "Bad id"

let get_json key =
  uri key |> Hn_misc.lwt_get_json_uri

let process_new_json key json =
  let hash = Hn_misc.hash_json json in
  match_lwt get_by_hacker_news_key_opt key with
  | None ->
     let item = make ~hacker_news_key:key ~last_instance:json ~hash in
     Lwt.return (None, item)
  | Some old_item ->
     Lwt.return (Some old_item, update old_item ~last_instance:json ~hash)

let change_event ~hn_id ~api_notified before after =
  let open Yojson.Basic.Util in
  let before_assoc = to_assoc before in
  let _ = to_assoc after in
  Hn_event.make_change
    ~hn_id
    ~api_notified
    ~fields:(`Assoc (List.filter_map
                       before_assoc
                       ~f:(fun (key, before_val) ->
                           let after_val = member key after in
                           if after_val = before_val
                           then None
                           else Some (key, `List [before_val; after_val]))))
    ()
