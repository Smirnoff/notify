open Core.Std

(* ok, so

what's happening:

* sometimes an item changes and we have no previous record of it
  we create a new Cached_item.t and notify everyone watching it
  (which should honestly be no one)
* if an item changes and we do have a record of it,
  we take the hash of it, compare it to our existing hash
  if it's changed, we compute the differences and notify everyone

*)

type 'key_type t = {
  id              : string option;
  rev             : string option;
  hacker_news_key : 'key_type;
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

let general_json_of_t key_conv t_tag
                      { id; rev; hacker_news_key; last_instance; hash; } =
  Couchdb.add_rev rev
    (`Assoc
      [
        "_id",             if id = None
                           then `Null
                           else `String (Option.value_exn id);
        "type",            `String t_tag;
        "hacker_news_key", key_conv hacker_news_key;
        "last_instance",   last_instance;
        "hash",            `String hash
      ])

let general_t_of_json key_conv json =
  let open Yojson.Basic.Util in
  {
    id              = member "_id" json             |> to_string_option;
    rev             = member "_rev" json            |> to_string_option;
    hacker_news_key = member "hacker_news_key" json |> key_conv;
    last_instance   = member "last_instance" json;
    hash            = member "hash" json            |> to_string;
  }

let add_by_hacker_news_key_view t_tag =
  let ensure_map_view = Couchdb.ensure_map_view Config.database_uri t_tag in
  Couchdb.append_map_view_init_function (fun () ->
    ignore
      (Lwt_main.run
         (ensure_map_view
            "by_hacker_news_key"
            (sprintf "function(doc) { if (doc.type == \"%s\") { emit(doc.hacker_news_key, null); }}"
                     t_tag))))

open Lwt

let hnnotify_view = Couchdb.get_raw_view_query Config.database_uri

let get_by_hacker_news_key_opt key_conv alist_conv
                               by_hacker_news_key_view t_of_json key =
  match_lwt (by_hacker_news_key_view
               ~include_docs:(`Bool true)
               ~key:(key_conv key)
               () >|=
               alist_conv) with
  | [_, _, json] -> Lwt.return (Some (t_of_json json))
  | (_, _, _) :: _ -> failwith "Multiple of hacker news key"
  | _            -> Lwt.return None

let put_hnnotify = Couchdb.put Config.database_uri

let put_t_func json_of_t item =
  let id = if item.id = None
           then (Uuid.create () |> Uuid.to_string)
           else Option.value_exn item.id in
  let item = { item with id = Some id } in
  put_hnnotify id (json_of_t item)

type change_key =
  | ItemKey of int
  | UserKey of string

module Item = struct
  type item_t = int t
  type t = item_t

  let key_conv int = `Int int

  let t_tag = "cached_hn_item"

  let t_of_json = general_t_of_json Yojson.Basic.Util.to_int
  let json_of_t = general_json_of_t key_conv t_tag

  let add_map_views () =
    add_by_hacker_news_key_view t_tag

  let cached_view = hnnotify_view t_tag
  let by_hacker_news_key_view = cached_view "by_hacker_news_key"

  let get_t_by_hacker_news_key_opt =
    let by_hacker_news_key_view ~include_docs ~key () =
      by_hacker_news_key_view ~include_docs ~key () in
    get_by_hacker_news_key_opt key_conv
                               Couchdb.view_results_to_int_alist_with_doc
                               by_hacker_news_key_view t_of_json

  let get_json key =
    Item.lwt_get_by_id key >|= Item.json_of_t

  let put_t = put_t_func json_of_t

  let process_new_json key json =
    let hash = Hn_misc.hash_json json in
    match_lwt get_t_by_hacker_news_key_opt key with
    | None ->
       let item = make ~hacker_news_key:key ~last_instance:json ~hash in
       Lwt.return (None, item)
    | Some old_item ->
       Lwt.return (Some old_item, update old_item ~last_instance:json ~hash)

  let change_key_of_key key = ItemKey key
end

module User = struct
  type user_t = string t
  type t = user_t

  let key_conv str = `String str

  let t_of_json = general_t_of_json Yojson.Basic.Util.to_string
  let json_of_t = general_json_of_t key_conv

  let t_tag = "cached_hn_user"

  let add_map_views () =
    add_by_hacker_news_key_view t_tag

  let cached_view = hnnotify_view t_tag
  let by_hacker_news_key_view = cached_view "by_hacker_news_key"

  let get_t_by_hacker_news_key_opt =
    let by_hacker_news_key_view ~include_docs ~key () =
      by_hacker_news_key_view ~include_docs ~key () in
    get_by_hacker_news_key_opt key_conv
                               Couchdb.view_results_to_string_alist_with_doc
                               by_hacker_news_key_view t_of_json

  let change_key_of_key key = UserKey key
end

let add_map_views () =
  Item.add_map_views () ;
  User.add_map_views ()

let change_key_of_json = function
  | `String key -> UserKey key
  | `Int    key -> ItemKey key
  | _           -> failwith "bad change key"

let json_of_change_key = function
  | ItemKey key -> `Int key
  | UserKey key -> `String key

type change = {
  id     : string;
  key    : change_key;
  fields : (string * (Yojson.Basic.json * Yojson.Basic.json)) list;
}

let change_of_json json =
  let open Yojson.Basic.Util in
  {
    id     = member "id" json     |> to_string;
    key    = member "key" json    |> change_key_of_json;
    fields = member "fields" json |>
               to_list |>
               List.map
                 ~f:(fun json ->
                     let key = index 0 json |> to_string in
                     let value = index 1 json in
                     let before = index 0 value in
                     let after = index 1 value in
                     (key, (before, after)));
  }

let change_json_type { key; _ } =
  match key with
  | ItemKey _ -> "item"
  | UserKey _ -> "user"

let json_of_change { id; key; fields; } =
  `Assoc [
     "type", `String (change_json_type { id; key; fields; });
     "id", `String id;
     "key", json_of_change_key key;
     "fields", `List (List.map ~f:(fun (key, (before, after)) ->
                                   `List [`String key;
                                          `List [before; after]])
                               fields);
   ]

let delta_with_key before after key =
  let open Yojson.Basic.Util in
  let before_assoc = to_assoc before in
  let _ = to_assoc after in
  {
    id = Uuid.create () |> Uuid.to_string;
    key = key;
    fields = List.filter_map
               before_assoc
               ~f:(fun (key, before_val) ->
                   let after_val = member key after in
                   if after_val = before_val
                   then None
                   else Some (key, (before_val, after_val)))
  }
