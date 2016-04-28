open Core.Std

let t_type_tag = "api_user"

let random_string () =
  2.0 ** 28.0 |> Int.of_float |> Random.int |> Int.to_string

let random_reset_code () =
  100000 |> Random.int |> Int.to_string

type access_token = {
  value  : string;
  expiry : Core.Time.t;
}

let json_of_access_token { value; expiry; } =
  `Assoc [
     "value",  `String value;
     "expiry", `Int (expiry |> Core.Time.to_epoch |> Float.to_int);
   ]

let access_token_of_json json =
  let open Yojson.Basic.Util in
  {
    value  = member "value" json |> to_string;
    expiry = member "expiry" json |>
               to_int |> Float.of_int |> Core.Time.of_float;
  }

let is_old { expiry; _ } =
  -1 = (Core.Time.compare expiry (Core.Time.now ()))

let new_access_token () =
  {
    value = Uuid.create () |> Uuid.to_string;
    expiry = Core.Time.add (Core.Time.now ())
                           (Core.Span.of_int_sec
                              Config.access_token_timeout_cp#get);
  }

type t = {
  id            : string option;
  rev           : string option;
  email         : string;
  salt          : string;
  password_hash : string;
  reset_code    : string option;

  access_tokens : access_token list;

  (* subscriptions *)
  items         : int list;
  users         : string list;
  topics        : string list;
}

let json_of_t { id; rev;
                email; salt; password_hash; reset_code;
                access_tokens;
                items; users; topics; } =
  Couchdb.add_rev rev
    (`Assoc [
        "_id",           if id = None
                         then `Null
                         else `String (Option.value_exn id);
        "type",          `String t_type_tag;
        "email",         `String email;
        "salt",          `String salt;
        "password_hash", `String password_hash;
        "reset_code",    if reset_code = None
                         then `Null
                         else `String (Option.value_exn reset_code);
        "access_tokens", `List (List.map ~f:json_of_access_token access_tokens);
        "items",         `List (List.map ~f:(fun x -> `Int x) items);
        "users",         `List (List.map ~f:(fun x -> `String x) users);
        "topics",        `List (List.map ~f:(fun x -> `String x) topics);
      ])

let t_of_json json =
  let open Yojson.Basic.Util in
(*
  let to_list_default item =
    if item = `Null
    then []
    else to_list item in*)
  {
    id            = member "_id" json           |> to_string_option;
    rev           = member "_rev" json          |> to_string_option;
    email         = member "email" json         |> to_string;
    salt          = member "salt" json          |> to_string;
    password_hash = member "password_hash" json |> to_string;
    reset_code    = (match member "reset_code" json with
                     | `Null         -> None
                     | `String value -> Some value
                     | _ -> failwith "Bad Api_user.t json");
    access_tokens = member "access_tokens" json |> to_list |>
                      List.map ~f:access_token_of_json;
    items         = member "items" json  |> to_list |> List.map ~f:to_int;
    users         = member "users" json  |> to_list |> List.map ~f:to_string;
    topics        = member "topics" json |> to_list |> List.map ~f:to_string;
  }

let update item =
  let old_reset_code = item.reset_code in
  fun ~password ?reset_code:(reset_code = old_reset_code) () ->
  let salt = Uuid.create () |> Uuid.to_string in
  {
    item with
    salt          = salt;
    password_hash = Cryptohash_sha256.bytes (salt ^ password) |>
                      Cryptohash_sha256.to_hex;
    reset_code = reset_code;
  }

let make ~email
         ?password:(password = Uuid.create () |> Uuid.to_string)
         () =
  update {
      id            = None;
      rev           = None;
      email         = email;
      salt          = "";
      password_hash = "";
      reset_code    = None;
      access_tokens = [];
      items         = [];
      users         = [];
      topics        = [];
    }
    ~password
    ()

let is_password_correct { salt; password_hash; _ } password =
  (Cryptohash_sha256.bytes (salt ^ password) |>
     Cryptohash_sha256.to_hex) = password_hash

let add_reset_code user =
  {
    user with
    reset_code = Some (random_reset_code ())
  }

let clean_access_tokens user =
  {
    user with
    access_tokens = List.filter ~f:(fun token -> not (is_old token))
                               user.access_tokens
  }

let login user password =
  if not (is_password_correct user password) then
    failwith "Bad password"
  else
    let new_token = new_access_token () in
    ({
      (clean_access_tokens user) with
      access_tokens = new_token :: user.access_tokens
     },
     new_token)

let logout user token_val =
  {
    user with
    access_tokens = List.filter ~f:(fun { value; _ } -> value <> token_val)
                                user.access_tokens
  }

let is_valid_access_token user value =
  let { access_tokens; _ } = clean_access_tokens user in
  not (None = List.find access_tokens
                        ~f:(fun {value = other_value; _ } ->
                            value = other_value))

let () =
  let ensure_map_view = Couchdb.ensure_map_view Config.database_uri t_type_tag in
  Couchdb.append_map_view_init_function
    (fun () ->
     ignore
      (Lwt_main.run
         (ensure_map_view
            "watched_items"
            (sprintf "function(doc) { if (doc.type == \"%s\") { var idx; for (idx = 0; idx < doc.items.length; idx++) { emit(doc.items[idx], doc._id); }}}"
                     t_type_tag))) ;
    ignore
      (Lwt_main.run
         (ensure_map_view
            "watched_users"
            (sprintf "function(doc) { if (doc.type == \"%s\") { var idx; for (idx = 0; idx < doc.users.length; idx++) { emit(doc.users[idx], doc._id); }}}"
                     t_type_tag))) ;
     ignore
       (Lwt_main.run
          (ensure_map_view
             "by_email"
             (sprintf "function(doc) { if (doc.type == \"%s\") { emit(doc.email, doc._id); } }"
                      t_type_tag))) ;
     ignore
       (Lwt_main.run
          (ensure_map_view
             "all_of_type"
             (Couchdb.all_of_type_view_func t_type_tag))))

open Lwt

let hnnotify_get = Couchdb.get Config.database_uri
let hnnotify_view = Couchdb.get_raw_view_query Config.database_uri
let api_user_view = hnnotify_view t_type_tag
let by_email_view = api_user_view "by_email"

let all_by_type = api_user_view "all_of_type"

let all_t_s () =
  all_by_type ~include_docs:(`Bool true) () >|=
    Couchdb.view_results_to_string_alist_with_doc >|=
    List.map ~f:(fun (_, _, doc) -> t_of_json doc)

let get_t id =
  hnnotify_get id >|= t_of_json

let get_t_by_email_opt email =
  match_lwt (by_email_view ~include_docs:(`Bool true)
                           ~key:(`String email)
                           () >|=
               Couchdb.view_results_to_string_alist_with_doc) with
  | [_, _, json] -> Lwt.return (Some (t_of_json json))
  | [_, _, _; _] -> failwith "Multiple of email"
  | _            -> Lwt.return None

let get_id_by_email_opt email =
  match_lwt (by_email_view ~include_docs:(`Bool true)
                           ~key:(`String email)
                           () >|=
               Couchdb.view_results_to_string_alist_with_doc) with
  | [_, id, _] -> Lwt.return (Some (Yojson.Basic.Util.to_string id))
  | [_, _, _; _] -> failwith "Multiple of email"
  | _            -> Lwt.return None

let put_hnnotify = Couchdb.put Config.database_uri

let put_t item = 
  let id = if item.id = None
           then (Uuid.create () |> Uuid.to_string)
           else Option.value_exn item.id in
  let item = { item with id = Some id } in
  put_hnnotify id (json_of_t item)

(* api views of this object *)

(* debug fix this to have all the fields, we just won't make the extra ones public *)
let api_json_of_t { id; rev; email; items; users; topics; _ } =
  `Assoc [
     "id",            if id = None
                      then `Null
                      else `String (Option.value_exn id);
     "rev",           if rev = None
                      then `Null
                      else `String (Option.value_exn rev);
     "email",         `String email;
     "items",         `List (List.map ~f:(fun x -> `Int x) items);
     "users",         `List (List.map ~f:(fun x -> `String x) users);
     "topics",        `List (List.map ~f:(fun x -> `String x) topics);
   ]

let update_t_from_api_json existing json =
  let open Yojson.Basic.Util in
  let id    = member "id" json    |> to_string in
  let email = member "email" json |> to_string in
  if id <> (Option.value_exn existing.id) then failwith "ID mismatch";
  if email <> existing.email then failwith "email mismatch";
  {
    existing with
    rev     = Some (member "rev" json |> to_string);
    items   = member "items" json  |> to_list |> List.map ~f:to_int;
    users   = member "users" json  |> to_list |> List.map ~f:to_string;
    topics  = member "topics" json |> to_list |> List.map ~f:to_string;
  }

let by_watched_items_view = api_user_view "watched_items"

let user_ids_watching_item item_id =
  by_watched_items_view ~startkey:(`Int item_id) ~endkey:(`Int item_id) () >|=
    Couchdb.view_results_to_int_alist >|=
    List.map ~f:(fun (key, value) ->
                 let open Yojson.Basic.Util in
                 to_string value)

let by_watched_users_view = api_user_view "watched_users"

let user_ids_watching_user username =
  by_watched_users_view ~startkey:(`String username)
                        ~endkey:(`String username) () >|=
    Couchdb.view_results_to_string_alist >|=
    List.map ~f:(fun (key, value) ->
                 let open Yojson.Basic.Util in
                 to_string value)
