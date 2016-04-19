open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

let object_uri db_uri id =
  Uri.of_string ((Uri.to_string db_uri) ^ "/" ^ id)

let get_opt db_uri id =
  Client.get (object_uri db_uri id) >>= fun (resp, body) ->
  if Response.status resp = `OK
  then body |> Cohttp_lwt_body.to_string >|=
         (fun body -> Some (Yojson.Basic.from_string body))
  else Lwt.return None

let get db_uri id =
  get_opt db_uri id >>= fun res ->
    match res with
    | None       -> failwith "Couldn't get item"
    | Some value -> Lwt.return res

let put db_uri id json =
  Client.put ~body:(Yojson.Basic.to_string json |> Cohttp_lwt_body.of_string)
             (object_uri db_uri id) >>= fun (resp, body) ->
  if Response.status resp = `Created
  then body |> Cohttp_lwt_body.to_string >|=
         Yojson.Basic.from_string
  else failwith "Failed to put item"

let json_delete target json =
  match json with
  | `Assoc entries ->
     `Assoc (List.filter entries ~f:(fun (key, value) -> key <> target))
  | _ -> failwith "Not a json object"

let empty_view_doc id_suffix =
  `Assoc ["_id",   `String ("_design/" ^ id_suffix);
          "views", `Assoc []]

let update_view_doc doc view map_func =
  match doc with
  | `Assoc entries ->
     `Assoc (List.map
               entries
               ~f:(fun (key, json) ->
                   if key <> "views"
                   then (key, json)
                   else match json_delete view json with
                        | `Assoc entries ->
                           (key, `Assoc
                                  ((view, `Assoc ["map", `String map_func]) ::
                                     entries))))
  | _ -> failwith "bad document"

let ensure_map_view db_uri id_suffix view map_func =
  let id = "_design/" ^ id_suffix in
  match_lwt get_opt db_uri id with
  | None ->
     put db_uri id
         (update_view_doc (empty_view_doc id_suffix) view map_func)
  | Some existing ->
     put db_uri id
         (update_view_doc existing view map_func)
