open Lwt
open Core.Std

type t = {
  id        : string;
  delay     : int;
  created   : Core.Time.t;
  karma     : int;
  about     : string;
  submitted : int list;
}

let json_of_t { id; delay; created; karma; about; submitted; } =
  `Assoc [
     "id",        `String id;
     "delay",     `Int    delay;
     "created",   `Int    (created |> Core.Time.to_epoch |> Float.to_int);
     "karma",     `Int    karma;
     "about",     `String about;
     "submitted", `List   (List.map ~f:(fun x -> `Int x) submitted)
   ]

let t_of_json json =
  let open Yojson.Basic.Util in
  {
    id        = member "id"        json |> to_string;
    delay     = member "delay"     json |> to_int;
    created   = member "created"   json |>
                  to_int |>
                  Float.of_int |>
                  Core.Time.of_float;
    karma     = member "karma"     json |> to_int;
    about     = member "about"     json |> to_string;
    submitted = member "submitted" json |> to_list |> List.map ~f:to_int;
  }

let id = function
  | { id; _ } -> id

let user_base_url = Hn_misc.base_versioned_url ^ "/user"

let uri_from_id username =
  Uri.of_string (Format.sprintf "%s/%s.json" user_base_url username)

let uri item = id item |> uri_from_id

let lwt_get_by_id id =
  uri_from_id id |> Hn_misc.lwt_get_json_uri >|= fun json ->
  try
    t_of_json json
  with
    _ -> failwith (sprintf "Failed to convert user #%s" id)
