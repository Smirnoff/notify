open Core.Std

type t = {
  items    : int list;
  profiles : string list;
}

let json_of_t { items; profiles; } =
  `Assoc [
     "items",    `List (List.map ~f:(fun x -> `Int x)    items);
     "profiles", `List (List.map ~f:(fun x -> `String x) profiles);
   ]

let t_of_json json =
  let open Yojson.Basic.Util in
  {
    items    = member "items"    json |> to_list |> List.map ~f:to_int;
    profiles = member "profiles" json |> to_list |> List.map ~f:to_string;
  }

let uri () =
  Uri.of_string (Hn_misc.base_versioned_url ^ "/updates.json")

(* network stuff *)

open Lwt
open Cohttp
open Cohttp_lwt_unix

let lwt_get () =
  uri () |> Hn_misc.lwt_get_json_uri >|= t_of_json
