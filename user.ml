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
