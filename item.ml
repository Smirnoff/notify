open Core.Std

type job = {
  id    : int;
  by    : string;
  time  : Core.Time.t;
  text  : string;
  title : string;
  score : int;
  url   : string;
}

let json_of_job { id; by; time; text; title; score; url; } =
  `Assoc [
     "id",    `Int    id;
     "by",    `String by;
     "time",  `Int    (time |> Core.Time.to_epoch |> Float.to_int);
     "text",  `String text;
     "title", `String title;
     "score", `Int    score;
     "url",   `String url;
   ]

let job_of_json json =
  let open Yojson.Basic.Util in
  {
    id    = member "id"    json |> to_int;
    by    = member "by"    json |> to_string;
    time  = member "time"  json |> to_int |> Float.of_int |> Core.Time.of_float;
    text  = member "text"  json |> to_string;
    title = member "title" json |> to_string;
    score = member "score" json |> to_int;
    url   = member "url"   json |> to_string;
  }

type story = {
  id          : int;
  by          : string;
  time        : Core.Time.t;
  title       : string;
  score       : int;
  url         : string;
  kids        : int list;
  descendants : int;
}

let json_of_story { id; by; time; title; score; url; kids; descendants; } =
  `Assoc [
     "id",          `Int    id;
     "by",          `String by;
     "time",        `Int    (time |> Core.Time.to_epoch |> Float.to_int);
     "title",       `String title;
     "score",       `Int    score;
     "url",         `String url;
     "kids",        `List   (List.map ~f:(fun x -> `Int x) kids);
     "descendants", `Int    descendants;
   ]

let story_of_json json =
  let open Yojson.Basic.Util in
  {
    id    = member "id"    json |> to_int;
    by    = member "by"    json |> to_string;
    time  = member "time"  json |> to_int |> Float.of_int |> Core.Time.of_float;
    title = member "title" json |> to_string;
    score = member "score" json |> to_int;
    url   = member "url"   json |> to_string;
    kids  = member "kids"  json |> to_list |> List.map ~f:to_int;
    descendants = member "descendants" json |> to_int;
  }

type comment = {
  id     : int;
  by     : string;
  time   : Core.Time.t;
  text   : string;
  parent : int;
  kids   : int list;
}

let json_of_comment { id; by; time; text; parent; kids; } =
  `Assoc [
     "id",     `Int    id;
     "by",     `String by;
     "time",   `Int    (time |> Core.Time.to_epoch |> Float.to_int);
     "text",   `String text;
     "parent", `Int    parent;
     "kids",   `List   (List.map ~f:(fun x -> `Int x) kids);
   ]

let comment_of_json json =
  let open Yojson.Basic.Util in
  {
    id     = member "id"     json |> to_int;
    by     = member "by"     json |> to_string;
    time   = member "time"   json |> to_int |> Float.of_int |> Core.Time.of_float;
    text   = member "text"   json |> to_string;
    kids   = member "kids"   json |> to_list |> List.map ~f:to_int;
    parent = member "parent" json |> to_int;
  }

type poll = {
  id          : int;
  by          : string;
  time        : Core.Time.t;
  text        : string;
  title       : string;
  score       : int;
  parts       : int list;
  kids        : int list;
  descendants : int;
}

let json_of_poll { id; by; time; text; title; score;
                   parts; kids; descendants; } =
  `Assoc [
     "id",           `Int    id;
     "by",           `String by;
     "time",         `Int    (time |> Core.Time.to_epoch |> Float.to_int);
     "text",         `String text;
     "title",        `String title;
     "score",        `Int    score;
     "kids",         `List   (List.map ~f:(fun x -> `Int x) kids);
     "parts",        `List   (List.map ~f:(fun x -> `Int x) parts);
     "descendants",  `Int    descendants;
   ]

let poll_of_json json =
  let open Yojson.Basic.Util in
  {
    id          = member "id"          json |> to_int;
    by          = member "by"          json |> to_string;
    time        = member "time"        json |>
                    to_int |>
                    Float.of_int |>
                    Core.Time.of_float;
    text        = member "text"        json |> to_string;
    title       = member "title"       json |> to_string;
    score       = member "score"       json |> to_int;
    kids        = member "kids"        json |> to_list |> List.map ~f:to_int;
    parts       = member "parts"       json |> to_list |> List.map ~f:to_int;
    descendants = member "descendants" json |> to_int;
  }

type pollopt = {
  id     : int;
  by     : string;
  time   : Core.Time.t;
  text   : string;
  score  : int;
  parent : int;
}

let json_of_pollopt { id; by; time; text; score; parent; } =
  `Assoc [
     "id",     `Int    id;
     "by",     `String by;
     "time",   `Int    (time |> Core.Time.to_epoch |> Float.to_int);
     "text",   `String text;
     "score",  `Int    score;
     "parent", `Int    parent;
   ]

let pollopt_of_json json =
  let open Yojson.Basic.Util in
  {
    id     = member "id"     json |> to_int;
    by     = member "by"     json |> to_string;
    time   = member "time"   json |>
               to_int |>
               Float.of_int |>
               Core.Time.of_float;
    text   = member "text"   json |> to_string;
    score  = member "score"       json |> to_int;
    parent = member "parent" json |> to_int;
  }

type t =
  | JobItem     of job
  | StoryItem   of story
  | CommentItem of comment
  | PollItem    of poll
  | PolloptItem of pollopt

let add_type_tag type_name lst =
  `Assoc (("type", `String type_name) :: lst)

let json_of_t =
  let open Yojson.Basic.Util in
  function
  | JobItem     item -> json_of_job     item |> to_assoc |> add_type_tag "job"
  | StoryItem   item -> json_of_story   item |> to_assoc |> add_type_tag "story"
  | CommentItem item -> json_of_comment item |> to_assoc |> add_type_tag "comment"
  | PollItem    item -> json_of_poll    item |> to_assoc |> add_type_tag "poll"
  | PolloptItem item -> json_of_pollopt item |> to_assoc |> add_type_tag "pollopt"

let t_of_json_opt json =
  let open Yojson.Basic.Util in
  match member "type" json with
  | `String "job"     -> Some (JobItem     (job_of_json     json))
  | `String "story"   -> Some (StoryItem   (story_of_json   json))
  | `String "comment" -> Some (CommentItem (comment_of_json json))
  | `String "poll"    -> Some (PollItem    (poll_of_json    json))
  | `String "pollopt" -> Some (PolloptItem (pollopt_of_json json))
  | _                 -> None

let t_of_json json =
  match t_of_json_opt json with
  | None     -> failwith "Invalid item"
  | Some res -> res
