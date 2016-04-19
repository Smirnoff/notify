open Config_file
open Core.Std

let group = new group
let access_token_timeout_cp =
  new int_cp ~group
      ["access_token_timeout"]
      (* 60 seconds times 60 minutes times 24 hours times 30 days *)
      (60 * 60 * 24 * 30)
      "Access token timeout in seconds"

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
  group#read "config" ;
  {
    value = random_string ();
    expiry = Core.Time.add (Core.Time.now ())
                           (Core.Span.of_int_sec access_token_timeout_cp#get);
  }

type t = {
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

let json_of_t { email; salt; password_hash; reset_code;
                access_tokens;
                items; users; topics; } =
  `Assoc [
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
   ]

let t_of_json json =
  let open Yojson.Basic.Util in
  {
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
  let salt = random_string () in
  {
    item with
    salt          = salt;
    password_hash = Cryptohash_sha256.bytes (salt ^ password) |>
                      Cryptohash_sha256.to_hex;
    reset_code = reset_code;
  }

let make ~email
         ?password:(password = random_string ())
         () =
  update {
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
