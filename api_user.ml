open Core.Std

type t = {
  username      : string;
  salt          : string;
  password_hash : string;
  reset_code    : string option;

  (* subscriptions *)
  items         : int list;
  users         : string list;
  topics        : string list;
}

let random_string () =
  2.0 ** 28.0 |> Int.of_float |> Random.int |> Int.to_string

let update item ~password () =
  let salt = random_string () in
  {
    item with
    salt          = salt;
    password_hash = Cryptohash_sha256.bytes (salt ^ password) |>
                      Cryptohash_sha256.to_hex
  }

let make ~username ~password () =
  update {
      username      = username;
      salt          = "";
      password_hash = "";
      reset_code    = None;
      items         = [];
      users         = [];
      topics        = [];
    }
    ~password
    ()

let is_password_correct { salt; password_hash; _ } password =
  (Cryptohash_sha256.bytes (salt ^ password) |>
     Cryptohash_sha256.to_hex) = password_hash
