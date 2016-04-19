open Config_file
open Core.Std

let group = new group
let access_token_timeout_cp =
  new int_cp ~group
      ["access_token_timeout"]
      (* 60 seconds times 60 minutes times 24 hours times 30 days *)
      (60 * 60 * 24 * 30)
      "Access token timeout in seconds"
let mailgun_domain_cp =
  new string_cp ~group ["mailgun_domain"] "" "Mailgun domain"
let mailgun_api_key_cp =
  new string_cp ~group ["mailgun_api_key"] "" "Mailgun API key"
