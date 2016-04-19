open Config_file
open Printf

let group = new group
let mailgun_domain_cp =
  new string_cp ~group ["mailgun_domain"] "" "Mailgun domain"
let mailgun_api_key_cp =
  new string_cp ~group ["mailgun_api_key"] "" "Mailgun API key"

let default_email () =
  group#read "config" ;
  sprintf "Donotreply <donotreply@%s>" mailgun_domain_cp#get

let quotify str =
  Yojson.Basic.to_string (`String str)

let curl_email ~from ~to_email ~subject ~text =
  group#read "config" ;
  sprintf "curl -s --user 'api:%s' https://api.mailgun.net/v3/%s/messages -F from=%s -F to=%s -F subject=%s -F text=%s 2>&1 >/dev/null"
          mailgun_api_key_cp#get
          mailgun_domain_cp#get
          (quotify from)
          (quotify to_email)
          (quotify subject)
          (quotify text)

let send ?from:(from = default_email ()) ~to_email ~subject ~text () =
  Unix.system (curl_email ~from ~to_email ~subject ~text)
