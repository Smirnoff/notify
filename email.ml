open Printf

let default_email () =
  sprintf "Donotreply <donotreply@%s>" Config.mailgun_domain_cp#get

let quotify str =
  Yojson.Basic.to_string (`String str)

let curl_email ~from ~to_email ~subject ~text =
  sprintf "curl -s --user 'api:%s' https://api.mailgun.net/v3/%s/messages -F from=%s -F to=%s -F subject=%s -F text=%s 2>&1 >/dev/null"
          Config.mailgun_api_key_cp#get
          Config.mailgun_domain_cp#get
          (quotify from)
          (quotify to_email)
          (quotify subject)
          (quotify text)

let send ?from:(from = default_email ()) ~to_email ~subject ~text () =
  Unix.system (curl_email ~from ~to_email ~subject ~text)
