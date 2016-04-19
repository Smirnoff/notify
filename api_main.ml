open Lwt
open Cohttp
open Cohttp_lwt_unix

open Core.Std

(* just for starting out, this is email -> Api_user.t *)
let api_users = String.Table.create ()

(* path -> callback *)
let api_calls = String.Table.create ()

let fail_with_bad_call () =
  Server.respond_error ~status:`I_m_a_teapot
                       ~body:"No such call\n" ();;

let api_user_by_email email =
  Hashtbl.find_or_add api_users email
                      ~default:(fun () -> Api_user.make ~email:email ())

let issue_reset_code_callback conn req body =
  let open Yojson.Basic.Util in
  match Request.meth req with
  | `POST ->
     body |> Cohttp_lwt_body.to_string >|= (fun body ->
     let json = Yojson.Basic.from_string body in
     let email = member "email" json |> to_string in
     let user = Hashtbl.find_or_add
                  api_users email
                  ~default:(fun () -> Api_user.make ~email:email ()) in
     let user = Api_user.add_reset_code user in
     Hashtbl.replace api_users ~key:email ~data:user ;
     ignore
       (Email.send ~to_email:email
                   ~subject:"Password reset code"
                   ~text:(sprintf
                            "Here is your password reset code: %s"
                            (Option.value_exn user.Api_user.reset_code))
                   ())) >>= (fun () ->
     Server.respond_string
       ~status:`OK
       ~body:(Yojson.Basic.to_string
                (`Assoc ["message", `String "Reset code issued"]))
       ())
  | _     -> fail_with_bad_call () ;;

let apply_reset_code_callback conn req body =
  let open Yojson.Basic.Util in
  match Request.meth req with
  | `POST ->
     body |> Cohttp_lwt_body.to_string >|=
       Yojson.Basic.from_string >>= (fun json ->
       let email = member "email" json |> to_string in
       let reset_code = member "reset-code" json |> to_string in
       let password = member "password" json |> to_string in
       let user = Hashtbl.find_or_add
                    api_users email
                    ~default:(fun () -> Api_user.make ~email:email ()) in
       if Some reset_code = user.Api_user.reset_code then
         (Hashtbl.replace api_users
                          ~key:email
                          ~data:(Api_user.update user
                                                 ~password ~reset_code:None
                                                 ()) ;
          Server.respond_string
            ~status:`OK
            ~body:(Yojson.Basic.to_string
                     (`Assoc ["message", `String "Password reset"])) ())
       else
         Server.respond_error ~status:`I_m_a_teapot
                              ~body:"{\"message\": \"Incorrect reset code\"}"
                              ())
  | _ -> fail_with_bad_call ();;

let login_callback conn req body =
  let open Yojson.Basic.Util in
  match Request.meth req with
  | `POST ->
     body |> Cohttp_lwt_body.to_string >|=
       Yojson.Basic.from_string >>= (fun json ->
       let email = member "email" json |> to_string in
       let password = member "password" json |> to_string in
       let user = Hashtbl.find_or_add
                    api_users email
                    ~default:(fun () -> Api_user.make ~email:email ()) in
       if Api_user.is_password_correct user password then
         let (new_user, new_token) = Api_user.login user password in
         Hashtbl.replace api_users ~key:email ~data:new_user ;
         Server.respond_string
           ~status:`OK
           ~body:(Yojson.Basic.to_string
                    (`Assoc ["message", `String "Logged in successfully";
                             "access-token", `String new_token.Api_user.value]))
           ()
       else
         Server.respond_error ~status:`I_m_a_teapot
                              ~body:"{\"message\": \"Incorrect password\"}"
                              ())
  | _ -> fail_with_bad_call ();;

let current_user_with_token conn req body =
  let query = req |> Request.uri |> Uri.query in
  match (List.Assoc.find query "email",
         List.Assoc.find query "access-token") with
  | Some [email], Some [token] ->
     let user = api_user_by_email email in
     if Api_user.is_valid_access_token user token
     then Some (user, token)
     else None
  | _ -> None

let current_user conn req body =
  match current_user_with_token conn req body with
  | Some (user, token) -> Some user
  | None -> None

let is_logged_in_callback conn req body =
  match Request.meth req with
  | `GET ->
     (match current_user conn req body with
      | None -> Server.respond_string
                  ~status:`OK
                  ~body:(Yojson.Basic.to_string (`Bool false)) ()
      | Some _ -> Server.respond_string
                    ~status:`OK
                    ~body:(Yojson.Basic.to_string (`Bool true)) ())
  | _ -> fail_with_bad_call ();;

let logout_callback conn req body =
  match Request.meth req with
  | `POST ->
     (match current_user_with_token conn req body with
      | None -> Server.respond_error
                  ~status:`I_m_a_teapot
                  ~body:(Yojson.Basic.to_string
                           (`Assoc ["message", `String "Not logged in"]))
                  ()
      | Some (user, token) ->
         Hashtbl.replace api_users ~key:user.Api_user.email
                                   ~data:(Api_user.logout user token) ;
         Server.respond_string
           ~status:`OK
           ~body: (Yojson.Basic.to_string
                     (`Assoc ["message", `String "logged out"]))
           ())
  | _ -> fail_with_bad_call ();;

let server =
  let callback conn req body =
    let uri = req |> Request.uri in
    match Hashtbl.find api_calls (Uri.path uri) with
    | None      -> fail_with_bad_call ()
    | Some func -> func conn req body in
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let main () =
  Unix.time () |> Float.to_int |> Random.init ;
  ignore(
      Hashtbl.add api_calls
                  ~key:"/issue-reset-code"
                  ~data:issue_reset_code_callback) ;
  ignore(
      Hashtbl.add api_calls
                  ~key:"/apply-reset-code"
                  ~data:apply_reset_code_callback) ;
  ignore(
      Hashtbl.add api_calls
                  ~key:"/login"
                  ~data:login_callback) ;
  ignore(
      Hashtbl.add api_calls
                  ~key:"/is-logged-in"
                  ~data:is_logged_in_callback) ;
  ignore(
      Hashtbl.add api_calls
                  ~key:"/logout"
                  ~data:logout_callback) ;
  ignore (Lwt_main.run server)
