open Core.Std
Config.group#read "config" ;
Unix.time () |> Float.to_int |> Random.init ;
Lwt_main.run (Notification_stream.thread_run ())
