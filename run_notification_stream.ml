Api_main.main_init () ;
Lwt_main.run (Notification_stream.thread_run ())
