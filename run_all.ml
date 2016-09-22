Api_main.main_init();
ignore (Lwt_main.run (Lwt.join [Api_main.server (); Monitor.thread_run (); Notification_stream.thread_run()]))
