open Core.Std

let some_early_time () =
  Core.Time.of_float 0.

let some_future_time () =
  Core.Time.add (Core.Time.now ()) (Core.Span.of_day 500.)

let time_to_int time =
  time |> Core.Time.to_epoch |> Float.to_int
