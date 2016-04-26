open Printf

let () =
  match Sys.argv with
  | [|_; filename|] -> Config.group#write filename
  | _ -> printf "%s: bad arguments, missing filename\n" Sys.argv.(0)

