type error =
  | NotFound of string
  | InvalidInput of string
  | NetworkError of string

let fetch_data url =
  if url = "" then Error (InvalidInput "dang, that input was invalid")
  else if url = "notfound" then Error (NotFound "ugh, resource not found")
  else if url = "network" then Error (NetworkError "👎 network failed")
  else Ok "hey it all worked!"

let handle_error = function
  | NotFound desc -> Printf.printf "PVE Error :: %s\n" desc
  | InvalidInput reason -> Printf.printf "PVE Error :: %s\n" reason
  | NetworkError msg -> Printf.printf "PVE Error :: %s\n" msg

let () =
  match fetch_data "notfound" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_data "network" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_data "" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_data "https://jmsdnns.com" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err
