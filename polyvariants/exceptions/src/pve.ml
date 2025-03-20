type base_error = [ `NotFound of string | `InvalidInput of string ]

let fetch_data url =
  match url with
  | "" -> Error (`InvalidInput "Invalid Input Error")
  | "notfound" -> Error (`NotFound "Not Found Error")
  | _ -> Ok "Data fetched successfully"

type extended_error =
  [ base_error | `NetworkError of string | `TimeoutError of string ]

let fetch_with_extended_error url =
  match url with
  | "network" -> Error (`NetworkError "Network Error")
  | "timeout" -> Error (`TimeoutError "Timed out Error")
  | _ -> (
      Printf.printf "[PVE] falling back to fetch_data\n";
      match fetch_data url with Ok msg -> Ok msg | Error e -> Error e)

let handle_error = function
  | `NotFound desc -> Printf.printf "PVE Error :: %s\n" desc
  | `InvalidInput reason -> Printf.printf "PVE Error :: %s\n" reason
  | `NetworkError msg -> Printf.printf "PVE Error :: %s\n" msg
  | `TimeoutError msg -> Printf.printf "PVE Error :: %s\n" msg

let () =
  match fetch_with_extended_error "notfound" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_with_extended_error "timeout" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_with_extended_error "network" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_with_extended_error "" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err

let () =
  match fetch_with_extended_error "https://jmsdnns.com" with
  | Ok msg -> Printf.printf "PVE Success :: %s\n" msg
  | Error err -> handle_error err
