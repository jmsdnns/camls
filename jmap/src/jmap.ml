module Scanner = struct

  (* attempt to open a port and return an option *)
  let open_port host port timeout = fun () -> 
    let open Lwt.Infix in
    let sockaddr = Unix.ADDR_INET (Unix.inet_addr_of_string host, port) in
    let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Lwt.catch
      (* Port is open *)
      (fun () ->
        Lwt_unix.connect socket sockaddr >>= fun () ->
        Lwt_unix.close socket >>= fun () ->
        Lwt.return (Some port) 
      )
      (* Port is closed *)
      (fun _ ->
        Lwt_unix.close socket >>= fun () ->
        Lwt.return None 
      )

  (* scans a port by trying to open it or timing out *)
  let scan_port host port timeout =
    let open Lwt.Infix in
    let connect_task = open_port host port timeout in
    (*let timeout_task = Lwt_unix.sleep timeout >>= fun () -> Lwt.fail Lwt_unix.Timeout in*)
    let timeout_task = Lwt_unix.sleep timeout >>= fun () -> Lwt.fail Lwt.Canceled in
    Lwt.pick [connect_task (); timeout_task] >>= function
    | Some port -> Lwt.return (Some port)
    | None -> Lwt.return None

  (* scans a range of port *)
  let scan_ports host ports timeout =
    let open Lwt.Infix in
    let scan_results =
      List.map (fun port ->
        scan_port host port timeout >>= function 
        | Some port ->
          Printf.printf "Port %d is open\n" port;
          Lwt.return ()
        | None -> 
          Printf.printf "Port %d is closed\n" port;
          Lwt.return ()
      ) ports
    in
    Lwt.join scan_results

end


module Cli = struct
  module C = Cmdliner

  let default_ports = [3000; 8000; 8080]

  let parse_ports input =
    let parse_range range_str =
      try
        let parts = String.split_on_char '-' range_str in
        match parts with
        | [start_str; end_str] ->
          let start = int_of_string start_str in
          let finish = int_of_string end_str in
          if start <= finish then
            Some (List.init (finish - start + 1) (fun i -> start + i))
          else
            None
        | _ -> None
      with _ -> None
    in

    try
      let ports = String.split_on_char ',' input in
      let all_ports = List.fold_left (fun acc port ->
        match parse_range port with
        | Some range -> acc @ range
        | None -> acc @ [int_of_string port]
      ) [] ports in
      Some all_ports
    with _ -> None

  let host = 
    let docv = "HOST" in
    let doc = "The host to scan (default is 127.0.0.1)." in
    C.Arg.(value & opt string "127.0.0.1" & info ["h"; "host"] ~docv ~doc)

  let ports = 
    let docv = "PORTS" in
    let doc = "Comma-separated list of ports or a single port to scan." in
    C.Arg.(value & opt (some string) None & info ["p"; "ports"] ~docv ~doc)

  let timeout = 
    let docv = "TIMEOUT" in
    let doc = "Timeout in seconds (default is 2.0)." in
    C.Arg.(value & opt float 2.0 & info ["t"; "timeout"] ~docv ~doc)

  let scan_action host ports timeout =
    let open Lwt.Infix in
    let ports_to_scan =
      match ports with
      | Some port_str -> (
        match parse_ports port_str with
        | Some ports -> ports
        | None -> default_ports 
      )
      | None -> default_ports 
    in
    Lwt_main.run (
      Scanner.scan_ports host ports_to_scan timeout >>= fun () -> 
      Lwt.return ()
    )

  let cmd =
    C.Term.(const scan_action $ host $ ports $ timeout)
end


let () =
  let version = "v3.1415" in
  let doc = "A simple port scanner written in OCaml." in
  let info = Cmdliner.Cmd.info "jmap" ~version ~doc in
  let cmd = Cmdliner.Cmd.v info Cli.cmd in
  exit @@ Cmdliner.Cmd.eval cmd
