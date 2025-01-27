module Cli = struct
    module C = Cmdliner

    let namee =
        let doc = "namee of the person to greet." in
        C.Arg.(required & pos 0 (some string) None & info [] ~doc)

    let greet_cmd f =
        let doc = "meow" in
        let exits = C.Cmd.Exit.defaults in
        C.Cmd.v (C.Cmd.info "greet" ~doc ~exits) C.Term.(const f $ namee)

    let farewell_cmd f =
        let doc = "meow" in
        let exits = C.Cmd.Exit.defaults in
        C.Cmd.v (C.Cmd.info "farewell" ~doc ~exits) C.Term.(const f $ namee)

end

let greet namee =
    Printf.printf "G'morning, %s!\n" namee

let farewell namee =
    Printf.printf "Take five, %s!\n" namee

let () =
    let info = Cmdliner.Cmd.info "poopycli" in
    exit @@ Cmdliner.Cmd.eval @@ Cmdliner.Cmd.group info [Cli.greet_cmd greet; Cli.farewell_cmd farewell]
