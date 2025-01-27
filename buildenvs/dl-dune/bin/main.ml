(*
compile with:
  `ocamlfind ocamlopt -thread -o downloader -linkpkg -package lwt,cohttp-lwt-unix,lwt_ssl downloader.ml`
*)

module Downloader = struct

  (* download a single file *)
  let dl_file url filename =
    let open Lwt.Infix in
    Cohttp_lwt_unix.Client.get (Uri.of_string url) >>= fun (resp, body) ->
    let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    if Cohttp.Code.(is_success status_code) then
      Lwt_io.with_file ~mode:Lwt_io.Output filename (fun file ->
        Cohttp_lwt.Body.to_string body >>= fun content ->
        Lwt_io.write file content >>= fun () ->
        Lwt_io.printf "Aww yeah got the file: %s\n" filename
      )
    else
      Lwt_io.printf "ERROR: this shit's all fucked up %s. HTTP(%d)\n" filename status_code >>= fun () ->
      Lwt.return ()

  (* download a list of files *)
  let dl_files urls_and_filenames =
    Lwt_list.iter_p (fun (url, filename) ->
      dl_file url filename
    ) urls_and_filenames

end

let () =
  let urls_and_filenames = [
    ("https://jmsdnns.com/images/james-and-benjamin.png", "james-and-benjamin.png");
    ("https://jmsdnns.com/images/history/index.png", "bookshelf.png");
  ] in
  let _ = Lwt_main.run (Downloader.dl_files urls_and_filenames) in ()

