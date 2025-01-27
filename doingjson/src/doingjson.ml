(*
ocamlfind ocamlopt -o doingjson -linkpkg -package ppx_deriving.show,ppx_deriving.ord,ppx_deriving.eq,ppx_deriving_yojson,yojson doingjson.ml
*)

type t = { name: string; age: int } [@@deriving show, yojson, ord, eq]

let () = 
    let foo = { name = "bart"; age = 39 } in
    let as_str = Yojson.Safe.pretty_to_string (to_yojson foo) in
    Printf.printf "%s\n" as_str;

    let as_yo = Yojson.Safe.from_string as_str in
    let as_str = Yojson.Safe.pretty_to_string as_yo in
    Printf.printf "%s\n" as_str;
