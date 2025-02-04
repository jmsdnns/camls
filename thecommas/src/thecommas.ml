open Printf
open Csv


let embedded_csv = "\
\"Name\",\"Age\",\"City\"
\"Billy Bob\",\"25\",\"Tennessee\"
\"Sierra\",\"26\",\"Meowchigan City\"
\"V\",\"27\",\"Night City\"
"

let column_name_to_index = [
  ("Name", 0);
  ("Age", 1);
  ("City", 2);
]

let load_csv filename = Csv.load filename

let print_it csv =
  let header = List.hd csv in
  let header_index_map = 
    List.mapi (fun idx col_name -> (col_name, idx)) header
  in
  print_endline "Headers:";
  List.iter (fun h -> Printf.printf "  - %s\n" h) header;
  print_endline "";

  print_endline "Rows:";
  let get_column_value col_name row =
    try
      let idx = List.assoc col_name header_index_map in
      List.nth row idx
    with
    | Not_found -> failwith ("Column not found in header: " ^ col_name)
  in
  let print_row row =
    let name = get_column_value "Name" row in
    let age = get_column_value "Age" row in
    let city = get_column_value "City" row in
    Printf.printf "  - Name: %s\n  - Age: %s\n  - City: %s\n\n" name age city
  in
  List.iter print_row (List.tl csv)
  

(* from file *)
let () =
  let csv_file = Csv.load "data/example.csv" in
  print_it csv_file

(* from memory *)
let () =
  let csv_data = Csv.input_all(Csv.of_string embedded_csv) in
  print_it csv_data
