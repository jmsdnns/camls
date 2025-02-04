open Printf
open Csv


let embedded_csv = "\
\"Name\",\"Age\",\"City\",
\"James\",\"25\",\"NYC\"
\"CCB\",\"26\",\"Philly\"
\"Malcolm\",\"27\",\"Amsterdam\"
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

  print_endline "Rows:";
  List.iter (fun row ->
    let get_column_value col_name =
      try
        let idx = List.assoc col_name header_index_map in
        List.nth row idx
      with
      | Not_found -> failwith ("Column not found in header: " ^ col_name)
    in

    let name = get_column_value "Name" in
    let age = get_column_value "Age" in
    let city = get_column_value "City" in

    Printf.printf "  - Name: %s\n  - Age: %s\n  - City: %s\n" name age city
  ) (List.tl csv) 
  

(* from file *)
let () =
  let csv_file = Csv.load "data/example.csv" in
  print_it csv_file

(* from memory *)
let () =
  let csv_data = Csv.input_all(Csv.of_string embedded_csv) in
  print_it csv_data
