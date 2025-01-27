module UserModel = struct
    module DB = Sqlite3
    module JSON = Yojson.Basic

    let create_table db =
        let create_sql =
            "CREATE TABLE IF NOT EXISTS users (" ^
            "id INTEGER PRIMARY KEY, " ^
            "name TEXT, " ^
            "age INTEGER, " ^
            "json_data TEXT);" in
        let rc = DB.exec db create_sql in
        match rc with
        | DB.Rc.OK -> Printf.printf "Users table created\n"
        | _ -> Printf.printf "ERROR: could not create table. %s\n" (DB.Rc.to_string rc)

    let insert_user db name age json_data =
        let json_str = JSON.to_string json_data in
        let insert_sql =
            "INSERT INTO " ^
            "users (name, age, json_data) " ^
            "VALUES (?, ?, ?);" in
        try
            let stmt = DB.prepare db insert_sql in
            let rc1 = DB.bind_text stmt 1 name in
            let rc2 = DB.bind_int stmt 2 age in
            let rc3 = DB.bind_text stmt 3 json_str in
            if rc1 = DB.Rc.OK && rc2 = DB.Rc.OK && rc3 = DB.Rc.OK then
                let rc_exec = DB.step stmt in
                match rc_exec with
                | DB.Rc.DONE -> Printf.printf "User created: '%s'\n" name
                | _ -> Printf.printf "ERROR: could not insert user. %s\n" (DB.Rc.to_string rc_exec)
            else
                Printf.printf "Binding failed. Error: %s, %s, %s\n"
                    (DB.Rc.to_string rc1) (DB.Rc.to_string rc2) (DB.Rc.to_string rc3);
            DB.finalize stmt |> ignore
        with DB.Error msg ->
            Printf.printf "ERROR: insert statement %s\n" msg

    let query_users db =
        let select_sql =
            "SELECT id, name, age, json_data " ^
            "FROM users;" in
        try
            let stmt = DB.prepare db select_sql in
            let rec loop () =
                match DB.step stmt with
                | DB.Rc.ROW -> 
                    let id = DB.column_int stmt 0 in
                    let name = DB.column_text stmt 1 in
                    let age = DB.column_int stmt 2 in
                    let json_data_str = DB.column_text stmt 3 in
                    begin
                        try
                            let json_data = JSON.from_string json_data_str in
                            Printf.printf "ID: %d, Name: %s, Age: %d, Hobbies: %s\n"
                                id name age (JSON.to_string json_data)
                        with Yojson.Json_error e ->
                            Printf.printf "ERROR: hobby json failed %s\n" e
                        end;
                    loop ()
                | DB.Rc.DONE -> ()
                | _ -> Printf.printf "ERROR: select failed %s\n" (DB.Rc.to_string (DB.step stmt))
            in
            loop ();
            DB.finalize stmt |> ignore
        with DB.Error msg ->
            Printf.printf "ERROR: preparing statement: %s\n" msg

    let query_users_by_hobby db hobby =
        let select_sql =
            "SELECT u.id AS user_id, u.name, u.age, u.json_data " ^
            "FROM users u " ^
            "JOIN json_each(json_extract(u.json_data, '$.hobbies')) AS j " ^
            "WHERE j.value LIKE ?" in
        try
            let stmt = DB.prepare db select_sql in
            let hobby_pattern = "%" ^ hobby ^ "%" in
            let rc = DB.bind_text stmt 1 hobby_pattern in
            if rc = DB.Rc.OK then
                let rec loop () =
                    match DB.step stmt with
                    | DB.Rc.ROW -> 
                        let user_id = DB.column_int stmt 0 in
                        let name = DB.column_text stmt 1 in
                        let age = DB.column_int stmt 2 in
                        Printf.printf "User '%s' (ID: %d, Age: %d) has hobby matching '%s'.\n"
                            name user_id age hobby;
                        loop ()
                    | DB.Rc.DONE -> ()
                    | _ -> Printf.printf "Error executing query: %s\n" (DB.Rc.to_string (DB.step stmt))
                in
                loop ();
                DB.finalize stmt |> ignore
            else
                Printf.printf "Error binding hobby pattern: %s\n" (DB.Rc.to_string rc)
        with DB.Error msg ->
            Printf.printf "Error preparing statement: %s\n" msg
end

let main () =
    (* Create DB with user table *)
    let db_name = "the.db" in
    let db =
        try
            Sqlite3.db_open db_name
        with
            | Sqlite3.Error msg ->
            Printf.printf "Failed to open database: %s\n" msg;
            exit 1
    in
    UserModel.create_table db;

    (* Insert two users with hobbies stored as JSON *)
    let james_data = `Assoc [("hobbies", `List [`String "Drums"; `String "Music"])] in
    let malcolm_data = `Assoc [("hobbies", `List [`String "Boating"; `String "Music"])] in
    UserModel.insert_user db "James" 21 james_data;
    UserModel.insert_user db "Malcolm" 25 malcolm_data;

    (* Select all users in DB *)
    Printf.printf "\nWhat users we got in this thing?\n";
    UserModel.query_users db;

    (* Query users by hobby *)
    Printf.printf "\nWho likes the drums?\n";
    UserModel.query_users_by_hobby db "Drums";
    Printf.printf "\nWho likes the boats?\n";
    UserModel.query_users_by_hobby db "Boating";
    Printf.printf "\nWho likes music?\n";
    UserModel.query_users_by_hobby db "Music";

    Sqlite3.db_close db |> ignore

let () = main ()

