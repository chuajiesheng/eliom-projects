#directory "+sqlite3";;
#load "sqlite3.cma";;
#load "unix.cma";;

exception E of string

let default d = function
  | None -> d
  | Some x -> x

let create_tables db =
  (* Create two tables in the database. *)
  let tables =
    [    "users", "user_id INTEGER PRIMARY KEY, username TEXT, password TEXT" ;
         "properties", "property_id INTEGER PRIMARY KEY, property TEXT, value TEXT" ;
    ]
  in
  let make_table (name, layout) =
    let stmt = Printf.sprintf "CREATE TABLE %s (%s);" name layout in
    match Sqlite3.exec db stmt with
    |    Sqlite3.Rc.OK -> Printf.printf "Table '%s' created.\n" name
    |    x -> raise (E (Sqlite3.Rc.to_string x))
  in
  List.iter make_table tables


let insert_data db =
  (* Insert data in both the tables. *)
  let users_data =
    [    "John", "password";
         "Helen", "password";
         "Adam", "password";
    ]
  in
  let properties_data =
    [    "admin", "John" ;
    ]
  in
  let insert_users (username, password) =
    (* Use NULL for primary key and Sqlite will generate a unique key. *)
    let stmt = Printf.sprintf "INSERT INTO users values (NULL, '%s', '%s');"
      username password
    in
    match Sqlite3.exec db stmt with
    |    Sqlite3.Rc.OK -> ()
    |    x -> raise (E (Sqlite3.Rc.to_string x))
  in
  let insert_properties (property, value) =
    let stmt = Printf.sprintf "INSERT INTO properties values (NULL, '%s', '%s');"
      property value
    in
    match Sqlite3.exec db stmt with
    |    Sqlite3.Rc.OK -> ()
    |    x -> raise (E (Sqlite3.Rc.to_string x))
  in
  List.iter insert_users users_data ;
  List.iter insert_properties properties_data ;
  print_endline "Data inserted."


let list_tables db =
  (* List the table names of the given database. *)
  let lister row headers =
    Printf.printf "    %s : '%s'\n" headers.(0) row.(0)
  in
  print_endline "Tables :" ;
  let code = Sqlite3.exec_not_null db ~cb:lister
    "SELECT name FROM sqlite_master;"
  in
  (    match code with
  |    Sqlite3.Rc.OK -> ()
  |    x -> raise (E (Sqlite3.Rc.to_string x))
  ) ;
  print_endline "------------------------------------------------"


let search_callback db =
  (* Perform a simple search using a callback. *)
  let print_headers = ref true in
  let lister row headers =
    if !print_headers then
      (    Array.iter (fun s -> Printf.printf "  %-12s" s) headers ;
           print_newline () ;
           print_headers := false
      ) ;
    Array.iter (Printf.printf "  %-12s") row ;
    print_newline ()
  in
  print_endline "Admin Users:" ;
  let code = Sqlite3.exec_not_null db ~cb:lister
    "SELECT * FROM properties WHERE property = 'admin';"
  in
  match code with
  |    Sqlite3.Rc.OK -> ()
  |    x -> raise (E (Sqlite3.Rc.to_string x))



let search_iterator db =
  (* Perform a simple search. *)
  let str_of_rc rc =
    match rc with
    |    Sqlite3.Data.NONE -> "none"
    |    Sqlite3.Data.NULL -> "null"
    |    Sqlite3.Data.INT i -> Int64.to_string i
    |    Sqlite3.Data.FLOAT f -> string_of_float f
    |    Sqlite3.Data.TEXT s -> s
    |    Sqlite3.Data.BLOB _ -> "blob"
  in
  let dump_output s =
    Printf.printf "  Row   Col   ColName    Type       Value\n%!"  ;
    let row = ref 0 in
    while Sqlite3.step s = Sqlite3.Rc.ROW do
      for col = 0 to Sqlite3.data_count s - 1 do
        let type_name = Sqlite3.column_decltype s col in
        let val_str = str_of_rc (Sqlite3.column s col) in
        let col_name = Sqlite3.column_name s col in
        Printf.printf "  %2d  %4d    %-10s %-8s   %s\n%!"
          !row col col_name (default "" type_name) val_str ;
      done ;
      row := succ !row ;
    done
  in
  print_endline "Users:" ;
  let stmt = Sqlite3.prepare db "SELECT * FROM users;" in
  dump_output stmt    ;
  match Sqlite3.finalize stmt with
  |    Sqlite3.Rc.OK -> ()
  |    x -> raise (E (Sqlite3.Rc.to_string x))


let update db =
  print_endline "Admin is change to Adam, so update table." ;
  let stmt = "UPDATE properties SET value = 'Adam' WHERE " ^
    "property = 'admin';"
  in
  (    match Sqlite3.exec db stmt with
  |    Sqlite3.Rc.OK -> ()
  |    x -> raise (E (Sqlite3.Rc.to_string x))
  ) ;
  search_callback db


let delete_from db =
  print_endline "Helen quit, so remove from table." ;
  let stmt = "DELETE FROM users WHERE username = 'Helen';" in
  match Sqlite3.exec db stmt with
  |    Sqlite3.Rc.OK -> ()
  |    x -> raise (E (Sqlite3.Rc.to_string x))


let play_with_database db =
  print_endline "" ;
  create_tables db ;
  print_endline "------------------------------------------------" ;
  list_tables db ;
  insert_data db ;
  print_endline "------------------------------------------------" ;
  search_callback db ;
  print_endline "------------------------------------------------" ;
  search_iterator db ;
  print_endline "------------------------------------------------" ;
  update db ;
  print_endline "------------------------------------------------" ;
  delete_from db ;
  print_endline "------------------------------------------------" ;
  search_iterator db ;
  print_endline "------------------------------------------------"

(* Program main. *)

let () =
  (* The database is called test.db. Delete it if it already exists. *)
  let db_filename = "test.db" in
  (    try Unix.unlink db_filename
       with _ -> ()
  ) ;

  (* Create a new database. *)
  let db = Sqlite3.db_open db_filename in

  play_with_database db ;

      (* Close database when done. *)
  if Sqlite3.db_close db then print_endline "All done.\n"
  else print_endline "Cannot close database.\n"
