{shared{
  open Eliom_lib
  open Eliom_content
}}

exception E of string

let defaulot d = function
  | None -> d
  | Some x -> x

let start =
  (* The database is called test.db. Delete it if it already exists. *)
  let db_filename = "users.db" in
  (    try Unix.unlink db_filename
       with _ -> ()
  ) ;

  (* Create a new database. *)
  let db = Sqlite3.db_open db_filename in

  (* Close database when done. *)
  if Sqlite3.db_close db then print_endline "All done.\n"
  else print_endline "Cannot close database.\n"

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


module Db_app =
  Eliom_registration.App (
    struct
      let application_name = "helloworld"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let rec fact = function
  | 0 -> 1
  | n -> n * fact(n-1)

let () =
  Db_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Database"
           ~css:[["css";"helloworld-app.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
             p [
               pcdata ("fact(10) = ");
               pcdata (string_of_int(fact(10)));
             ]])))

let fact =
  Db_app.register_service
    ~path:["fact"]
    ~get_params:(Eliom_parameter.int "seed")
    (fun seed () ->
      Lwt.return
        Html5.D.(html
                   (head (title (pcdata "")) [])
                   (body
                      [p [
                        pcdata "factorial of ";
                        strong [pcdata(string_of_int(seed))];
                        pcdata ": ";
                        strong [pcdata(string_of_int(fact(seed)))]]])))

let create =
  Db_app.register_service
    ~path:["create"]
    ~get_params:(Eliom_parameter.string "dbname")
    (fun dbname () ->
      start;
      create_tables (Sqlite3.db_open "users.db");
      Lwt.return
        Html5.D.(html
                   (head (title(pcdata "DB Creation")) [])
                   (body [p [ pcdata dbname;
                            ]])))

let insert =
  Db_app.register_service
    ~path:["insert"]
    ~get_params:(Eliom_parameter.string "data")
    (fun data () ->
      insert_data (Sqlite3.db_open "users.db");
      Lwt.return
        Html5.D.(html
                   (head (title(pcdata "DB Insertion")) [])
                   (body [p [pcdata "insert";
                            ]])))
