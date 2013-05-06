module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)

open Eliom_content.Html5.D
open Lwt

(* ----- Services / Links  ----- *)
open Services

(* ----- Scope  ----- *)

let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

let wrong_pwd =
  Eliom_reference.eref
    ~scope:Eliom_common.request_scope false

(* ----- Function / Action ----- *)

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  ul (List.map (fun (name, _) ->
                  li [a
                        ~service:user_service [pcdata name] name])
               !users)

(* ----- Check Database Function ----- *)
let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler = ref None in
  fun () ->
    match !db_handler with
    | Some h -> Lwt.return h
    | None -> Lwt_PGOCaml.connect ~database:"macaque-demo" ()

let table = <:table< users (
  login text NOT NULL,
  password text NOT NULL
) >>

let find name =
  (get_db () >>= fun dbh ->
   Lwt_Query.view dbh
   <:view< {password = user_.password} |
            user_ in $table$;
            user_.login = $string:name$; >>)

let insert name pwd =
  (get_db () >>= fun dbh ->
  Lwt_Query.query dbh
  <:insert< $table$ :=
    { login = $string:name$; password = $string:pwd$; } >>)

let check_pwd name pwd =
  (get_db () >>= fun dbh ->
   Lwt_Query.view dbh
   <:view< {password = user_.password} |
            user_ in $table$;
            user_.login = $string:name$;
    user_.password = $string:pwd$ >>)
  >|= (function [] -> false | _ -> true)

let check_pwd_old name pwd =
  try List.assoc name !users = pwd with Not_found -> false

(* ----- HTML ----- *)
let disconnect_box () =
  post_form disconnection_service
    (fun _ -> [p [string_input
                     ~input_type:`Submit ~value:"Log out" ()]]) ()

let login_box =
  [post_form ~service:connection_service
      (fun (name1, name2) ->
        [fieldset
            [label ~a:[a_for name1] [pcdata "login: "];
             string_input ~input_type:`Text
               ~name:name1 ();
             br ();
             label ~a:[a_for name2] [pcdata "password: "];
             string_input ~input_type:`Password
               ~name:name2 ();
             br ();
             string_input ~input_type:`Submit
               ~value:"Connect" ()
            ]]) ();
   p [a new_user_form_service
         [pcdata "Create an account"] ()]]

(*
   To use this connected_box
   Declare let cf = div (connected_box username) in
   And use cf; after that
*)
let connected_box username =
  [p [pcdata "You are connected as "; pcdata username; ];
   disconnect_box ()]

(*
   To use this connection_box
   Declare lwt cf = connection_box ()
   And use cf; after that
   It uses lwt and thus require the lwt declaration
 *)
let connection_box () =
  lwt u = Eliom_reference.get username in
  lwt wp = Eliom_reference.get wrong_pwd in
  Lwt.return
    (match u with
    | Some s -> div (connected_box s)
    | None ->
      if wp
      then div ((p [em [pcdata "Wrong user or password"]])::login_box)
      else div login_box
    )

let account_form =
  (* post_form ~service:create_account_service *)
  post_form ~service:account_confirmation_service
    (fun (name1, name2) ->
      [fieldset
          [label ~a:[a_for name1] [pcdata "login: "];
           string_input ~input_type:`Text ~name:name1 ();
           br ();
           label ~a:[a_for name2] [pcdata "password: "];
           string_input ~input_type:`Password ~name:name2 ();
           br ();
           string_input ~input_type:`Submit ~value:"Register" ()
          ]]) ()

(* ----- Service Registration ----- *)
let authenticated_handler f =
  let handle_anonymous _get _post =
    lwt cf = connection_box () in
    Lwt.return
      (html
         (head (title (pcdata "")) [])
         (body [h1 [pcdata "Please connect"];
                cf;]))
  in
  Eliom_tools.wrap_handler
    (fun () -> Eliom_reference.get username)
    handle_anonymous (* Called when [username] is [None] *)
    f (* Called [username] contains something *)


let _ = Eliom_registration.Html5.register
  ~service:main_service
  (authenticated_handler
     (fun username _get _post ->
       let cf = div (connected_box username) in
       Lwt.return
         Eliom_content.Html5.F.(html
                     (head (title (pcdata "")) [])
                     (body [h1 [pcdata ("Hello " ^ username) ];
                           cf;]))))

let _ = Eliom_registration.Html5.register
  ~service:alt_main_service
  (fun () () ->
    lwt cf = connection_box () in
  Lwt.return
    (html (head (title (pcdata "")) [])
       (body [h1 [pcdata "Hello"];
              cf;
              user_links ()])
    ))

(* .send function from the module to send the output *)
let _ = Eliom_registration.Any.register
  ~service:user_service
  (fun name () ->
    if List.exists (fun (n, _) -> n = name) !users
    then begin
      lwt cf = connection_box () in
         Eliom_registration.Html5.send
           (html (head (title (pcdata name)) [])
              (body [h1 [pcdata name];
                     cf;
                     p [a ~service:main_service [pcdata "Home"] ()]]))
   end else
  Eliom_registration.Html5.send
    ~code:404 (* specify the error code but page need to configure *)
    (html (head (title (pcdata "404")) [])
       (body [h1 [pcdata "404"];
              p [pcdata "That page does not exist"]]))
    )

let _ = Eliom_registration.Html5.register
  ~service:hacker_service
  (authenticated_handler
     (fun username _get _post ->
       Lwt.return (html
                     (head (title (pcdata "")) [])
                     (body [h1 [pcdata ("Hello " ^ username)];
                            span [pcdata "_get: "];
                            span [pcdata _get]]
                     ))))

let _ = Eliom_registration.Html5.register
  ~service:old_connection_service
  (fun () (name, password) ->
    lwt message =
      if check_pwd_old name password
      then begin
        Eliom_reference.set username (Some name) >>=
          (fun _ -> Lwt.return ("Hello "^name))
      end else
        Lwt.return "Wrong name or password" in
  Lwt.return
    (html (head (title (pcdata "")) [])
       (body [h1 [pcdata message];
              user_links ()])))

let _ = Eliom_registration.Action.register
  ~service:connection_service
  (fun () (name, password) ->
    check_pwd name password >>=
      (function
      | true -> Eliom_reference.set username (Some name)
      | false -> Lwt.return ()))

let _ = Eliom_registration.Action.register
  ~service:disconnection_service
  (fun () () -> Eliom_state.discard ~scope:Eliom_common.default_session_scope ())

let _ = Eliom_registration.Html5.register
  ~service:new_user_form_service
  (fun () () ->
    Lwt.return
      (html (head (title (pcdata "")) [])
         (body [h1 [pcdata "Create an account"];
                account_form;
               ])))

let _ = Eliom_registration.Action.register
  ~service:create_account_service
  (fun () (name, pwd) ->
    find name >>=
      (function
      | [] -> insert name pwd
      | _ -> Lwt.return ()) )

let _ = Eliom_registration.Html5.register
  ~service:account_confirmation_service
  (fun () (name, pwd) ->
    let create_account_service =
      Eliom_registration.Action.register_coservice
        ~fallback:main_service
        ~get_params:Eliom_parameter.unit
        ~timeout:30. (* timeout in seconds *)
        (fun () () ->
          (* users := (name, pwd)::!users; *)
          insert name pwd;
          Lwt.return ())
    in
    Lwt.return
      (html
         (head (title (pcdata "")) [])
         (body
            [h1 [pcdata "Confirm account creation for "; pcdata name];
             p [a ~service:create_account_service [pcdata "Yes"] ();
                pcdata " ";
                a ~service:main_service [pcdata "No"] ()]
            ])))
