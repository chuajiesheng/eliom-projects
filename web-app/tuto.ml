open Eliom_content.Html5.D
open Lwt

(* ----- Services / Links  ----- *)
open Services

(* ----- Scope  ----- *)

let username =
  Eliom_reference.eref
    ~scope:Eliom_common.default_session_scope None

(* ----- Function / Action ----- *)

(* User names and passwords: *)
let users = ref [("Calvin", "123"); ("Hobbes", "456")]

let user_links () =
  ul (List.map (fun (name, _) ->
                  li [a
                        ~service:user_service [pcdata name] name])
               !users)

let check_pwd name pwd =
  try List.assoc name !users = pwd with Not_found -> false

(* ----- HTML ----- *)
let disconnect_box () =
  post_form disconnection_service
    (fun _ -> [p [string_input
                    ~input_type:`Submit ~value:"Log out" ()]]) () in

let connection_box () =
  lwt u = Eliom_reference.get username in
  Lwt.return
    (match u with
    | Some s -> div [p [pcdata "You are connected as "; pcdata s; ];
                       disconnect_box ()]
    | None ->
        div [post_form ~service:connection_service
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
                  [pcdata "Create an account"] ()]
            ]) in

let account_form =
  post_form ~service:create_account_service
    (fun (name1, name2) ->
      [fieldset
         [label ~a:[a_for name1] [pcdata "login: "];
          string_input ~input_type:`Text ~name:name1 ();
          br ();
          label ~a:[a_for name2] [pcdata "password: "];
          string_input ~input_type:`Password ~name:name2 ();
          br ();
          string_input ~input_type:`Submit ~value:"Connect" ()
         ]]) () in

(* ----- Service Registration ----- *)
Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
      lwt cf = connection_box () in
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Hello"];
                     cf;
                     user_links ()])));

Eliom_registration.Html5.register
    ~service:alt_main_service
    (fun () () ->
      lwt cf = connection_box () in
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Hello"];
                     cf;
                     user_links ()])
         ));

Eliom_registration.Html5.register
    ~service:user_service
    (fun name () ->
      lwt cf = connection_box () in
      Lwt.return
        (html (head (title (pcdata name)) [])
              (body [h1 [pcdata name];
                     cf;
                     p [a
                          ~service:main_service [pcdata "Home"] ()]])));

Eliom_registration.Html5.register
    ~service:old_connection_service
    (fun () (name, password) ->
      lwt message =
        if check_pwd name password
        then begin
          Eliom_reference.set username (Some name) >>=
            (fun _ -> Lwt.return ("Hello "^name))
        end else
      Lwt.return "Wrong name or password" in
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata message];
                     user_links ()])));

Eliom_registration.Action.register
    ~service:connection_service
    (fun () (name, password) ->
      if check_pwd name password
      then Eliom_reference.set username (Some name)
      else Lwt.return ());

Eliom_registration.Action.register
    ~service:disconnection_service
    (fun () () -> Eliom_state.discard ~scope:Eliom_common.default_session_scope ());

Eliom_registration.Html5.register
    ~service:new_user_form_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
              (body [h1 [pcdata "Create an account"];
                     account_form;
                    ])));

Eliom_registration.Action.register
    ~service:create_account_service
    (fun () (name, pwd) ->
      users := (name, pwd)::!users;
      Lwt.return ());
