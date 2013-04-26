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
let connection_box () =
  lwt u = Eliom_reference.get username in
  Lwt.return
    (match u with
    | Some s -> p [pcdata "You are connected as "; pcdata s]
    | None ->
        post_form ~service:connection_service
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
                   ]]) ()) in

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
      Lwt.return
        (html (head (title (pcdata name)) [])
              (body [h1 [pcdata name];
                     p [a ~service:main_service [pcdata "Home"] ()]])));

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
