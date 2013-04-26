open Eliom_content.Html5.D
let main_service =
  Eliom_service.service
    ~path:[""]
    ~get_params:Eliom_parameter.unit ()

let user_service =
  Eliom_service.service
    ~path:["users"] ~get_params:Eliom_parameter.(suffix (string "name")) ()

let connection_service =
  Eliom_service.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** int "password")
    ()

(* User names and passwords: *)
let users = ref [("Calvin", 123); ("Hobbes", 456)]

let user_links () =
  ul (List.map (fun (name, _) ->
                  li [a
                        ~service:user_service [pcdata name] name])
               !users)

let connection_box () =
  post_form ~service:connection_service
    (fun (name1, name2) ->
      [fieldset
          [label ~a:[a_for name1] [pcdata "login: "];
           string_input ~input_type:`Text
             ~name:name1 ();
           br ();
           label ~a:[a_for name2] [pcdata "password: "];
           int_input ~input_type:`Password
             ~name:name2 ();
           br ();
           label ~a:[a_for name2] [pcdata "domain: "];
           string_input ~input_type:`Text
             ();
           br ();
           string_input ~input_type:`Submit
             ~value:"Connect" ()
          ]]) ()

let main_reg =
  Eliom_registration.Html5.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body [h1 [pcdata "Hello"];
                  user_links ();
                  br ();
                  connection_box () ])));

  Eliom_registration.Html5.register
    ~service:user_service
    (fun name () ->
      Lwt.return
        (html (head (title (pcdata name)) [])
           (body [h1 [pcdata name];
                  p [a ~service:main_service [pcdata "Home"] ()]])))

let connection_reg =
  Eliom_registration.Html5.register
    ~service:connection_service
    (fun () (name, password) ->
      let check_pwd name pwd =
        try List.assoc name !users = pwd with Not_found -> false
      in
      let message =
        if check_pwd name password
        then "Hello "^name
        else "Wrong name or password"
      in
      Lwt.return
        (html (head (title (pcdata "")) [])
           (body [h1 [pcdata message];
                  user_links ()])));
