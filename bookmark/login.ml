open Eliom_content.Html5.D
open Link

let login_box =
  [post_form ~service:authentication_service
      (fun (username, password) ->
        [fieldset
            [label ~a:[a_for username] [pcdata "username: "];
             string_input ~input_type:`Text
               ~name:username ();
             br ();
             label ~a:[a_for password] [pcdata "password: "];
             string_input ~input_type:`Password
               ~name:password ();
             br ();
             string_input ~input_type:`Submit
               ~value:"Connect" ()
            ]]) ();
   p [a main_service
         [pcdata "Create an account"] ()]
  ]
