(* ----- Services / Links  ----- *)
let main_service =
  Eliom_service.service ~path:[""] ~get_params:Eliom_parameter.unit ()

let alt_main_service =
  Eliom_service.service ~path:["alt"] ~get_params:Eliom_parameter.unit ()

let user_service =
  Eliom_service.service
    ~path:["users"] ~get_params:Eliom_parameter.(suffix (string "name")) ()

let old_connection_service =
  Eliom_service.post_service
    ~fallback:alt_main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

let connection_service =
  Eliom_service.post_coservice'
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()

(*
   this coservices allow the webapp to logout a user yet stay on the same url
   i.e. localhost:8080/users/Calvin -> logout()
   webapp reload localhost:8080/users/Calvin
   but now this time have a login form instead of a logout form
 *)
let disconnection_service =
  Eliom_service.post_coservice' ~post_params:Eliom_parameter.unit ()

let new_user_form_service =
  Eliom_service.service ~path:["registration"] ~get_params:Eliom_parameter.unit ()

(*
  this use of coservice allow the create user form to submit the parameter
  then redirect to the service stated in the fallback
  could try with "new_user_form_serivce" | "main_service"
*)
let create_account_service =
  Eliom_service.post_coservice
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "name" ** string "password") ()

let account_confirmation_service =
  Eliom_service.post_coservice
    ~fallback:new_user_form_service
    ~post_params:Eliom_parameter.(string "name" ** string "password")
    ()
