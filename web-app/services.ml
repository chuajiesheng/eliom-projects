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
