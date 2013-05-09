let main_service =
  Eliom_service.service ~path:[""]
    ~get_params:Eliom_parameter.unit
    ()

let authentication_service =
  Eliom_service.post_service
    ~fallback:main_service
    ~post_params:Eliom_parameter.(string "username" ** string "password")
    ()
