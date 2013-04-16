{shared{
  open Eliom_lib
  open Eliom_content
}}

module Basic_form_app =
  Eliom_registration.App (
    struct
      let application_name = "basic_form"
    end)

let main_service =
  Eliom_service.service ~path:[] ~get_params:Eliom_parameter.unit ()

let () =
  Basic_form_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"basic_form"
           ~css:[["css";"basic_form.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
           ])))
