{shared{
  open Eliom_lib
  open Eliom_content
}}

module Helloworld_app =
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
  Helloworld_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"helloworld"
           ~css:[["css";"helloworld-app.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's destillery!"];
             p [pcdata(string_of_int(fact(10)))];
           ])))

let fact =
  Helloworld_app.register_service
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
