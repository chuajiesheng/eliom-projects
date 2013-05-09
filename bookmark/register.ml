open Eliom_content.Html5.D
open Eliom_content
open Link
open Login

let _ = Eliom_registration.Html5.register
  ~service:main_service
  (fun () () ->
    let cf = div (login_box) in
    Lwt.return
      (Eliom_tools.F.html
         ~title:"bookmark"
         ~css:[["css";"bookmark.css"]]
         Html5.F.(body [
           h2 [pcdata "Welcome from Eliom's destillery!"];
           cf;
         ])))

let _ = Eliom_registration.Action.register
  ~service:authentication_service
  (fun () (username, password) ->
    Lwt.return ())
