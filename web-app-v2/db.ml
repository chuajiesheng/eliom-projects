module Lwt_thread = struct
  include Lwt
  include Lwt_chan
end
module Lwt_PGOCaml = PGOCaml_generic.Make(Lwt_thread)
module Lwt_Query = Query.Make_with_Db(Lwt_thread)(Lwt_PGOCaml)
open Lwt

let get_db : unit -> unit Lwt_PGOCaml.t Lwt.t =
  let db_handler = ref None in
  fun () ->
    match !db_handler with
    | Some h -> Lwt.return h
    | None -> Lwt_PGOCaml.connect ~database:"macaque-demo" ()

let table = <:table< users (
  login text NOT NULL,
  password text NOT NULL
) >>

let find name =
  (get_db () >>= fun dbh ->
   Lwt_Query.view dbh
   <:view< {password = user_.password} |
            user_ in $table$;
            user_.login = $string:name$; >>)

let insert name pwd =
  (get_db () >>= fun dbh ->
  Lwt_Query.query dbh
  <:insert< $table$ :=
    { login = $string:name$; password = $string:pwd$; } >>)

let check_pwd name pwd =
  (get_db () >>= fun dbh ->
   Lwt_Query.view dbh
   <:view< {password = user_.password} |
            user_ in $table$;
            user_.login = $string:name$;
    user_.password = $string:pwd$ >>)
  >|= (function [] -> false | _ -> true)
