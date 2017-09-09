open Core
open Async
open Protocol

let listen_command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"listen node"
    [%map_open
      let id = anon ("NODE-ID" %: string)
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (r, _w) = Helper.connect (Node.Id.of_string id) in
        Pipe.iter r ~f:(fun {port; data} ->
            printf !"%d: %{sexp: char list}\n" port data;
            Deferred.unit)
        |> Deferred.ok
    ]

let yes_command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"yes node"
    [%map_open
       let id = anon ("NODE-ID" %: string)
       and port = anon ("PORT" %: int)
       in
       fun () ->
         let open Deferred.Or_error.Let_syntax in
         let%bind (_r, w) = Helper.connect (Node.Id.of_string id) in
         Clock.every' (Time.Span.of_string "1s") (fun () ->
             Pipe.write w {Message. port; data = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j']});
         Deferred.never ()
     ]

let command =
  Command.group ~summary:"various client programs" [
    ("listen", listen_command );
    ("yes",    yes_command    );
  ]

let () = Command.run command
