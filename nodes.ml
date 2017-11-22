open Core
open Async
open Sdn_local_protocol

let make_port_command ~summary r_transform w_transform =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary
    [%map_open
      let id = anon ("NODE-ID" %: string)
      and port = anon ("PORT" %: int)
      in
      fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (r, w, _) = Helper.connect (Node.Id.of_string id) in
        let (r, w) = (r_transform r, w_transform w) in
        let r =
          Pipe.filter_map r ~f:(fun {Message. port = port'; data} ->
              if port = port'
              then Some (String.of_char_list data)
              else None)
        in
        let stdin =
          Reader.lines (Lazy.force Reader.stdin)
          |> Pipe.map ~f:(fun str ->
              {Message. port; data = String.to_list (str ^ "\n")})
        in
        let stdout =
          Writer.pipe (Lazy.force Writer.stdout)
        in
        don't_wait_for (Pipe.transfer_id stdin w);
        don't_wait_for (Pipe.transfer_id r stdout);
        Deferred.never ()
    ]

let passthrough_command =
  make_port_command ~summary:"stdout/stdin with no processing" Fn.id Fn.id

let layer_one_command =
  make_port_command ~summary:"stdout/stdin via layer 1 frames"
    Layer_one.reader Layer_one.writer

let command =
  Command.group ~summary:"various client programs" [
    ("layer-one",  layer_one_command    );
    ("passthrough", passthrough_command );
  ]

let () = Command.run command
