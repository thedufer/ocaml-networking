open Core
open Async
open Sdn_local_protocol

let make_port_command ~summary ~param r_transform w_transform =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary
    [%map_open
      let id = anon ("NODE-ID" %: string)
      and port = anon ("PORT" %: int)
      and transform_config = param
      in fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (r, w, me, _) = Helper.connect (Node.Id.of_string id) in
        let (r, w) =
          ( r_transform ~me transform_config r
          , w_transform ~me transform_config w)
        in
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

let make_port_command_simple ~summary r_transform w_transform =
  make_port_command ~summary ~param:(Command.Param.return ())
    (fun ~me () -> r_transform)
    (fun ~me () -> w_transform)

let passthrough_command =
  make_port_command_simple ~summary:"stdout/stdin with no processing"
    Fn.id Fn.id

let layer_one_command =
  make_port_command_simple ~summary:"stdout/stdin via layer 1 frames"
    Layer_one.reader Layer_one.writer

let layer_two_command =
  make_port_command ~summary:"stdout/stdin via layer 2 frames"
    ~param:Command.Param.(anon ("ADDR" %: Address.arg_type))
    (fun ~me dest r ->
       Pipe.filter_map (Layer_two.reader r) ~f:(fun {msg; to_; from} ->
           (* I'm not sure if [Address.equal from dest] is the right thing to
           do. *)
           if Address.equal from dest && Address.equal to_ me then
             Some msg
           else
             None))
    (fun ~me dest w ->
       Pipe.create_writer (fun r ->
           Pipe.transfer r (Layer_two.writer w) ~f:(fun msg ->
               {Layer_two. msg; from = me; to_ = dest})))

let switch_command =
  let open Command.Let_syntax in
  Command.async_or_error' ~summary:"start a switch"
    [%map_open
      let id = anon ("NODE-ID" %: string)
      in fun () ->
        let open Deferred.Or_error.Let_syntax in
        let%bind (r, w, me, ports) = Helper.connect (Node.Id.of_string id) in
        let r = Layer_two.reader r in
        let w = Layer_two.writer w in
        let r =
          Pipe.map' r ~f:(fun q ->
              List.concat_map (Queue.to_list q) ~f:(fun msg ->
                  List.init ports ~f:(fun port ->
                      {msg with msg = {msg.msg with port}}))
              |> Queue.of_list
              |> Deferred.return)
        in
        Pipe.transfer_id r w
        |> Deferred.ok
    ]

let command =
  Command.group ~summary:"various client programs" [
    ("layer-one",   layer_one_command   );
    ("layer-two",   layer_two_command   );
    ("switch",      switch_command);
    ("passthrough", passthrough_command );
  ]

let () = Command.run command
