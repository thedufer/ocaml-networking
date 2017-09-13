open Core

let command =
  Command.group ~summary:"sdn-local admin controls" [
    ("client", Client.command);
    ("server", Server.command);
  ]

let () = Command.run command
