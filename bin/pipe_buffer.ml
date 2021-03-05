open Core
open Async

type 'a t = {
  writers : 'a Pipe.Writer.t Bag.t
; contents : 'a Queue.t
; size : int
}

let create ~size =
  if size <= 0 then raise_s [%message "size must be positive" (size : int)];
  { writers = Bag.create (); contents = Queue.create (); size }

let get t =
  let r, w = Pipe.create () in
  Bag.add_unit t.writers w;
  (Queue.to_list t.contents, r)

let write t elt =
  Queue.enqueue t.contents elt;
  if Queue.length t.contents > t.size then ignore (Queue.dequeue_exn t.contents : _);
  Bag.to_list t.writers
  |> Deferred.List.iter ~how:`Parallel ~f:(fun w ->
      Pipe.write w elt)

let write_without_pushback t elt = don't_wait_for (write t elt)
