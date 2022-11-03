open Lwt.Syntax

module SubId = struct
  type t = int

  let compare = Int.compare
  let of_int n = n
end

module Subs = Map.Make (SubId)

type 'a next = 'a -> unit
type _ complete = unit -> unit
type 'a observer = { next : 'a next; complete : 'a complete }

type 'a t = {
  stream : 'a Lwt_stream.t;
  observers : 'a observer Subs.t ref;
  count : int ref;
}

type 'a subscription = { id : SubId.t; observable : 'a t }

(** [react obs] reactively pushes each stream value to observers. *)
let react obs =
  (* [map_observers k obs] applies [k] to each of the observers. *)
  let map_observers k { observers; _ } =
    !observers |> Subs.bindings |> List.map snd
    |> Lwt_list.map_s (fun obr -> Lwt.wrap (fun () -> k obr))
    |> Lwt.map ignore
  in

  (* [forward_next v obs] pushes [v] to each observer. *)
  let forward_next v = map_observers (fun obr -> obr.next v) in
  (* [forward_complete obs] pushes the completion signal to each observer. *)
  let forward_complete = map_observers (fun obr -> obr.complete ()) in

  (* [react_next obs] gets the next stream value and pushes it to each
     observer, returning [true] if the stream is not complete. *)
  let react_next ({ stream; _ } as obs) =
    let next = Lwt_stream.get stream in
    Lwt.try_bind
      (fun () -> next)
      (function
        | Some v ->
            let+ _ = obs |> forward_next v in
            true
        | None ->
            let+ _ = obs |> forward_complete in
            false)
      (fun _exn -> Lwt.return_true)
  in

  (* [loop obs] calls [react_next] until the stream is complete. *)
  let rec loop obs =
    let* continue = react_next obs in
    if continue then loop obs else Lwt.return_unit
  in
  loop obs

let of_stream stream =
  let obs = { stream; observers = ref Subs.empty; count = ref 0 } in
  let _ = react obs in
  obs

let create () =
  let stream, push = Lwt_stream.create () in
  let obs = of_stream stream in

  let next v = push (Some v) in
  let complete () = push None in
  (obs, next, complete)

let subscribe ({ stream; observers; count; _ } as obs) next complete =
  let obr = { next; complete } in
  let id = SubId.of_int !count in
  let sub = { id; observable = obs } in

  (* If the stream is already closed, don't actually subscribe. *)
  if Lwt_stream.is_closed stream then obr.complete ()
  else (
    observers := Subs.add sub.id obr !observers;
    incr count);

  sub

let unsubscribe { id; observable } =
  observable.observers := Subs.remove id !(observable.observers)

let pipe f { stream; _ } = stream |> f |> of_stream
