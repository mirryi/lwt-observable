(** Reactive streams. *)

type 'a t
(** The type of a reactive stream. *)

(** {2 Construction} *)

val of_stream : 'a Lwt_stream.t -> 'a t
(** [of_stream stream] creates an observable from [stream]. This is destructive on [stream]. *)

val create : unit -> 'a t * ('a -> unit) * (unit -> unit)
(** [create ()] returns a new observable, a push function, and a complete function. *)

(** {2 Subscribing} *)

type 'a next = 'a -> unit
(** The type of a subscription callback. *)

type _ complete = unit -> unit
(** The type of a subscription completion callback. *)

type 'a subscription
(** The type of a subscription. *)

val subscribe : 'a t -> 'a next -> 'a complete -> 'a subscription
(** [subscribe obs next complete] adds a new subscription to [obs] with the
    callbacks [next] and [complete], returning a {!subscription}.
    {!unsubscribe} may be used remove the subscription.

    If the observable is already complete, a completion signal is forward to
    the observer. *)

val unsubscribe : 'a subscription -> unit
(** [unsubscribe sub] removes the given subscription. This function is idempotent. *)

(** {2 Transformation} *)

val pipe : ('a Lwt_stream.t -> 'b Lwt_stream.t) -> 'a t -> 'b t
(** [pipe f obs] applies [f] to the stream of [obs]. This is destructive on [obs]. *)
