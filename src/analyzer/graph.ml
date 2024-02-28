module type S =
  sig
    type t
    val pp : Format.formatter -> t -> unit
    val fold : (String.t -> String.t list -> 'b -> 'b) -> t -> 'b -> 'b
    val succ : Basicblock.t -> t -> Basicblock.t list
  end