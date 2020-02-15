(** Bijection type and composition *)

module type S = sig

  (** Bijection record *)
  type ('a, 'b) bijection = { to_ : 'a -> 'b; from : 'b -> 'a; }

  (** Bijection inversion *)
  val flip : ('a, 'b)  bijection -> ('b, 'a)  bijection

  (** Bijection composition *)
  val ( % ) :
    ('a, 'b)  bijection -> ('c, 'a) bijection -> ('c, 'b)  bijection
end

include S
