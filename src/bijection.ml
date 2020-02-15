module type S = sig

  (** Bijection record *)
  type ('a, 'b)  bijection = { to_ : 'a -> 'b; from : 'b -> 'a; }

  (** Bijection inversion *)
  val flip : ('a, 'b)  bijection -> ('b, 'a)  bijection

  (** Bijection composition *)
  val ( % ) : ('a, 'b)  bijection -> ('c, 'a)  bijection -> ('c, 'b)  bijection
end

type ('a,'b)  bijection = { to_ : 'a -> 'b ; from : 'b -> 'a }

let flip iso = { to_ = iso.from; from = iso.to_ }

let ( % ) {to_; from} source =
  {
    to_ = (fun x -> to_ @@ source.to_ x) ;
    from = (fun x -> source.from @@ from x )
  }
