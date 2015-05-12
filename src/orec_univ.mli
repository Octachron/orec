type t = ..
exception Nothing
val specialize : unit -> ('a -> t) * (t -> 'a)
