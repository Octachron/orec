type t

val empty:t

module type Typed= sig type t end

module type PropertySig=sig 
	type r
	val s:r-> t -> t
	val set:t -> r -> t
	val get: t -> r
end 

val property : (module Typed with type t = 'a) -> (module PropertySig with type r = 'a)
