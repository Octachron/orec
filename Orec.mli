

module Id:sig

	module type Base=sig type p end
	type t
	val empty:t

	module type Sig=
	sig		
		include (Base)
		val s:p-> t -> t
		val set:t -> p -> t
		val get: t -> p 
	end 
	module Property(S:Base):(Sig with type p=S.p)
end


module Named : sig

	module type Base=sig type p val name:string end
	type t
	val empty:t

	module type Sig=
	sig
			include (Base)
			val s:p-> t -> t
			val set:t -> p -> t
			val get: t -> p 
	end 

	module Property(S:Base):(Sig with type p=S.p)
end


module Repr : sig

	module type Base=sig
		val name:string
		type p 
		type rpr
		val repr : p -> rpr
		val specify: rpr -> p
	end

	type t
	val empty:t

	module type Sig=
	sig
		include (Base)
		val s:p-> t -> t
		val set:t -> p -> t
		val get: t -> p 
		module Repr:sig
			val s:rpr-> t -> t
			val set:t -> rpr -> t
			val get: t -> rpr 			
		end
	end 

	module Property(S:Base):(Sig with type p=S.p and type rpr=S.rpr)
end

