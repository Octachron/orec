
exception Nothing
module type UnivS=sig
    type t
    val specialize: unit->('a->t)*(t->'a)
end

module Universal:UnivS=struct
  type t=exn
  let specialize (type u) ()=
    let module S=struct
    exception E of u
    let inj x = E x
    let proj = function E x -> x | _-> raise Nothing
    end
    in
S.(inj,proj)
end



type elt =  Universal.t

module Id=struct

module type Base = sig type p end


let id =ref 0
type key=int




module Ordered= struct
    type t=key
    let compare l r=match (l - r) with
      | 0 -> 0
      | x when x>0 -> 1
      | _ -> -1
end
module M = Map.Make(Ordered)
type t=elt M.t
let empty=M.empty 


module type Sig=
	sig		
		include(Base)
		val s:p-> t -> t
		val set:t -> p -> t
		val get: t -> p 
	end 

module Property(T:Base)=
struct
    type p=T.p
    let id= incr id; !id
    let inj, proj = Universal.specialize()  
    let s value orec= M.add id (inj value) orec
    let set orec value = s value orec
    let get orec = proj @@ M.find id orec     
end



end


module NameOrdered= struct
    type t=string
    let compare sl sr= compare sl sr
end
module NameMap=Map.Make(NameOrdered)


module Named=struct

module type Base= 
sig	
	val name : string
	include Id.Base
end

module N=NameMap
type t=elt N.t
let empty=N.empty 


module type Sig=
	sig
			include(Base)
			val s:p-> t -> t
			val set:t -> p -> t
			val get: t -> p 
	end 

module Property(T:Base)=struct
	include(T)
	let inj, proj = Universal.specialize()  
	let s value orec= N.add name (inj value) orec
	let set orec value = s value orec
	let get orec = proj @@ N.find name orec     
end
end

module Repr=struct


module N=NameMap
type t=elt N.t
let empty=N.empty


module type Base = sig 
	include(Named.Base)
	type rpr
	val repr : p -> rpr
	val specify: rpr -> p
end



module type Sig=
sig
	include(Base)
	val s:p-> t -> t
	val set:t -> p -> t
	val get: t -> p 
	module Repr:sig
		val s:rpr-> t -> t
		val set:t -> rpr -> t
		val get: t -> rpr 			
	end
end 

 

module Property(T:Base)=struct
	include T
	let inj, proj = Universal.specialize()  
	let s value orec= N.add name (inj value) orec
	let set orec value = s value orec
	let get orec = proj @@ N.find name orec     
	module Repr=struct
		let s repr orec = N.add name (inj @@ specify repr) orec
		let set orec repr= s repr orec
		let get orec= repr @@ get  orec  
	end 
end

end
