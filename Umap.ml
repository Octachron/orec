
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

let id =ref 0
type key=int
type elt =  Universal.t





module Ordered= struct
    type t=key
    let compare l r=match (l - r) with
      | 0 -> 0
      | x when x>0 -> 1
      | _ -> -1
end
module M = Map.Make(Ordered)
type t= elt M.t
let empty=M.empty

module type PropertySig=sig 
	type r
	val s:r-> elt M.t -> elt M.t
	val set:elt M.t -> r -> elt M.t
	val get: elt M.t -> r
end

module type Typed = sig type t end

let property (type r) (module T:Typed with type t=r)=
  (module struct
    type r=T.t
    let id= incr id; !id
    let inj, proj = Universal.specialize()  
    let s v m= M.add id (inj v) m
    let set m v = s v m
    let get m = proj @@ M.find id m     
   end : PropertySig with type r=r)



