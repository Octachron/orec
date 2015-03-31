type elt =  Univ.t
	      
module Id ()=struct
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
    val get_e: t -> p 
    val get : t -> p option
    val map : (p->'a) -> t -> 'a option
    val get_d : p -> t -> p 
  end 
    
module Property(T:Base)=
  struct
    type p=T.p
    let id= incr id; !id
    let (inj, proj) : (p -> Univ.t) * (Univ.t->p) = Univ.specialize()  
    let s value orec= M.add id (inj value) orec
    let set orec value = s value orec
    let get_e orec = proj @@ M.find id orec
    let get orec = try Some( get_e orec ) with Not_found -> None
    let map f orec = match get orec with None -> None | Some x -> Some (f x)
    let get_d default orec = try get_e orec with Not_found -> default      
  end
    
    
    
end
	       
	       
	     
		 
module NameOrdered= struct
  type t=string
  let compare sl sr= compare sl sr
end
module NameMap=Map.Make(NameOrdered)
		       
		       
module Named ()=struct
  
  module type Base= 
    sig	
      val name : string
      type p
    end
      
  module N=NameMap
  type t=elt N.t
  let empty=N.empty 
	      
	      
  module type Sig=
    sig
      include(Base)
      val s:p-> t -> t
      val set:t -> p -> t
      val get_e: t -> p 
      val get: t -> p option
      val map: (p->'a) -> t -> 'a option
      val get_d: p -> t -> p 
    end 
      
  module Property(T:Base)=struct
    include(T)
    let (inj, proj) : (p->Univ.t) * (Univ.t -> p) = Univ.specialize()  
    let s value orec= N.add name (inj value) orec
    let set orec value = s value orec
    let get_e orec = proj @@ N.find name orec
    let get orec = try Some( get_e orec ) with Not_found -> None
    let map f orec = match get orec with None -> None | Some x -> Some (f x)
    let get_d default orec = try get_e orec with Not_found -> default 
  end
end

module Repr ()=struct
  module N=NameMap
  type t=elt N.t
  let empty=N.empty
	      
	      
  module type Base = sig 
      type p
      val name:string
      type rpr
      val repr : p -> rpr
      val specify: rpr -> p
    end
		       
		       
		       
  module type Sig=
    sig
      include(Base)
      val s:p-> t -> t
      val set:t -> p -> t
	val get_e: t -> p 
	val get : t -> p option
	val map : (p->'a) -> t -> 'a option
	val get_d : p -> t -> p 
	module Repr:sig
	  val s:rpr-> t -> t
	  val set:t -> rpr -> t
	  val get_e: t -> rpr 
	  val get : t -> rpr option
	  val map : (rpr->'a) -> t -> 'a option
	  val get_d : rpr -> t -> rpr 			
	end
    end 
      
      

  module Property(T:Base)=struct
    include T
    let inj, proj = Univ.specialize()  
    let s value orec= N.add name (inj value) orec
    let set orec value = s value orec
    let get orec = proj @@ N.find name orec
    let get_e orec = proj @@ N.find name orec
    let get orec = try Some( get_e orec ) with Not_found -> None
    let map f orec = match get orec with None -> None | Some x -> Some (f x)
    let get_d default orec = try get_e orec with Not_found -> default   
								
    module Repr=struct
      let s repr orec = N.add name (inj @@ specify repr) orec
      let set orec repr= s repr orec
      let get_e orec= repr @@ get_e orec
      let get orec = try Some( get_e orec ) with Not_found -> None
      let map f orec = match get orec with None -> None | Some x -> Some (f x)
      let get_d default orec = try get_e orec with Not_found -> default   
    end 
end
			    
end
		 
		 
		 



