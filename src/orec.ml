module U = Orec_univ_gadt
type 'a witness = 'a U.witness
type elt = U.binding

(* Phantom type brands for immutable and mutable field *)	       
type mut = Nil_mutable
type imm = Nil_immutable

(* Phantom type brands for exn or option getter *)
type opt = Nil_opt
type exn_ = Nil_exn
	     
(* Phantom type brand for const updater ( field ^= const ), function updater 
field ^= f (field value) and del updater  *)	     
type top = Nil_top
type only = Nil_bottom
type ('a,'b,'c) lattice_point = 'a*'b*'c
type 'a fn = top * 'a *'a 
type 'a const ='a * top *'a 
type 'a del = 'a *'a * top
type any =top*top*top
	     
(* Phantom type brand for discriminating updater and getter *) 	       
type ('core_type,'brand) getter = Nil_getter
type  +'kind updater = Nil_updater

(* Storage type: is the type 'ty stored directly or through a reference *)	     
type ('ty,'fy, 'brand) storage =
  | Imm : ('a,'a,imm) storage
  | Mut : ('a, 'a ref,mut) storage

(** Failure handling phantom type : either exception or option *)
type ('ty_arg,'ty_res ) access =
  | Opt : ('a,'a option) access
  | Exn : ('a,'a) access  

			   
(* Bijection auxiliary function and type *)	     
module Bijection = struct	     

type ('a,'b) bijection = { to_ : 'a -> 'b ; from : 'b -> 'a }

let flip_bij iso = { to_ = iso.from; from = iso.to_ }

let ( <*> ) {to_; from} source =
  {
    to_ = (fun x -> to_ @@ source.to_ x) ;
    from = (fun x -> source.from @@ from x )
  }
			   
end

(* Utility option monad functions *)			      
    let ( |>? ) x f = match x with
      | Some x -> Some ( f x )
      | None -> None

    let ( >>? ) x f = match x with
      | Some x -> f x 
      | None -> ()

 
(* Key type : 
 * 'ty the type of the key 
 * 'tys the type of the stored value 
 * 'brand : storage brand either imm or mut 
 *)  		     
type ('ty,'tys, 'tya, 'brand) key = { witness : 'tys witness; storage: ('ty,'tys,'brand) storage; access: ('ty,'tya) access }

(* Namespace signature *)					  
module type Namespace_sig = sig
    include (module type of Bijection) 
    (** The type of record within the namespace *)
    type t

    (** The type of a field getter or updater *) 
    type ('info,'return_type) field_action

    (** Alias for the type of immutable fields *)
    type 'a field = ( ('a,imm) getter, 'a option) field_action			     
    type 'a mut_field = ( ('a,mut) getter, 'a option) field_action
    type 'a exn_field= ( ('a,imm) getter, 'a ) field_action
    type 'a exn_mut_field = (('a,mut) getter, 'a) field_action

    (** The empty record *) 
    val empty : t
    (** Create a new open record from a list of field updater : [create [ field1 ^= value1; field2 ^= value2; ... ] ] *)
    val create : (only const updater,t) field_action list -> t						    

    (** Creation of a new field *)
    val new_field : unit -> 'ty field 
    val new_field_mut : unit -> 'ty mut_field
    val new_field_exn : unit -> 'ty exn_field 
    val new_field_exn_mut : unit -> 'ty exn_mut_field
								   
    (** Transform a field getter into a field updater *)
    val put :  ( ('ty,'brand) getter, 'ty_access ) field_action -> 'ty -> ('a const updater,t) field_action
    val ( ^= ) : ( ('ty,'brand) getter, 'ty_access ) field_action -> 'ty -> ('a const updater,t) field_action
										 
    (** Field fmap: [ record.{field |= f } ] is equivalent to record.{ field ^= fmap f record.{field} } *)
    val fmap : ( ('ty,'brand) getter, 'ty_access ) field_action -> ('ty->'ty) -> ('a fn updater,t) field_action 
						       
    val ( |= ) : ( ('ty,'brand) getter, 'ty_access ) field_action -> ('ty->'ty) -> ('a fn updater,t) field_action
										     
    (** Copy a mutable field *)
    val copy : ( ('ty,mut) getter, 'ty_access) field_action -> ('a fn updater,t) field_action
    val delete: ( ('ty,'any) getter, 'ty_acces ) field_action -> ('a del updater,t) field_action

    (** getter, updater and setter for t *)
    val get : (('ty,'kind) getter,'ret) field_action -> t -> 'ret
    val update: ( any updater, t ) field_action -> t -> t
    val set : ( ('ty,mut) getter , 'ty_access  ) field_action -> 'ty -> t -> unit  

    (** Operator version of get+update and set *)
    (** (.{} ) operator: 
     - [ record.{field} ] returns the value of the field
     - [record.{field ^= value}] returns a functional update of record 
     - [ record.{field |= f} is equivalent to record.{ field ^= f record.{field} }
     - [ record.{delete field} returns an updated version of record without this field  *)
    val get : t -> ('kind,'ret) field_action -> 'ret [@@indexop]
    (** The expressions record.{ field ^= value, field2 ^= value2, ...  } are shortcuts for record.{ field ^= value }.{ field2 ^= value2 }... *)
    val get_2 : t -> (any updater,t) field_action -> (any updater,t) field_action -> t [@@indexop]
    val get_3 : t -> (any updater,t) field_action -> (any updater,t) field_action -> (any updater,t) field_action -> t [@@indexop]
    val get_n : t -> (any updater,t) field_action array -> t [@@indexop]
    (** Setter for mutable field: [ orec.{field}<-x ] *)
    val set :  t -> ( ('ty,mut) getter , 'ty_access  ) field_action -> 'ty -> unit [@@indexop]

  
										   
    (** Use the type equality implied by the bijection 'a<->'b to create a new ['b] field getter from a ['a] field getter. The new field getter uses option access *)
    val transmute :  ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection -> ( ('b,'brand) getter, 'b option) field_action
    (** Operator version of [transmute] *)
    val ( @: ) :    ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection ->  ( ('b,'brand) getter, 'b option) field_action

    (** exception based version of transmute *)
    val transmute_exn :  ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection -> ( ('b,'brand) getter, 'b ) field_action
    (** Operator version of [transmute_exn] *)
    val ( @:! ) :    ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection ->  ( ('b,'brand) getter, 'b ) field_action
end
(* Namespace() generates a new module with abstract open record  *)		     
module Namespace() : Namespace_sig =
  struct
    (* Including bijection function to lighten use of the namespace *)
    include(Bijection)

    (* Underlying type of the open record *)
    module M= Map.Make(
		  struct
		    type t=int
		    let compare:int -> int -> int = compare
		  end)


    type t= elt M.t
    let empty : t = M.empty

    let find_exn witness orec =  M.find (U.id witness) orec |> U.extract_exn witness

    let find witness orec = match find_exn witness orec with
      | x -> Some x
      | exception Not_found -> None

    let find_gen: type ty tya. (ty,tya) access -> ty witness-> t -> tya  = fun access witness orec ->
      match access with
      | Exn -> find_exn witness orec
      | Opt -> find witness orec

    let add key val_ orec = M.add (U.id key) (U.B (key,val_) ) orec
    let delete_key key orec= M.remove (U.id key.witness) orec				  
 			 
    (* Field action : either  getter or updater associated to a given key  *) 	     
    type ('info , 'ret)  field_action =
      | Get: ('ty,'tys,'tya, 'brand ) key -> ( ('ty,'brand) getter, 'tya ) field_action
      | Indirect_get : ('ty,'tys,'tya,'brand) key * ('ty, 'ty2) bijection * ('ty2,'tya2) access -> ( ('ty2,'brand) getter, 'tya2 ) field_action   
      | Update: ('ty,'tys,'tya,'brand) key * 'ty -> ('a const updater, t) field_action
      | Fn_update: ('ty,'tys,'tya, 'brand) key * ('ty->'ty) -> ('a fn updater, t) field_action
      | Delete: ('ty,'tys,'tya,'brand) key -> ('a del updater, t) field_action 

    type 'ty field = ( ('ty,imm) getter,'ty option) field_action
    type 'ty mut_field = ( ('ty,mut) getter, 'ty option) field_action
    type 'a exn_field= ( ('a,imm) getter, 'a ) field_action
    type 'a exn_mut_field = (('a,mut) getter, 'a) field_action

    let put : type ty brand ret . ( (ty,brand) getter, ret ) field_action -> ty -> ('a const updater , t ) field_action =
	fun field_action x -> match field_action with
			      | Get key -> Update(key,x)
			      | Indirect_get (key,bij,access) -> Update(key, bij.from x)   

    let ( ^= ) field x = put field x
								       
    let fmap : type ty brand ret . ( (ty,brand) getter, ret ) field_action -> (ty->ty) -> ('a fn updater , t ) field_action =
	fun field_action f -> match field_action with
			      | Get key -> Fn_update(key,f)
			      | Indirect_get (key,bij,access) -> Fn_update(key,fun x ->   x |> bij.to_ |> f |>  bij.from )   

    let ( |= ) field f = fmap field f
									  
    (* Perform a copy of a mutable field. Copying an immutable would be pointless *)
    let copy field = field |= (fun x -> x)

    (* Delete a field *)
    let delete = function
      | Get key -> Delete key
      | Indirect_get (key,bij,access) -> Delete key 
		
    (* Convert from the stored type 'tys to the core type 'ty *)  
    let deref: type ty tys brand. (ty,tys,brand) storage -> tys -> ty = fun storage val_ ->
      match storage with
      | Mut -> !val_
      | Imm -> val_

    (* ref_ st · deref st = identity *)
    let ref_: type ty tys brand. (ty,tys,brand) storage -> ty -> tys = fun storage val_ ->
      match storage with
      | Mut -> ref val_
      | Imm -> val_

    let find_key_exn key orec = find_exn key.witness orec |> deref key.storage
		 
    let find_key: type ty tya. (ty,'tys,tya,'brand) key -> t -> tya  = fun key orec ->
      match key.access with
      | Opt ->
	 begin
	  try Some (find_key_exn key orec) with Not_found -> None
	end
      | Exn -> find_key_exn key orec

    let find_key_with: type ty2 tya2. (ty2,tya2) access -> ('ty,'tys,'tya,'brand) key -> ('ty->ty2) -> t -> tya2 = fun access key f orec ->
      match access with
      | Exn -> find_key_exn key orec |> f
      | Opt ->
	 begin
	   try Some(find_key_exn key orec |> f) with Not_found -> None 
	 end
			    

    let add_key key val_ orec = add key.witness (ref_ key.storage val_) orec 
  				    
    let update_key key f orec =
      match find_key_exn key orec with
      | x -> add_key key (f x) orec
      | exception Not_found -> orec

    (* get, update and set functions *)
    let get : ( ('ty,'brand) getter, 'tya ) field_action -> t -> 'tya = fun field orec ->
      match field with
      | Get key -> find_key key orec
      | Indirect_get (key, bijection,access) -> find_key_with access key bijection.to_ orec
				 
    let update :(any updater, t) field_action -> t -> t = fun field_action orec ->
      match field_action with
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | Delete key -> delete_key key orec

    let  set : type ty. ( (ty,mut) getter , 'ty_access ) field_action -> ty -> t -> unit = fun field x orec ->
      match field with
      | Get {witness; storage=Mut } -> (try find_exn witness orec := x with Not_found -> () ) 
      | Indirect_get ( {witness;storage=Mut}, bijection, access ) -> (try find_exn witness orec := bijection.from x with Not_found -> () )
					 
    
    
    let%indexop get: type kind ret. t -> (kind,ret) field_action -> ret = fun orec ->
      function
      | Get key ->  find_key key orec
      | Indirect_get (key, bijection,access) -> find_key_with access key bijection.to_ orec
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | Delete key -> delete_key key orec
    and get_2: t -> (any updater,t) field_action -> (any updater,t) field_action -> t = fun orec field1 field2 ->
      orec |> update field1 |> update field2
    and get_3: t -> (any updater,t) field_action -> (any updater,t) field_action -> (any updater,t) field_action ->  t =
      fun orec field1 field2 field3 ->
      orec |> update field1 |> update field2 |> update field3
    and get_n: t -> (any updater, t ) field_action array -> t = fun orec arr ->
      Array.fold_left (fun orec x -> update x orec) orec arr
    and set : type ty. t -> ( (ty,mut) getter , 'ty_access ) field_action -> ty -> unit = fun orec field x -> set field x orec
															  

    let transmute_gen: type ty brand. ('ty2,'ty2a) access -> ( (ty,brand) getter, 'ty_access ) field_action -> (ty,'ty2) bijection -> (('ty2, brand) getter, 'ty2a) field_action =
      fun access action_field bijection ->
      match action_field with
      | Get witness -> Indirect_get (witness,bijection,access)
      | Indirect_get (witness, bijection',_) -> Indirect_get (witness, bijection <*> bijection',access) 


    let transmute field bijection = transmute_gen Opt field bijection
    let ( @: ) field  bijection = transmute field bijection


    let transmute_exn field bijection = transmute_gen Exn field bijection
    let ( @:! ) field  bijection = transmute_exn field bijection
					    
					
    let new_field_generic(*:  type ty tys tya brand .  (ty,tys,brand) storage -> (ty,tya) ->  ( (ty,brand) getter, ty option) field_action *) =
	fun storage access-> 
	Get { witness = U.create () ; storage; access}

    let new_field ()= new_field_generic Imm Opt
    let new_field_mut () = new_field_generic Mut Opt
    let new_field_exn ()= new_field_generic Imm Exn
    let new_field_exn_mut () = new_field_generic Mut Exn
					     
    let create l = List.fold_left ( fun orec field_action -> orec.{field_action} ) empty l

  end
