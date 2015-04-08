module U = Orec_univ_gadt
type 'a witness = 'a U.witness
type elt = U.binding

(* Phantom type brand for immutable and mutable field *)	       
type mut = Nil_mutable
type imm = Nil_immutable

(* Phantom type brand for const updater ( field ^= const ) and function updater 
field ^= f (field value)  *)	     
type lt_fn = Nil_lt_fn
type fn = lt_fn
type const = Nil_const

(* Phantom type brand for discriminating updater and getter *) 	       
type 'brand getter = Nil_getter
type  'kind updater = Nil_updater

(* Storage type: is the type 'ty stored directly or through a reference *)	     
type ('ty,'fy, 'brand) storage =
  | Imm : ('a,'a,imm) storage
  | Mut : ('a, 'a ref,mut) storage

(* Bijection auxiliary function and type *)	     
module Bijection = struct	     

type ('a,'b) bijection = { to_ : 'a -> 'b ; from : 'b -> 'a }

let flip iso = { to_ = iso.from; from = iso.to_ }

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
type ('ty,'tys, 'brand) key = { witness : 'tys witness; storage: ('ty,'tys,'brand) storage }

(* Namespace signature *)					  
module type Namespace_sig = sig
    include (module type of Bijection) 
    (** The type of record within the namespace *)
    type t



    (** The type of a field getter or updater *) 
    type ('info,'return_type) field_action

    (** Alias for the type of immutable fields *)
    type 'a field = (imm getter,'a option) field_action				
    type 'a mut_field = (mut getter,'a option) field_action	       

    (** The empty record *) 
    val empty : t
    (** Create a new open record from a list of field updater : [create [ field1 ^= value1; field2 ^= value2; ... ] ] *)
    val create : (const updater,t) field_action list -> t						    

    (** Creation of a new field *)
    val new_field : unit -> 'ty field 
    val new_field_mut : unit -> 'ty mut_field
								   
    (** Transform a field getter into a field updater *)
    val ( ^= ) : ( 'brand getter, 'ty option ) field_action -> 'ty -> ('a updater,t) field_action
    (** Field map: [ record.{field |= f } ] is equivalent to record.{ field ^= f record.{field} } *)
    val ( |= ) : ( 'brand getter, 'ty option ) field_action -> ('ty->'ty) -> (fn updater,t) field_action
    (** Copy a mutable field *)
    val copy : ( mut getter, 'ty option ) field_action -> (fn updater,t) field_action
										     

    (** field access : [ record.{field} ] returns the value of the field, [record.{field ^= value}] returns a functional update of record *)
    val get : t -> ('kind,'ret) field_action -> 'ret [@@indexop]
    (** The expressions record.{ field ^= value, field2 ^= value2, ...  } are shortcuts for record.{ field ^= value }.{ field2 ^= value2 }... *)
    val get_2 : t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> t [@@indexop]
    val get_3 : t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> t [@@indexop]
    val get_n : t -> (lt_fn updater,t) field_action array -> t [@@indexop]
    (** Setter for mutable field: [ orec.{field}<-x ] *)
    val set :  t -> ( mut getter , 'ty option ) field_action -> 'ty -> unit [@@indexop]

    (** Use the type equality implied by the bijection 'a<->'b to create a new ['b] field getter from a ['a] field getter *)
    val transmute :  ('brand getter, 'a option) field_action -> ('a,'b) bijection -> ( 'brand getter, 'b option) field_action
    (** Operator version of [transmute] *)
    val ( @: ) :    ( 'brand getter, 'ty option ) field_action -> ('ty,'vty) bijection ->  ( 'brand getter, 'vty option) field_action 

    val map : ('ty -> 'ty)  -> ( 'brand getter, 'ty option) field_action -> t -> t   end

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

    let find_exn key orec =  M.find (U.id key) orec |> U.extract_exn key

    let find key orec = match find_exn key orec with
      | x -> Some x
      | exception Not_found -> None

    let add key val_ orec = M.add (U.id key) (U.B (key,val_) ) orec
 			 
    (* Field action : either  getter or setter associated to a given key  *) 	     
    type ('info , 'ret)  field_action =
      | Get : ('ty,'tys,'brand) key -> (  'brand getter, 'ty option ) field_action
      | Indirect_get : ('ty,'tys,'brand) key * ('ty, 'vty) bijection -> (  'brand getter, 'vty option) field_action   
      | Update : ('ty,'tys,'brand) key * 'ty -> ('kind updater, t) field_action
      | Fn_update: ('ty,'tys,'brand) key * ('ty->'ty) -> (fn updater, t) field_action

    type 'ty field = (imm getter,'ty option) field_action
    type 'ty mut_field = (mut getter, 'ty option) field_action

    let (^=) : type ty brand ret . ( brand getter, ty option ) field_action -> ty -> ('a updater , t ) field_action =
	fun field_action x -> match field_action with
			      | Get key -> Update(key,x)
			      | Indirect_get (key,bij) -> Update(key, bij.from x)   
								
    let ( |= ) : type ty brand ret . ( brand getter, ty option ) field_action -> (ty->ty) -> ('a updater , t ) field_action =
	fun field_action f -> match field_action with
			      | Get key -> Fn_update(key,f)
			      | Indirect_get (key,bij) -> Fn_update(key,fun x ->   x |> bij.to_ |> f |>  bij.from )   

    (* Perform a copy of a mutable field. Copying an immutable would be pointless *)
    let copy field = field |= (fun x -> x)
		
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
	     
    let find_key key orec = find key.witness orec |>? deref key.storage
    let add_key key val_ orec = add key.witness (ref_ key.storage val_) orec 

    let update_key key f orec =
      match find_key key orec with
      | Some x -> add_key key x orec
      | None -> orec
				
    let update :('a updater, t) field_action -> t -> t = fun field_action orec -> 				
      match field_action with
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
    
    let%indexop get: type kind ret. t -> (kind,ret) field_action -> ret = fun orec ->
      function
      | Get key ->  find_key key orec
      | Indirect_get (witness, bijection) -> find_key witness orec |>? bijection.to_
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
    and get_2: t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> t = fun orec field1 field2 ->
      orec |> update field1 |> update field2
    and get_3: t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action ->  t =
      fun orec field1 field2 field3 ->
      orec |> update field1 |> update field2 |> update field3
    and get_n: t -> (lt_fn updater, t ) field_action array -> t = fun orec arr ->
      Array.fold_left (fun orec x -> update x orec) orec arr
    and set : type ty. t -> ( mut getter , ty option ) field_action -> ty -> unit = fun orec field x ->
      match field with
      | Get {witness; storage=Mut } -> find witness orec >>? fun r -> r := x 
      | Indirect_get ( {witness;storage=Mut}, bijection ) -> find witness orec >>? fun r -> r  := bijection.from x
					 

							     

    let map f field orec = match orec.{field} with Some x -> orec.{field ^= f x} | None -> orec   
			 

    let transmute: type ty brand. ( brand getter, ty option ) field_action -> (ty,'vty) bijection -> ( brand getter, 'vty option) field_action =
	fun action_field bijection ->  match action_field with
				       | Get witness -> Indirect_get (witness,bijection)
				       | Indirect_get (witness, bijection' ) -> Indirect_get (witness, bijection <*> bijection' ) 

    let ( @: ) field  bijection = transmute field bijection

					
    let new_field_generic:  type ty tys brand .  (ty,tys,brand) storage ->  (brand getter, ty option) field_action  =
	fun storage -> 
	Get { witness = U.create () ; storage}

    let new_field ()= new_field_generic Imm
    let new_field_mut () = new_field_generic Mut
					      
    let create l = List.fold_left ( fun orec field_action -> orec.{field_action} ) empty l

  end
