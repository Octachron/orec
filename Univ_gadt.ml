type _ type_id = ..
exception Type_non_equal
		   
module type Type = sig type t end
	       
module type Id = sig
    type t
    type _ type_id += Id : t type_id
  end

type 'a witness = ( module Id with type t = 'a )

type (_,_) eq = Proof : ('a,'a) eq 

let ( =? ) (type u) (type v) : u witness -> v witness -> (u,v) eq option =
  fun (module U) (module V) ->
  match U.Id with
  | V.Id -> Some Proof
  | _ -> None
	   
let ( =! ) (type u) (type v) : u witness -> v witness -> (u,v) eq =
  fun (module U) (module V) ->
  match U.Id with
  | V.Id -> Proof
  | _ -> raise Type_non_equal 
	   

let id (type a) (module M : Id with type t = a ) = Obj.extension_id M.Id
	   
type binding = B : 'a witness * 'a -> binding

let extract: type a. a witness -> binding -> a option=  fun  witness (B (witness',x) ) ->
  match witness =? witness' with
  | Some Proof -> Some x
  | None -> None

let extract_exn: type a. a witness -> binding -> a =  fun  witness (B (witness',x) ) ->
  match witness =? witness' with
  | Some Proof -> x
  | None -> raise Type_non_equal 

	      
let create (type u) (module T : Type with type t = u ) : u witness  =
  let module K = struct
    type t = u
    type _ type_id += Id : t type_id
  end in
  (module K : Id with type t = u )
			    
