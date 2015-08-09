(* Open type for unique type identifier *)
type _ type_id = ..
exception Type_non_equal

(* Module type for type-level identifier *)
module type Id = sig
    type t
    type _ type_id += Id : t type_id
  end

(* Helper type *)
type 'a type_carrier = T

(* Type witness for type equality *)
type 'a witness = ( module Id with type t = 'a )

type (_,_) eq = Proof : ('a,'a) eq

(* Test type equality and, if true, return a proof *)
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

(* Compute the term identifier associated to a type level identifier *)
let id (type a) (module M : Id with type t = a ) = Obj.extension_id M.Id (* Obj.extension? Brittle or not? *)

(* Binding: pair an 'a value with an 'a witness and hides the type 'a *)
type binding = B : 'a witness * 'a -> binding

(* extract back an 'a value from a binding, if the witness is equal to the bound witness *)
let extract: type a. a witness -> binding -> a option=  fun  witness (B (witness',x) ) ->
  match witness =? witness' with
  | Some Proof -> Some x
  | None -> None

(* Same thing with an exception rather than an option *)
let extract_exn: type a. a witness -> binding -> a =  fun  witness (B (witness',x) ) ->
  match witness =? witness' with
  | Some Proof -> x
  | None -> raise Type_non_equal

(* Create a new type witness *)
let create (type u) () : u witness  =
  let module K = struct
    type t = u
    type _ type_id += Id : t type_id
  end in
  (module K : Id with type t = u )
          
