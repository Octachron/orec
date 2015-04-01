module U = Univ_gadt
type 'a witness = 'a U.witness
type elt = U.binding
		     
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



type ('a,'b) bijection = { to_ : 'a -> 'b ; from : 'b -> 'a }
let flip iso = { to_ = iso.from; from = iso.to_ }

let ( <*> ) {to_; from} source =
  {
    to_ = (fun x -> to_ @@ source.to_ x) ;
    from = (fun x -> source.from @@ from x )
  }

type option_tyf = Opt_tyf
type id_tyf = Id_tyf
type ref_tyf = Ref_tyf
			   
type (_,_) tyf_call =
  | Opt: 'x -> ('x, option_tyf ) tyf_call
  | Nil: ('x, option_tyf) tyf_call 
  | Id : 'x -> ('x, id_tyf) tyf_call
  | Ref : 'x ref -> ('x, ref_tyf) tyf_call

type 'tyf constructor = { c: 'x. 'x -> ('x,'tyf) tyf_call } 
				    
type 'tyf access =
  | Opt : option_tyf access
  | Exn : id_tyf access

type 'tyf storage =
  | Imm : id_tyf storage
  | Mut : ref_tyf storage

type 'ty getter = Nil_getter
type  updater = Nil_updater

type ('ty,'tys,'tya) key = { witness : 'ty witness; storage: 'tys storage; access: 'tya access} 			 
		   
type ('kind, 'ret)  field_action =
  | Get : 'ty witness -> ( 'ty getter, 'ty option ) field_action
  | Indirect_get : 'ty witness * ('ty, 'vty) bijection -> ( 'vty getter, 'vty option) field_action   
  | Update : 'ty witness * 'ty -> (updater, t) field_action 			      

let (^=) : type ty ret . (ty getter, ret ) field_action -> ty -> ( updater , t ) field_action =
    fun field_action x -> match field_action with
			 | Get witness -> Update(witness,x)
			 | Indirect_get (witness,bij) -> Update(witness, bij.from x)   

let ( |>? ) x f = match x with
  | Some x -> Some ( f x )
  | None -> None
							       
       					  
let%indexop get: type kind ret. t -> (kind,ret) field_action -> ret = fun orec ->  function
  | Get witness -> find witness orec
  | Update (witness,x) -> add witness x orec
  | Indirect_get (witness, bijection) -> find witness orec |>? bijection.to_
and get_2: t -> (updater,t) field_action -> (updater,t) field_action -> t = fun orec (Update(witness,x)) (Update(witness',y)) ->
  orec |> add witness x |> add witness' y
and get_3: t -> (updater,t) field_action -> (updater,t) field_action -> (updater,t) field_action ->  t =
  fun orec (Update(witness,x)) (Update(witness',y)) (Update(witness'',z)) -> orec |> add witness x |> add witness' y |> add witness'' z
and get_n: t -> (updater, t ) field_action array -> t = fun orec arr ->
  Array.fold_left (fun orec ( Update(witness,x) ) -> add witness x orec ) orec arr 
					 
let ( **? ) x y=  match x,y with
		| Some x, Some y -> Some (x,y)
		| _ -> None
							     

let map field f orec = match orec.{field} with Some x -> orec.{field ^= f x} | None -> orec   
			 


let transmute: type ty ret. (ty getter, ret) field_action -> (ty,'vty) bijection -> ( 'vty getter, 'vty option) field_action =
    fun action_field bijection ->  match action_field with
				   | Get witness -> Indirect_get (witness,bijection)
				   | Indirect_get (witness, bijection' ) -> Indirect_get (witness, bijection <*> bijection' ) 

let ( @< )  bijection field = transmute field bijection

				     (*
let zip field1 field2  = Get {
  get = ( fun orec -> orec.{field1}  **? orec.{field2} );
  set = (fun orec (x,y) -> orec.{ field1 ^= x, field2 ^= y } ) 
}

let ( *: ) field1 field2 = zip 
 *)
