
type  (_,_) get_kind =
  | Opt : ('ty, 'ty option) get_kind
  | Exn : ('ty, 'ty) get_kind

type 'a succ = Nil_stop
		 
type ( 't, 'ty,'tyg, 'perm )  lens2 =
  | R :  ('t,'ty,'tyg) r_lens -> ( 't, 'ty, 'tyg, 'any succ succ ) lens2
  | RU :  ('t,'ty,'tyg) ru_lens -> ('t, 'ty, 'tyg, 'any succ ) lens2
  | RUW : ('t,'ty,'tyg) ruw_lens -> ('t, 'ty, 'tyg,  'any ) lens2
 and ('t,'ty,'tyg) r_lens = { kind: ('ty,'tyg) get_kind; get : 't -> 'tyg }
 and ('t,'ty,'tyg) ru_lens = { kind: ('ty,'tyg) get_kind; get : 't -> 'tyg; update: 't -> 'ty -> 't }
 and ('t,'ty,'tyg) ruw_lens = { kind: ('ty,'tyg) get_kind; get : 't -> 'tyg; update: 't -> 'ty -> 't; write: 't -> 'ty -> unit }
		    
type ('t,'ty,'get_ret ) lens = {
  set : 'ty -> 't  -> 't; 
  get : 't -> 'get_ret
}

type 'ty getter = Nil_getter
type setter = Nil_setter
		  
type ('t, 'ty, 'res) field =
  | Get:  ('t,'ty,'get_ret) lens -> ('t, 'ty getter, 'get_ret ) field
  | Set: ('t,'ty,'get_ret) lens * 'ty ->  ('t, setter, 't) field   			

let (^=) : type t ty get_ret . (t, ty getter,get_ret) field -> ty -> (t, setter , t ) field = fun (Get lens) x ->
 Set(lens,x)
						  
let%indexop get: type t ty ret. t -> (t, ty,ret) field -> ret = fun orec ->  function
  | Get lens -> lens.get orec
  | Set (lens,x) -> lens.set x orec
and get_2: 't -> ('t,setter,'t) field -> ('t,setter,'t) field -> 't = fun orec (Set(lens,x)) (Set(lens',y)) ->
  orec |> lens.set x |> lens'.set y
and get_3: 't -> ('t,setter,'t) field -> ('t,setter,'t) field -> ('t,setter,'t) field ->  't = fun orec (Set(lens,x)) (Set(lens',y)) (Set(lens'',z)) ->
  orec |> lens.set x |> lens'.set y |> lens''.set z
and get_n: 't -> ('t,setter,'t) field array -> 't = fun orec arr ->
  Array.fold_left (fun orec ( Set(lens,x) ) -> lens.set x orec ) orec arr 
			     
let ( >>? ) x f = match x with None -> None | Some x -> Some (f x)
let ( **? ) x y=  match x,y with
		| Some x, Some y -> Some (x,y)
		| _ -> None
							     

let map field f orec = match orec.{field} with Some x -> orec.{field ^= f x} | None -> orec   
			 
type ('a,'b) bijection = { to_ : 'a -> 'b ; from : 'b -> 'a }
let flip iso = { to_ = iso.from; from = iso.to_ }

let transmute field {to_;from}  = Get {
  get = ( fun orec ->  orec.{field}  >>? to_ ) ;
  set = ( fun orec y -> orec.{ field ^= y } )
}

let ( @< ) iso field = transmute field iso

let zip field1 field2  = Get {
  get = ( fun orec -> orec.{field1}  **? orec.{field2} );
  set = (fun orec (x,y) -> orec.{ field1 ^= x, field2 ^= y } ) 
}

let ( *: ) field1 field2 = zip 


module type Type = sig type t end

module type IdS = 
  sig
    type t
    val empty:t
    val new_field :  ('ty,'get_ty) get_kind -> (module Type with type t = 'ty) -> (t, 'ty getter, 'get_ty ) field 
  end
    
module Id() : IdS = struct  
  let id  = ref 0
  type id  = Nil_id
		
  module Ordered= struct
    type t=int
    let compare: int -> int -> int= compare
  end
		    
  module M = Map.Make(Ordered)
  type t=Univ.t M.t
  let empty : t =M.empty


  let choose: type ty get_ty . ('t-> ty) -> ('t -> ty option) -> (ty,get_ty) get_kind -> (t -> get_ty) =
      fun get_exn get_opt ->
      function
      | Opt -> get_opt
      | Exn -> get_exn
		   
		       
  let new_field : type ty get_ty .  (ty,get_ty) get_kind -> (module Type with type t=ty)  ->  (t,ty getter, get_ty ) field =
      fun get_kind (module T) ->
      incr id;    
      let id = !id in
      let inj, proj  = Univ.specialize() in 
      let set value orec = M.add id (inj value) orec in
      let get_exn orec = proj @@ M.find id orec in
      let get_opt orec  =  try Some( get_exn orec ) with Not_found -> None in
      let get = choose get_exn get_opt get_kind in
      Get {set;get}
end


		 
		 
