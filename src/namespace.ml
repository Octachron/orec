
module U = Univ
type 'a witness = 'a U.witness
type elt = U.binding

(* Key type :
 * 'ty the type of the key
 * 'tys the type of the stored value
 * 'mut : storage brand either imm or mut
*)

open Type_data
type 'data key = {
  witness : 'tys witness;
  storage: ('ty,'tys,'m) storage;
  access: ('ty,'tya) access
}
  constraint 'data = <mut:'m; typ: 'ty; access:'tya; stored:'tys>

module type S = sig
  open Type_data
    include Bijection.S

    (** The type of record within the namespace *)
    type t

    (** The type of a field getter or updater *)
    type 'info field_action

    (** Aliases for the type of fields *)
    type 'info get = ( ('a,'mut) getter * 'res) field_action
        constraint 'info = <x:'a; mut:'mut; ret:'res>
    type 'a field =  <x:'a; mut:imm; ret:'a option> get
    type 'a mut_field =  <x:'a; mut:mut; ret:'a option> get
    type 'a exn_field=  <x:'a; mut:imm; ret:'a> get
    type 'a exn_mut_field =  <x:'a; mut:mut; ret:'a> get

    type ('param,'t) update = ('param updater * 't) field_action

    (** The empty record *)
    val empty: t

    (** Create a new open record from a list of field updater :
        [create [ field1 ^= value1; field2 ^= value2; ... ] ]
        Only const updater make sense in this context,
        since there is no fields present.
    *)
    val create: (only const, t) update list -> t

    (** Creation of a new fields.
        Note that the type 'ty would be  weakly polymorphic once the field created.
        However, in this specific use case, it seems reasonable to annotate the
        field type by using one of the field type aliases.
    *)
    val new_field: unit -> 'ty field
    val new_field_mut: unit -> 'ty mut_field
    val new_field_exn: unit -> 'ty exn_field
    val new_field_exn_mut: unit -> 'ty exn_mut_field

    (** Constant field updater:
        [record.{ field ^= v }] sets the value of [field] to [v]
        and is equivalent to [record.{ put field v }] *)
    val put:
      <x:'ty; .. > get -> 'ty -> (_ const, t) update
    val ( ^= ):
     <x:'ty; .. > get -> 'ty -> (_ const, t) update

    (** Field map:
        [ record.{field |= f } ] or [record.{ fmap field f }] are equivalent to
        [record.{ field ^= fmap f record.{field} }] if the field exists, and do
        nothing otherwise
    *)
    val fmap:
      <x:'ty; .. > get -> ('ty->'ty) -> ('a fn, t) update
    val ( |= ) :
      <x:'ty; .. > get -> ('ty->'ty) -> ('a fn, t) update


    (** Field combinator
        [ orec.%{ x & y }] is [ orec.%{x}.%{y}]
    *)

    val (&): (any, t) update -> (any, t) update -> (any, t) update
    val and_then: (any, t) update -> (any, t) update -> (any, t) update

    (** Copy a mutable field *)
    val copy: <x:'ty; mut:mut; .. > get  -> ('a fn, t) update

    (** Delete a field, if the field does not exist, do nothing *)
    val delete: < .. > get -> ('a del, t) update

    (** getter, updater and setter for t *)
    val get: < ret:'ret; .. > get -> t -> 'ret
    val update: ( any, t) update -> t -> t
    val set: <x:'ty; mut:mut; .. > get -> 'ty -> t -> unit

    (** Operator version of get+update and set *)

    (** [(.%{} )] operator:
     - [ record.%{field} ] returns the value of the field
     - [record.%{field ^= value}] returns a functional update of record
     - [ record.%{field |= f} ] is equivalent to
        [ record.{ field ^= f record.{field} } ]
     - [ record.%{delete field}] returns an updated version of record
        without this field  *)
    val (.%{}): t -> (_ * 'ret) field_action -> 'ret
    val (.%{}<-):  t -> < x:'ty; mut:mut; .. > get -> 'ty -> unit

    (** Use the type equality implied by the bijection ['a⟺'b] to create
        a new ['b] field getter from a ['a] field getter.
        The new field getter uses option access *)
    val transmute :
      (< x:'a; mut:'m; ..> as 'x)  get
      -> ('a,'b) bijection
      -> < x:'b; mut:'m; ret:'b option > get

    (** Operator version of [transmute] *)
    val ( @: ) :
      (< x:'a; mut:'m; ..> as 'x)  get
      -> ('a,'b) bijection
      -> < x:'b; mut:'m; ret:'b option > get

    (** exception based version of transmute *)
    val transmute_exn:
      (< x:'a; mut:'m; ..> as 'x)  get
      -> ('a,'b) bijection
      -> < x:'b; mut:'m; ret:'b> get

    (** Operator version of [transmute_exn] *)
    val ( @:! ) :
      (< x:'a; mut:'m; ..> as 'x)  get
      -> ('a,'b) bijection
      -> < x:'b; mut:'m; ret:'b> get

  end



(* Namespace() generates a new module with abstract open record  *)
module Make(): S =
  struct
    (* Including bijection function to lighten use of the namespace *)
    include(Bijection)

    (* Underlying type of the open record *)
    module M= Map.Make(
          struct
            type t=U.key
            let compare:U.key-> U.key-> int = compare
          end)


    (** The type of record within the namespace *)
    type t= elt M.t

    (** The empty record *)
    let empty : t = M.empty

    let find_exn witness orec =
      M.find (U.id witness) orec |> U.extract_exn witness

    let add key val_ orec = M.add (U.id key) (U.B (key,val_) ) orec
    let delete_key key orec= M.remove (U.id key.witness) orec

    (* Field action : either  getter or updater associated to a given key  *)
    type 'info field_action =
      | Get:
          <typ: 'ty; access:'tya; mut:'m; .. > key ->
        ( ('ty,'m) getter * 'tya ) field_action
      | Indirect_get :
           <typ: 'ty; mut:'m;.. > key * ('ty, 'ty2) bijection * ('ty2,'tya2) access
        -> ( ('ty2,'m) getter * 'tya2 ) field_action
      | Update:
          <typ: 'ty; .. > key * 'ty -> ('a const updater * t) field_action
      | Fn_update:
          <typ: 'ty; .. > key * ('ty->'ty) -> ('a fn updater * t) field_action
      | And :
          ('any updater * t) field_action * ('any updater * t) field_action ->
        ('any updater * t) field_action
      | Delete:
          < .. > key -> ('a del updater * t) field_action


    (** Alias for the type of fields *)
    type 'info get = ( ('a,'mut) getter * 'res) field_action
      constraint 'info = <x:'a; mut:'mut; ret:'res>
    type 'a field =  <x:'a; mut:imm; ret:'a option> get
    type 'a mut_field =  <x:'a; mut:mut; ret:'a option> get
    type 'a exn_field=  <x:'a; mut:imm; ret:'a> get
    type 'a exn_mut_field =  <x:'a; mut:mut; ret:'a> get
    type ('param,'t) update = ('param updater * 't) field_action

    (** Creation of a new field *)
    let new_field_generic =
      fun storage access->
    Get { witness = U.create () ; storage; access}

    let new_field ()= new_field_generic Imm Opt
    let new_field_mut () = new_field_generic Mut Opt
    let new_field_exn ()= new_field_generic Imm Exn
    let new_field_exn_mut () = new_field_generic Mut Exn

    (** Transform a field getter into a field updater *)
    let put : type ty m ret.
      <x:ty; mut:m; ret: ret> get -> ty -> ('a const, t) update =
    fun field_action x -> match field_action with
                  | Get key -> Update(key,x)
                  | Indirect_get (key,bij,_access) -> Update(key, bij.from x)

    let ( ^= ) field x = put field x

    (** Field fmap: [ record.{field |= f } ] is equivalent to
        [record.{ field ^= fmap f record.{field} }], if the field exists *)
    let fmap : type ty m ret.
       <x:ty; mut:m; ret: ret> get  -> (ty->ty) -> ('a fn,t) update =
    fun field_action f -> match field_action with
                  | Get key -> Fn_update(key,f)
                  | Indirect_get (key,bij,_access) ->
                    Fn_update(key,fun x ->   x |> bij.to_ |> f |>  bij.from )

    let ( |= ) field f = fmap field f

    (* Perform a copy of a mutable field. Copying an immutable would be pointless *)
    let copy field = field |= (fun x -> x)

    (* Delete a field *)
    let delete = function
      | Get key -> Delete key
      | Indirect_get (key,_bij,_access) -> Delete key

    (* Convert from the stored type 'tys to the core type 'ty *)
    let deref: type ty tys brand. (ty,tys,brand) storage -> tys -> ty =
      fun storage val_ ->
      match storage with
      | Mut -> !val_
      | Imm -> val_

    (* ref_ st · deref st = identity *)
    let ref_: type ty tys brand. (ty,tys,brand) storage -> ty -> tys =
      fun storage val_ ->
      match storage with
      | Mut -> ref val_
      | Imm -> val_

    let find_key_exn key orec = find_exn key.witness orec |> deref key.storage

    let find_key: type ty tya. <typ:ty; access:tya; .. > key -> t -> tya  =
      fun key orec -> match key.access with
      | Opt ->
     begin
      try Some (find_key_exn key orec) with Not_found -> None
    end
      | Exn -> find_key_exn key orec

    let find_key_with:
      type ty2 tya2. (ty2,tya2) access -> <typ:'ty; .. > key
      -> ('ty->ty2) -> t -> tya2 =
      fun access key f orec -> match access with
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
    let get : <ret:'tya; .. > get -> t -> 'tya = fun field orec ->
      match field with
      | Get key -> find_key key orec
      | Indirect_get (key, bijection,access) ->
        find_key_with access key bijection.to_ orec

    let rec update :(any, t) update -> t -> t = fun field_action orec ->
      match field_action with
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | Delete key -> delete_key key orec
      | And (l, r) ->
        update r (update l orec)

    let and_then l r = And(l,r)
    let (&) = and_then

    let  set : type ty r. <x:ty; mut:mut; ret:r > get -> ty -> t -> unit =
      fun field x orec ->
      match field with
      | Get {witness; storage=Mut; access=_ } ->
        (try find_exn witness orec := x with Not_found -> () )
      | Indirect_get({witness; storage=Mut; access=_ }, bijection, _access ) ->
        (try find_exn witness orec := bijection.from x with Not_found -> () )


    (** Operator version of get+update and set *)

    (** (.{} ) operator:
        - [ record.{field} ] returns the value of the field
        - [record.{field ^= value}] returns a functional update of record
        - [ record.{field |= f} is equivalent to record.{ field ^= f record.{field} }
        - [ record.{delete field} returns an updated version of record
        without this field  *)
    let rec (.%{}): type kind ret.
      t -> (kind * ret) field_action -> ret =
      fun orec ->
      function
      | Get key ->  find_key key orec
      | Indirect_get (key, bijection,access) ->
        find_key_with access key bijection.to_ orec
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | And(l,r) -> orec.%{l}.%{r}
      | Delete key -> delete_key key orec

    (** The expressions record.{ field ^= value, field2 ^= value2, ...  } are
        shortcuts for record.{ field ^= value }.{ field2 ^= value2 }... *)
    let (.%{}<-) : type ty.
      t -> <x:ty; mut:mut; ..> get -> ty -> unit =
      fun orec field x -> set field x orec

    (** Create a new open record from a list of field updater :
        [create [ field1 ^= value1; field2 ^= value2; ... ] ] *)
    let create l = List.fold_left (
        fun orec field_action -> orec.%{field_action} ) empty l

    (** Use the type equality implied by the bijection 'a<->'b to create a
        new ['b] field getter from a ['a] field getter. The new field getter uses
        the provided access type *)
    let transmute_gen: type ty.
      ('ty2,'ty2a) access ->
       <x:ty; mut:'mut; ..> get -> (ty,'ty2) bijection ->
        <x:'ty2; mut:'mut; ret: 'ty2a> get =
      fun access action_field bijection ->
      match action_field with
      | Get witness -> Indirect_get (witness,bijection,access)
      | Indirect_get (witness, bijection',_) ->
        Indirect_get (witness, bijection % bijection',access)

    let transmute field bijection = transmute_gen Opt field bijection
    let ( @: ) field  bijection = transmute field bijection

    let transmute_exn field bijection = transmute_gen Exn field bijection
    let ( @:! ) field  bijection = transmute_exn field bijection

  end
