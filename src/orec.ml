module U = Univ
type 'a witness = 'a U.witness
type elt = U.binding
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

open Type_data
type ('ty,'tys, 'tya, 'brand) key = {
  witness : 'tys witness;
  storage: ('ty,'tys,'brand) storage;
  access: ('ty,'tya) access
}

(* Namespace() generates a new module with abstract open record  *)
module Namespace() =
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

    let find witness orec = match find_exn witness orec with
      | x -> Some x
      | exception Not_found -> None

    (* find the value associated with the key witness,
       choose the error handling in function of the access argument *)
    let find_gen: type ty tya. (ty,tya) access -> ty witness-> t -> tya  =
      fun access witness orec ->
      match access with
      | Exn -> find_exn witness orec
      | Opt -> find witness orec

    let add key val_ orec = M.add (U.id key) (U.B (key,val_) ) orec
    let delete_key key orec= M.remove (U.id key.witness) orec

    (* Field action : either  getter or updater associated to a given key  *)
    type ('info , 'ret)  field_action =
      | Get:
          ('ty,'tys,'tya, 'brand ) key -> ( ('ty,'brand) getter, 'tya ) field_action
      | Indirect_get :
          ('ty,'tys,'tya,'brand) key * ('ty, 'ty2) bijection * ('ty2,'tya2) access
        -> ( ('ty2,'brand) getter, 'tya2 ) field_action
      | Update:
          ('ty,'tys,'tya,'brand) key * 'ty -> ('a const updater, t) field_action
      | Fn_update:
          ('ty,'tys,'tya, 'brand) key * ('ty->'ty) -> ('a fn updater, t) field_action
      | Delete:
          ('ty,'tys,'tya,'brand) key -> ('a del updater, t) field_action

    (** Alias for the type of fields *)
    type ('a,'mut,'res) get = ( ('a,'mut) getter, 'res) field_action
    type 'a field = ('a, imm, 'a option) get
    type 'a mut_field = ('a, mut, 'a option) get
    type 'a exn_field= ('a, imm, 'a) get
    type 'a exn_mut_field = ('a, mut, 'a) get

    type ('param,'t) update = ('param updater, 't) field_action

    (** Creation of a new field *)
    let new_field_generic =
      fun storage access->
    Get { witness = U.create () ; storage; access}

    let new_field ()= new_field_generic Imm Opt
    let new_field_mut () = new_field_generic Mut Opt
    let new_field_exn ()= new_field_generic Imm Exn
    let new_field_exn_mut () = new_field_generic Mut Exn

    (** Transform a field getter into a field updater *)
    let put : type ty brand ret. (ty,brand,ret) get -> ty -> ('a const, t) update =
    fun field_action x -> match field_action with
                  | Get key -> Update(key,x)
                  | Indirect_get (key,bij,access) -> Update(key, bij.from x)

    let ( ^= ) field x = put field x

    (** Field fmap: [ record.{field |= f } ] is equivalent to
        [record.{ field ^= fmap f record.{field} }], if the field exists *)
    let fmap : type ty brand ret.
      (ty,brand,ret) get -> (ty->ty) -> ('a fn,t) update =
    fun field_action f -> match field_action with
                  | Get key -> Fn_update(key,f)
                  | Indirect_get (key,bij,access) ->
                    Fn_update(key,fun x ->   x |> bij.to_ |> f |>  bij.from )

    let ( |= ) field f = fmap field f

    (* Perform a copy of a mutable field. Copying an immutable would be pointless *)
    let copy field = field |= (fun x -> x)

    (* Delete a field *)
    let delete = function
      | Get key -> Delete key
      | Indirect_get (key,bij,access) -> Delete key

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

    let find_key: type ty tya. (ty,'tys,tya,'brand) key -> t -> tya  =
      fun key orec -> match key.access with
      | Opt ->
     begin
      try Some (find_key_exn key orec) with Not_found -> None
    end
      | Exn -> find_key_exn key orec

    let find_key_with:
      type ty2 tya2. (ty2,tya2) access -> ('ty,'tys,'tya,'brand) key
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
    let get : ('ty,'brand,'tya) get -> t -> 'tya = fun field orec ->
      match field with
      | Get key -> find_key key orec
      | Indirect_get (key, bijection,access) ->
        find_key_with access key bijection.to_ orec

    let update :(any, t) update -> t -> t = fun field_action orec ->
      match field_action with
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | Delete key -> delete_key key orec

    let  set : type ty. (ty,mut,'ty_access) get -> ty -> t -> unit =
      fun field x orec ->
      match field with
      | Get {witness; storage=Mut } ->
        (try find_exn witness orec := x with Not_found -> () )
      | Indirect_get ( {witness;storage=Mut}, bijection, access ) ->
        (try find_exn witness orec := bijection.from x with Not_found -> () )


    (** Operator version of get+update and set *)
    (** (.{} ) operator:
        - [ record.{field} ] returns the value of the field
        - [record.{field ^= value}] returns a functional update of record
        - [ record.{field |= f} is equivalent to record.{ field ^= f record.{field} }
        - [ record.{delete field} returns an updated version of record
        without this field  *)
    let (.%{}): type kind ret. t -> (kind,ret) field_action -> ret = fun orec ->
      function
      | Get key ->  find_key key orec
      | Indirect_get (key, bijection,access) ->
        find_key_with access key bijection.to_ orec
      | Update (key,x) -> add_key key x orec
      | Fn_update(key,f) -> update_key key f orec
      | Delete key -> delete_key key orec
    (** The expressions record.{ field ^= value, field2 ^= value2, ...  } are
        shortcuts for record.{ field ^= value }.{ field2 ^= value2 }... *)
    let (.%{}<-) : type ty.
      t -> (ty,mut,'ty_access) get -> ty -> unit =
      fun orec field x -> set field x orec

    (** Create a new open record from a list of field updater :
        [create [ field1 ^= value1; field2 ^= value2; ... ] ] *)
    let create l = List.fold_left (
        fun orec field_action -> orec.%{field_action} ) empty l

    (** Use the type equality implied by the bijection 'a<->'b to create a
        new ['b] field getter from a ['a] field getter. The new field getter uses
        the provided access type *)
    let transmute_gen: type ty brand.
      ('ty2,'ty2a) access ->
      (ty,brand,'ty_access) get -> (ty,'ty2) bijection ->
      ('ty2, brand, 'ty2a) get =
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
