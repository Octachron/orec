

(** Type brand for getter field *)
type mut = Nil_mutable
type imm = Nil_immutable


(** Phantom type info carrier for updater and getter *)
type ('core_type,'brand) getter = Nil_getter
type  +'kind updater = Nil_updater


(** Phantom type brand for const updater ( field ^= const ), function updater
field |= f (field value) and delete updater  *)
type top = Nil_top
type only = Nil_bottom

type 'a fn = top * 'a *'a
type 'a const ='a * top *'a
type 'a del = 'a *'a * top

(** A type 'a [fn|const|del] can be unified to [any] or only [fn|const|del],
whereas a type only [fn|const|del] is fixed *)
type any =top*top*top


(** Storage type-level function *)
type ('ty, 'fy, 'brand) storage =
  | Imm: ('a, 'a, imm) storage
  | Mut: ('a, 'a ref, mut) storage

(** Failure handling phantom type : either exception or option *)
type ('ty_arg,'ty_res ) access =
  | Opt: ('a,'a option) access
  | Exn: ('a,'a) access


(** Bijection type and composition *)
module Bijection :
sig
  (** Bijection record *)
  type ('a, 'b) bijection = { to_ : 'a -> 'b; from : 'b -> 'a; }
  (** Bijection inversion *)
  val flip_bij : ('a, 'b) bijection -> ('b, 'a) bijection
  (** Bijection composition *)
  val ( <*> ) :
      ('a, 'b) bijection -> ('c, 'a) bijection -> ('c, 'b) bijection
end

(** Key storage type *)
type ('ty, 'tys,'tya, 'brand) key = {
  witness: 'tys Orec_univ_gadt.witness;
  storage: ('ty, 'tys, 'brand) storage;
  access: ('ty,'tya) access
}

(** Open record namespace functor *)
module Namespace : functor () ->
  sig
    include (module type of Bijection)
    (** The type of record within the namespace *)
    type t

    (** The type of a field getter or updater *)
    type ('info,'return_type) field_action

    (** Aliases for the type of fields *)
    type 'a field = ( ('a,imm) getter, 'a option) field_action
    type 'a mut_field = ( ('a,mut) getter, 'a option) field_action
    type 'a exn_field= ( ('a,imm) getter, 'a ) field_action
    type 'a exn_mut_field = (('a,mut) getter, 'a) field_action

    (** The empty record *)
    val empty: t
    (** Create a new open record from a list of field updater :
        [create [ field1 ^= value1; field2 ^= value2; ... ] ]
        Only const updater make sense in this context,
        since there is no fields present.
    *)
    val create: (only const updater,t) field_action list -> t
    (** Creation of a new fields.
        Note that the type 'ty would be  weakly polymorphic once the field created.
        However, in this specific use case, it seems reasonable to annotate the
        field type by using one of the field type aliases.
    *)
    val new_field: unit -> 'ty field
    val new_field_mut: unit -> 'ty mut_field
    val new_field_exn: unit -> 'ty exn_field
    val new_field_exn_mut: unit -> 'ty exn_mut_field

    (** Transform a field getter into a field updater *)
    (** Constant field updater:
        record.{ field ^= v } set the value of [field] to [v]
        and is equivalent to record.{ put field v } *)
    val put:  ( ('ty,'brand) getter, 'ty_access ) field_action -> 'ty -> ('a const updater,t) field_action
    val ( ^= ) : ( ('ty,'brand) getter, 'ty_access ) field_action -> 'ty -> ('a const updater,t) field_action

    (** Field map:
        [ record.{field |= f } ] or record.{ fmap field f } are equivalent to
        record.{ field ^= fmap f record.{field} } if the field exists, and do
        nothing otherwise
    *)
    val fmap: ( ('ty,'brand) getter, 'ty_access ) field_action -> ('ty->'ty) -> ('a fn updater,t) field_action
    val ( |= ) : ( ('ty,'brand) getter, 'ty_access ) field_action -> ('ty->'ty) -> ('a fn updater,t) field_action

    (** Copy a mutable field *)
    val copy: ( ('ty,mut) getter, 'ty_access) field_action -> ('a fn updater,t) field_action
    (** Delete a field, if the field does not exist, do nothing *)
    val delete: ( ('ty,'any) getter, 'ty_acces ) field_action -> ('a del updater,t) field_action

    (** getter, updater and setter for t *)
    val get: (('ty,'kind) getter,'ret) field_action -> t -> 'ret
    val update: ( any updater, t ) field_action -> t -> t
    val set: ( ('ty,mut) getter , 'ty_access  ) field_action -> 'ty -> t -> unit

    (** Operator version of get+update and set *)
    (** (.{} ) operator:
     - [ record.{field} ] returns the value of the field
     - [record.{field ^= value}] returns a functional update of record
     - [ record.{field |= f} is equivalent to record.{ field ^= f record.{field} }
     - [ record.{delete field} returns an updated version of record without this field  *)
    val get: t -> ('kind,'ret) field_action -> 'ret [@@indexop]
    (** The expressions record.{ field ^= value, field2 ^= value2, ...  } are shortcuts for record.{ field ^= value }.{ field2 ^= value2 }... *)
    val get_2: t -> (any updater,t) field_action -> (any updater,t) field_action -> t [@@indexop]
    val get_3: t -> (any updater,t) field_action -> (any updater,t) field_action -> (any updater,t) field_action -> t [@@indexop]
    val get_n: t -> (any updater,t) field_action array -> t [@@indexop]
    (** Setter for mutable field: [ orec.{field}<-x ] *)
    val set:  t -> ( ('ty,mut) getter , 'ty_access  ) field_action -> 'ty -> unit [@@indexop]

    (** non-operator version of get,set and update *)
    val get: ( ('ty,'kind) getter,'ret) field_action -> t -> 'ret
    val update: ( any updater, t ) field_action -> t -> t
    val set: ( ('ty,mut) getter , 'ty_access  ) field_action -> 'ty -> t -> unit

    (** Use the type equality implied by the bijection 'a<->'b to create
        a new ['b] field getter from a ['a] field getter.
        The new field getter uses option access *)
    val transmute :  ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection -> ( ('b,'brand) getter, 'b option) field_action
    (** Operator version of [transmute] *)
    val ( @: ) :    ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection ->  ( ('b,'brand) getter, 'b option) field_action

    (** exception based version of transmute *)
    val transmute_exn:  ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection -> ( ('b,'brand) getter, 'b ) field_action
    (** Operator version of [transmute_exn] *)
    val ( @:! ) :    ( ('a,'brand) getter, 'a_access ) field_action -> ('a,'b) bijection ->  ( ('b,'brand) getter, 'b ) field_action
  end
