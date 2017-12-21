
open Type_data

(** Key storage type *)
type ('ty, 'tys,'tya, 'brand) key


(** Open record namespace functor *)
module Namespace : functor () ->
  sig
    include Bijection.S
    (** The type of record within the namespace *)
    type t

    (** The type of a field getter or updater *)
    type ('info,'return_type) field_action

    (** Aliases for the type of fields *)
    type ('a,'mut,'res) get = ( ('a,'mut) getter, 'res) field_action
    type 'a field = ('a, imm, 'a option) get
    type 'a mut_field = ('a, mut, 'a option) get
    type 'a exn_field= ('a, imm, 'a) get
    type 'a exn_mut_field = ('a, mut, 'a) get

    type ('param,'t) update = ('param updater, 't) field_action

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

    (** Transform a field getter into a field updater *)
    (** Constant field updater:
        record.{ field ^= v } set the value of [field] to [v]
        and is equivalent to record.{ put field v } *)
    val put:
      ('ty,'brand,'ty_access ) get -> 'ty -> ('a const, t) update
    val ( ^= ):
     ('ty,'brand,'ty_access ) get -> 'ty -> ('a const, t) update

    (** Field map:
        [ record.{field |= f } ] or record.{ fmap field f } are equivalent to
        record.{ field ^= fmap f record.{field} } if the field exists, and do
        nothing otherwise
    *)
    val fmap:
      ('ty, 'brand, 'ty_access) get -> ('ty->'ty) -> ('a fn, t) update
    val ( |= ) :
      ('ty, 'brand, 'ty_access) get -> ('ty->'ty) -> ('a fn, t) update

    (** Copy a mutable field *)
    val copy: ('ty, mut, 'ty_access) get  -> ('a fn, t) update
    (** Delete a field, if the field does not exist, do nothing *)
    val delete: ('ty,'any, 'ty_acces) get -> ('a del, t) update

    (** getter, updater and setter for t *)
    val get: ('ty,'kind,'ret) get -> t -> 'ret
    val update: ( any, t) update -> t -> t
    val set: ('ty,mut,'ty_access) get -> 'ty -> t -> unit

    (** Operator version of get+update and set *)
    (** (.{} ) operator:
     - [ record.{field} ] returns the value of the field
     - [record.{field ^= value}] returns a functional update of record
     - [ record.{field |= f} is equivalent to record.{ field ^= f record.{field} }
     - [ record.{delete field} returns an updated version of record without this field  *)
    val (.%{}): t -> ('kind,'ret) field_action -> 'ret
    val (.%{}<-):  t -> ('ty,mut,'ty_access) get -> 'ty -> unit

    (** non-operator version of get,set and update *)
    val get: ('ty,'kind,'ret) get -> t -> 'ret
    val update: ( any , t) update -> t -> t
    val set: ('ty,mut, 'ty_access) get -> 'ty -> t -> unit

    (** Use the type equality implied by the bijection 'a<->'b to create
        a new ['b] field getter from a ['a] field getter.
        The new field getter uses option access *)
    val transmute :
      ('a, 'brand, 'a_access) get
      -> ('a,'b) bijection
      -> ('b,'brand,'b option) get
    (** Operator version of [transmute] *)
    val ( @: ) :
     ('a, 'brand, 'a_access) get
      -> ('a,'b) bijection
      -> ('b,'brand,'b option) get

    (** exception based version of transmute *)
    val transmute_exn:
     ('a, 'brand, 'a_access) get
      -> ('a,'b) bijection
      -> ('b,'brand,'b) get

      (** Operator version of [transmute_exn] *)
    val ( @:! ) :
     ('a, 'brand, 'a_access) get
      -> ('a,'b) bijection
      -> ('b,'brand,'b) get
  end
