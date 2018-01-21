

(** Key storage type *)
type 'data key
  constraint 'data = <mut:'m; typ: 'ty; access:'tya; stored:'tys>


open Type_data

module Make:
  functor () ->
  sig
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

    (** non-operator version of get,set and update *)
    val get: < ret:'ret; .. > get -> t -> 'ret
    val update: ( any , t) update -> t -> t
    val set: <x:'ty; mut:mut; .. > get -> 'ty -> t -> unit

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
