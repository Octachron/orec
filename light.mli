
(** Type carrier for field creation *)
type 'a ft = T

(** Type brand for getter field *)
type mut = Nil_mutable
type imm = Nil_immutable

type 'brand getter = Nil_getter

(** Type brand for updater field *)	     
type lt_fn = Nil_lt_fn
type fn = lt_fn
type const = Nil_const
type 'kind updater = Nil_updater

(** Storage type function *)
type ('ty, 'fy, 'brand) storage =
    Imm : ('a, 'a, imm) storage
  | Mut : ('a, 'a ref, mut) storage

(** Bijection type and composition *)			    
module Bijection :
sig
  (** Bijection record *)
  type ('a, 'b) bijection = { to_ : 'a -> 'b; from : 'b -> 'a; }
  (** Bijection inversion *)
  val flip : ('a, 'b) bijection -> ('b, 'a) bijection
  (** Bijection composition *)
  val ( <*> ) :
      ('a, 'b) bijection -> ('c, 'a) bijection -> ('c, 'b) bijection
end
(** Key storage type *)
type ('ty, 'tys, 'brand) key = {
  witness : 'tys Univ_gadt.witness;
  storage : ('ty, 'tys, 'brand) storage;
}

(** Open record namespace functor *)
module type Namespace_sig =
  sig
    include (module type of Bijection) 
    (** The type of record within the namespace *)
    type t

    (** The type of a field getter or updater *) 
    type ('info,'return_type) field_action

    (** The empty record *) 
    val empty : t
    (** Create a new open record from a list of field updater : [create [ field1 ^= value1; field2 ^= value2; ... ] ] *)
    val create : (const updater,t) field_action list -> t						    
    (** Creation of a new field *)
    val new_field : 'ty ft -> ( imm getter, 'ty option ) field_action
    val new_field_mut : 'ty ft -> (mut getter,'ty option) field_action
								   
    (** Transform a field getter into a field updater *)
    val ( ^= ) : ( 'brand getter, 'ty option ) field_action -> 'ty -> ('const updater,t) field_action
    (** Field map: [ record.{field |= f } ] is equivalent to record.{ field ^= f record.{field} } *)
    val ( |= ) : ( 'brand getter, 'ty option ) field_action -> ('ty->'ty) -> (fn updater,t) field_action
    (** Copy a mutable field *)
    val copy : ( mut getter, 'ty option ) field_action -> (fn updater,t) field_action
										     

    (** field access : [ record.{field} ] returns the value of the field, [record.{field ^= value}] returns a functional update of record *)
    val get : t -> ('kind,'ret) field_action -> 'ret [@@indexop]
    (** The expressions record.{ field ^= value, field2 ^= value2, field3 |= f ...  } are shortcuts for record.{ field ^= value }.{ field2 ^= value2 }.{field3 |= f }... *)
    val get_2 : t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> t [@@indexop]
    val get_3 : t -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> (lt_fn updater,t) field_action -> t [@@indexop]
    val get_n : t -> (lt_fn updater,t) field_action array -> t [@@indexop]
    (** Setter for mutable field: [ orec.{field}<-x ] *)
    val set :  t -> ( mut getter , 'ty option ) field_action -> 'ty -> unit [@@indexop]

    (** Use the type equality implied by the bijection 'a<->'b to create a new ['b] field getter from a ['a] field getter *)
    val transmute :  ('brand getter, 'a option) field_action -> ('a,'b) bijection -> ( 'brand getter, 'b option) field_action
    (** Operator version of [transmute] *)
    val ( @: ) :    ( 'brand getter, 'ty option ) field_action -> ('ty,'vty) bijection ->  ( 'brand getter, 'vty option) field_action 

    val map : ('ty -> 'ty)  -> ( 'brand getter, 'ty option) field_action -> t -> t		
  end
module Namespace : functor () -> Namespace_sig
