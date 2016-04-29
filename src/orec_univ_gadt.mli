(** Helper type *)
type 'a type_carrier = T

(** Unique type identifier with equality*)
type 'a witness

(** key type *)
type key

(** Compute the value-level identifier associated to a type-level identifier*)
val id : 'a witness -> key

(** Bind together an 'a type-identifier with an 'a value *)
type binding = B : 'a witness * 'a -> binding

(** Try to extract an 'a value from a binding using the given type identifier*)
val extract : 'a witness -> binding -> 'a option
val extract_exn : 'a witness -> binding -> 'a

(** Create a new type identifier*)
val create : unit -> 'a witness
