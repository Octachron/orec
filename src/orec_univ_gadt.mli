
type 'a type_carrier = T
type 'a witness
val id : 'a witness -> int
type binding = B : 'a witness * 'a -> binding 
val extract : 'a witness -> binding -> 'a option
val extract_exn : 'a witness -> binding -> 'a
val create : unit -> 'a witness
