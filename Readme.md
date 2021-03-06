
Orec provides an implementation of "open records" as an interface over universal map.

To start using this library, the first step is to chose which namespace to use.
The default namespace can be used with

```Ocaml
(* Use the default namespace for record field *)
open Orec.Default
```

or it is possible to define a new namespace with

```OCaml
module N = Orec.Namespace.Make()
open N
```

We can then create dynamically a new field
```Ocaml
let age : int field = new_field ()
```
Field are immutable by default, but mutable fields are also available
```Ocaml
let position : float mut_field = new_field_mut ()
```

Creating a new record is as simple as
```Ocaml
let r = create [ age ^= 10; position ^= 100. ].
```

Fields can then be accessed using the `record.%{field}` syntax
```Ocaml
assert ( r.%{age} = Some 10 );;
```
Unfortunately, the fields presents in `r` are not tracked by the type system.
For a field of type `'a field`,  `record.%{field}` returns an `'a option `.

Functional update can be performed using `record.%{ field ^= value }`
```Ocaml
let r2 = r.%{age ^= 20}
assert ( r.%{age} = Some 10 && r2.%{age} = Some 20 &&
r2.%{position} = r.%{position} );;
```

Note that field can still be added after creation
```Ocaml
let tags : 'string list field = new_field()
let r = r.%{ tags ^= ["a tag"] }
```

Assignment for mutable field follows the natural syntax `r.%{field}<- value`
```Ocaml
let () = r.%{position} <- 20.
assert( r.%{position} = Some 20. && r2.%{position} = Some 20. );;
```

Mutable fields are shared during functional update. To avoid this behavior, copy the field
```Ocaml
let r3= r.%{copy position}
let () = r3.%{position} <- 10.
assert ( r2.%{position} <> Some 10. );;
```
It is also possible to delete a field,
```Ocaml
let r4= r.%{delete position}
assert ( r4.%{position} = None )
```
or apply a transformation to a field, if the field exists:
```Ocaml
let f age = age + 1
let r5= r.%{ fmap f age }
let r5= r.%{ age |= f }
assert ( r5.%{age} = Some 40 )
```
All these different field updates can be potentially mixed together with the
`&` operator
```Ocaml
let r6 = r.%{ age |= f & position ^= 5. & delete tags }
```

Given a bijection between type `'a` and `'b`, it is possible to transmute an `'a field` to an `'b field`
```Ocaml
let age_str: string field = transmute age {to_ = string_of_int; from = int_of_string }
assert (r.%{age_str} = "10" );;
```

In some cases, accessing fields through an option type might be unpractical,
for instance, external invariants might enforce that a given field is always present.
In these situations, it is possible to use exception based fields rather than
option based field
```Ocaml
let name : int exn_field = new_field_exn ()
let error : unit exn_field = new_field_exn ()
let no_one = create [ name ^= "Ulysse" ]
assert( no_one.name = "Ulysse" )
let raise_exception =
    try no_one.%{error} with Not_found -> ()
```
An exception-based field `'a exn_field` returns directly the core type `'a` of
the field if the field is present and will raise a `Not_found` exception otherwise.
The behavior of all functions and operators defined in `orec` is independent from
the access mode. In particular, the `fmap` function always update the record if and
only if the underlying field is present in the record.

It is possible to switch from an option-based field to an exception-based by using
the `tranmute_exn` function.
