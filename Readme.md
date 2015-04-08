
Orec provides an implementation of "open records" as an interface over universal map.
To start using this library first defines a namespace for the "open record" fields

```Ocaml
module M = Orec.Namespace () (* module M provides a namespace for record field *)
open M 
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
```
let r = create [ age ^= 10; position ^= 100. ]. 
```


Fields can then be accessed using the `record.{field}` syntax 
```Ocaml
assert ( r.{age} = Some 10 );;
```
Unfortunately, it is not possible to track the fields presents in r using the type system.
For a field of type `'a`,  `record.{field}` returns an `'a option `. 

Functional update can be performed using `record.{ field ^= value }`
```Ocaml
let r' = r.{age ^= 20} 
assert ( r2.{age} = Some 10 && r2.{position} = r.{position} );;
```

Note that field can still be added after creation 
```Ocaml
let tags : 'string list field = new_field()
let r = r.{ tags ^= ["a tag"] }
```

Assignment for mutable field follows the natural syntax `r.{field}<- value`
```Ocaml
let () = r.{position}<- 20.
assert( r.{position} = Some 20. && r2.{position} = Some 20. );;
```

Mutable fields are shared during functional update. To avoid this behavior, copy the field
```Ocaml
let r3= r.{copy position}
let () = r3.{position} <- 10.
assert ( r2.{position} <> Some 10. );;
```

Given a bijection between type `'a` and `'b`, it is possible to transmute an `'a field` to an `'b field` 
```Ocaml
let age_str: string field = view age {to_ = string_of_int; from = int_of_string }
assert (r.{age_str} = "10" );; 

```
