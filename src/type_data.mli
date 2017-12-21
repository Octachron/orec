(** Type level data *)

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
