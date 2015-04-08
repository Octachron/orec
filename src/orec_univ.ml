type t = ..

exception Nothing
let specialize (type u) ()=
  let module S=struct
    type t += C of u
    let inj x = C x
    let proj = function C x -> x | _-> raise Nothing
  end
  in
  S.(inj,proj)
