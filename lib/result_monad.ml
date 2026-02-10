type 'a t = Ok of 'a | Error of string

let return x = Ok x
let fail e = Error e
let bind f g = match f with Ok x -> g x | Error e -> Error e
