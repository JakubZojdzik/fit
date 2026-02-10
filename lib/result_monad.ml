type 'a t = 
  | Ok of 'a
  | Error of string

let return x = Ok x

let fail e = Error e

let bind m f =
  match m with
  | Ok x -> f x
  | Error e -> Error e
