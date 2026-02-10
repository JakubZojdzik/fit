type 'a t = Ok of 'a | Error of string

val return : 'a -> 'a t
val fail : string -> 'a t
val bind : 'a t -> ('a -> 'b t) -> 'b t
