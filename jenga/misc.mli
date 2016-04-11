open! Core.Std

val put     : ('a, unit, string, unit) format4 -> 'a
val message : ('a, unit, string, unit) format4 -> 'a

val getenv_bool : string -> default:bool        -> bool
val getenv_args : string -> default:string list -> string list

val getenv_enumeration
  :  ?fallback : (string -> 'a)
  -> string
  -> choices : 'a String.Map.t
  -> default : string
  -> 'a
