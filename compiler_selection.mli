(** This module decides which ocaml compiler to run, and defines various options that
    depend on the compiler version. *)

val compiler_dir : string

(* Options to [CC] and [CXX] compilers. *)
val cflags : string list

(* Same as [cflags] but for architecture-specific options that are unconditionally
   needed by [CC] and [CXX] compilers.
   These options should never need to be overridden by user-defined configuration.
   Currently used to select between 32-bit and 64-bit targets. *)
val arch_cflags : string list

val disabled_warnings : int list

val m32 : bool
val javascript : bool

(** extra options you need to pass to pa_macro so it can tell
    the compiler versions apart *)
val pa_macro_flags : string list
