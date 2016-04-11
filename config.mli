open Import

(** Location of the root of the [ppx_tools] distribution *)
val ppx_tools : Path.t

(** Location of inline test runtime support *)
val inline_test_runner_dir : Path.t

(** Location of expect test runtime support *)
val expect_runner_dir : Path.t

(** Location of scripts used by build-rule actions in jenga/*.ml *)
val bin_dir : Path.t

(** Location of C header used by C-compilation rules in jenga/root.ml *)
val include_dir : Path.t

(** Location of [hg] program *)
val hg_prog : Path.t

(** Special handling for CentOS at JS *)
val do_jswrap : bool
