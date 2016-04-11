open Import

let ppx_tools               = root_relative "packages/ppx_tools-ppx_tools_0.99.3"
let inline_test_runner_dir  = root_relative "packages/ppx_inline_test-113.24.00/runner"
let expect_runner_dir       = root_relative "packages/ppx_expect-113.24.00/evaluator"
let bin_dir                 = root_relative "jenga/bin"
let include_dir             = root_relative "include"
let hg_prog                 = Path.absolute "/usr/local/bin/hg"
let do_jswrap               = false
