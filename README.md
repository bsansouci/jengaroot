Set of build rules for jenga.
---

# Setup

1) Run `./setup.sh`

2) Replace `/Users/benjamin/.opam/4.02.3` in `compiler_selection.ml` with the absolute path to the 4.02.3 compiler tools

3) Replace `/Users/benjamin/Desktop/jengaroot/hacky-fix-for-mac.sh` in `jenga/root` with the absolute path to the script in this repository.

4) Run `jenga -verbose -act --progress -show-error-dependency-paths ./example`
