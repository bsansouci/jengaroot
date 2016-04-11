
This is a distribution of the jenga build rules used at Jane Street, with minimal changes
to allow them to work externally. In particular it contains rules for:

- ocaml compilation (libraries and executables)
- ocaml library discovery
- inline unit & expect (cram style) tests

Also in this distribution is a small [example], demonstrating the use of per-directory
[jbuild] files to configure the build. The example builds two libraries and an executable,
and in [example/tricky-algorithms] there are examples of unit and expect tests.



(1) Run ./setup.sh to download & unpack dependant packages.

(2) Edit compiler_selection.ml, replacing [/path/to/ocaml/distribution]. Perhaps:

        $ sed "s|/path/to/ocaml/distribution|/home/$USER/.opam/4.02.3|" -i compiler_selection.ml

(3) Build using an Opam built & installed jenga (perhaps: /home/$USER/.opam/4.02.3/bin/jenga):


Build everything. (-P means polling. C-c to exit jenga)

    $ jenga -progress -P 

Build everything necessary to build example & run example tests:

    $ jenga -progress -P example/.runtest


Use the [-act] flag to see which targets are build or re-build.
For more verbosity use [-verbose].


Build artifacts are tracked using "hg", and can be cleaned using:

    $ hg st -ni0 | xargs -0 rm -v


(4) Incremental rebuild

If jenga is stopped and restarted, only the actions necessary to bring the build
up-to-date will be run.  This can be seen by running jenga twice and comparing the
[act]ion count reported in the [todo] line.

    $ jenga
    <...>
    *** jenga: todo: 0 (2144 / 2144) j=0+0 con=2144 save=592 act=1184, finished

    $ jenga
    <...>
    *** jenga: todo: 0 (2144 / 2144) j=0+0 con=2144 save=0 act=0, finished


(5) Polling rebuild

If jenga is running in polling mode (-P), file changes are detected and the build is
brought up-to-date, without having to restart jenga. For example:

    $ jenga -prog -P example/.runtest


And then while the polling jenga is running, modify the source in another terminal:


    Break a unit test:
        $ sed 's/fact 4 = 24/fact 4 = 25/' -i example/tricky-algorithms/fact.ml

    Revert:
        $ hg revert example/tricky-algorithms/fact.ml


    Break the expect test:
        $ sed 's/fibs 7/fibs 10/' -i example/tricky-algorithms/fibs.ml

    Update the expectation:
        $ mv example/tricky-algorithms/fibs.ml{.corrected,}


(6) Issues when displaying expect-test diffs.

Expect-test differences are displayed using [patdiff]. This can be installed via opam, but
the code which runs patdiff (sadly) expects to find it in the path. The path used by jenga
is locked down by jenga/root.ml - search for: [command_lookup_path] - and so you will have
to extend this with the path to where [patdiff] is installed at your site... unless it
happens to be installed (as it is at JS) in one of:

  let minimal_path_kept_even_after_replacement = "/bin:/usr/bin:/usr/local/bin" in

(This line is from env.ml of the jenga code base.)

In future we should have more principled configuration for this.


(7) Add your own sources!

The sources can go anywhere in the tree. Each directory has a [jbuild] file which declares
what is being built (library, executables), it's name, what libraries are referenced, and
other settings. See jenga/jbuild_types.ml for some limited documentation.

The location of the libraries within the repository under jenga's control is determined
automatically by the build rules - written to the file [libmap.sexp] at the root of the
repo - and a per-library subdir within the [.liblinks] dir is used when compiling.  The
root of the repository is marked by the [jenga.conf] file.
