(lang dune 3.10)

(name vg_example_plot)

(generate_opam_files true)

(authors "Ryan M. Moore")

(maintainers "Ryan M. Moore")

(homepage "https://github.com/mooreryan/vg_example_plot")

(documentation "https://github.com/mooreryan/vg_example_plot")

(bug_reports "https://github.com/mooreryan/vg_example_plot/issues")

(version 1.0.0)

(license "MIT")

(package
 (allow_empty)
 (name vg_example_plot)
 (synopsis "An example plot using brr and vg")
 (depends ocaml brr gg iter vg))
