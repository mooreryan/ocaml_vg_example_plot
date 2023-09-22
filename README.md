# OCaml Vg Example Plot

A tiny example plot using OCaml, [Js_of_ocaml](http://ocsigen.org/js_of_ocaml/latest/manual/overview), [Brr](https://erratique.ch/software/brr), and [Vg](https://erratique.ch/software/vg).

Check it out [here](https://mooreryan.github.io/ocaml_vg_example_plot/). You can click on the plot to add dots, remove them, and save your image!

## Building the site

The site is in `docs` so it works with GitHub pages.

To build the site, run `just build_site` (or simply `dune build --profile=release`).

## Hacking

If you want to hack on it, note that I didn't bother to specify package versions for the `opam` file. But you could use the lock file if you want to play with it:

```
opam install . --deps-only --locked
```

In particular, this will ensure you have the pinned version of `vg`, which is needed to work with `brr` rather than the jsoo library.

## JS bundle size

The generated JavaScript is 125KB (43KB gzipped). Not too bad, all things considered.
