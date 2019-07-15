# Building

    opam install topkg-care
    topkg build
    topkg doc -r

Alternatively if you have a `brzo` in your path:

    brzo -b

# Running

    topkg run brzo_bin
    brzo                # Be wary of the infinite build loop


Invoking `source dev-env` declares a `brzo` function that uses a brzo
built by one of the alternatives above (favouring a brzo build if 
it can be found).

# Adding a new domain

Adding a new domain is a matter of implementing the `Brzo_domain.T`
interface and adding the module to the list of domains in
[`brzo_domain_list.ml`](src/brzo_domain_list.ml). In general it is
good if the domain favours producing debuggable artefacts over
performant ones.

Each domain lives in its own `src-$(DOMAIN)` directory.

The documentation needs to be updated in a few places, the
[README.md](README.md) should mention the new domain. Besides in the
[manual](doc/manual.mld), the section with the list of domain and the
section for domain selection need to be updated. A new section for the
domain should be added.

A simple example to look at is the [cmark](src-cmark/brzo_cmark.ml) a more
complex one is the [ocaml](src-ocaml/brzo_ocaml.ml) domain.




