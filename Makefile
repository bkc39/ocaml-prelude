.PHONY: all
all: control data

.PHONY: control
control:
	ocamlbuild -r control.cma

.PHONY: data
data:
	ocamlbuild -r data.cma

.PHONY: clean
clean: force
	ocamlbuild -clean

force:
