.PHONY: all
all: data control

.PHONY: control
control:
	ocamlbuild -r control.cmo

.PHONY: data
data:
	ocamlbuild -r data.cmo

.PHONY: clean
clean: force
	ocamlbuild -clean

force:
