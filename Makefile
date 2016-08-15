OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-I bindings -w +a-4-29-30-40-41-42-44-45-48 -strict-sequence -strict-formats
PPXFLAGS=-package lwt -package gen_js_api.ppx

MODULES=js_bindings asynchronous_computations math helper textures models plot main

CMOS=$(patsubst %,%.cmo,$(MODULES))


main.js: main.byte
	js_of_ocaml --pretty -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api $(CMOS) -linkpkg -o $@

js_bindings.cmo: js_bindings.ml
helper.cmo: helper.ml js_bindings.cmo
models.cmo: models.ml math.cmo helper.cmo js_bindings.cmo textures.cmo
plot.cmo: plot.ml models.cmo
textures.cmo: textures.ml js_bindings.cmo
main.cmo: main.ml js_bindings.cmo models.cmo plot.cmo
asynchronous_computations.cmo: asynchronous_computations.ml js_bindings.cmo
math.cmo: math.ml js_bindings.cmo 

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm* 
