OCAMLDEP=ocamlfind ocamldep
OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-w +a-4-29-30-40-41-42-44-45-48 -strict-sequence -strict-formats -bin-annot
PPXFLAGS=-package lwt -package gen_js_api.ppx

MODULES=js_bindings webgl asynchronous_computations math helper geometry intersection shaders textures repere scene component main # models plot main

CMOS=$(patsubst %,%.cmo,$(MODULES))
GENERATED=webgl.ml js_bindings.ml

main.js: main.byte
	js_of_ocaml --pretty -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api $(CMOS) -linkpkg -o $@

.SUFFIXES: .ml .mli .cmo .cmi

webgl.ml: webgl.mli
	ocamlfind gen_js_api/gen_js_api $<

js_bindings.ml: js_bindings.mli
	ocamlfind gen_js_api/gen_js_api $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<


.depend: $(GENERATED) $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm* $(GENERATED)


-include .depend
