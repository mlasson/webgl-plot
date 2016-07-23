OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-I bindings
PPXFLAGS=-package gen_js_api.ppx

MODULES=bindings/js_core math helper drawing main

CMOS=$(patsubst %,%.cmo,$(MODULES))


main.js: main.byte
	js_of_ocaml -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package gen_js_api $(CMOS) -linkpkg -o $@

bindings/js_core.cmo: bindings/js_core.ml
math.cmo: math.ml
helper.cmo: helper.ml bindings/js_core.cmi
drawing.cmo: drawing.ml helper.cmi
main.cmo: main.ml

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm* bindings/*.cm*
