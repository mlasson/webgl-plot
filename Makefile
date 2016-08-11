OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-I bindings -w +a-4-29-30-40-41-42-44-45-48 -strict-sequence -strict-formats
PPXFLAGS=-package lwt -package gen_js_api.ppx

MODULES=dom computations math helper textures drawing main

CMOS=$(patsubst %,%.cmo,$(MODULES))


main.js: main.byte
	js_of_ocaml --pretty -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api $(CMOS) -linkpkg -o $@

dom.cmo: dom.ml
helper.cmo: helper.ml dom.cmo
drawing.cmo: math.cmo drawing.ml helper.cmo dom.cmo textures.cmo
textures.cmo: textures.ml dom.cmo
main.cmo: main.ml dom.cmo drawing.cmo
computations.cmo: computations.ml dom.cmo
math.cmo: math.ml dom.cmo 

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm* 
