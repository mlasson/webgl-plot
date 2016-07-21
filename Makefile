OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-I bindings  
PPXFLAGS=-package gen_js_api.ppx

MODULES=math helper drawing main

CMOS=$(patsubst %,%.cmo,$(MODULES))

main.js: main.byte
	js_of_ocaml -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package gen_js_api js_core.cmo $(CMOS) -linkpkg -o $@

.SUFFIXES: .ml .mli .cmo .cmi

.ml.cmo:
	        $(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<

.mli.cmi:
	        $(OCAMLC) $(OCAMLFLAGS) $(PPXFLAGS) -c $<

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm*
