OCAMLDEP=ocamlfind ocamldep
OCAMLC=ocamlfind ocamlc
OCAMLFLAGS=-w +a-4-29-30-40-41-42-44-45-48-26 -strict-sequence -strict-formats -bin-annot -package gen_js_api.ppx

MODULES=js_windows js_array webgl webgl_plot_math webgl_plot_dom_helper webgl_plot_geometry webgl_plot_intersection webgl_plot_shaders webgl_plot_textures webgl_plot_repere webgl_plot_scene webgl_plot_component webgl_plot_export webgl_plot_main # models plot main

CMOS=$(patsubst %,%.cmo,$(MODULES))
GENERATED=webgl.ml js_windows.ml js_array.ml webgl_plot_export.ml

main.js: main.byte
	js_of_ocaml --pretty -o main.js +gen_js_api/ojs_runtime.js main.byte

main.byte: $(CMOS)
	$(OCAMLC) $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api $(CMOS) -linkpkg -o $@

.SUFFIXES: .ml .mli .cmo .cmi

webgl.ml: webgl.mli
	ocamlfind gen_js_api/gen_js_api $<

js_array.ml: js_array.mli
	ocamlfind gen_js_api/gen_js_api $<

js_windows.ml: js_windows.mli
	ocamlfind gen_js_api/gen_js_api $<

webgl_plot_export.ml: webgl_plot_export.mli
	ocamlfind gen_js_api/gen_js_api $<

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $< -o $@

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<


.depend: $(GENERATED) $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(INCLUDES) *.mli *.ml > .depend

.PHONY: clean

clean:
	rm -f main.js main.byte *.cm* $(GENERATED)


-include .depend
