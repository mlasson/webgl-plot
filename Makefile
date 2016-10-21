VERSION=1.0

MODULES=js_windows js_array webgl webgl_plot_export webgl_plot_math webgl_plot_misc webgl_plot_dom_helper \
	webgl_plot_geometry webgl_plot_intersection webgl_plot_shaders \
	webgl_plot_drawable webgl_plot_histogram webgl_plot_surface \
	webgl_plot_textures webgl_plot_repere \
	webgl_plot_scene webgl_plot_component webgl_plot

JS_INTERFACE=webgl_plot_js.cmo

CMOS=$(patsubst %,%.cmo,$(MODULES))
GENERATED=webgl.ml js_windows.ml js_array.ml webgl_plot_export.ml

NAME=webgl-plot

PACKAGES=-package gen_js_api.ppx
OCAMLFLAGS=-w +a-4-29-30-40-41-42-44-45-48 -strict-sequence -strict-formats -bin-annot $(PACKAGES)

.PHONY: all lib js clean doc

all: check_dependencies lib js doc META

META: Makefile
	@sed -i "s/version = \"[^\"]*\"/version = \"$(VERSION)\"/" $@

lib: $(NAME).cma

js: $(NAME).js

doc: $(CMOS)
	mkdir -p doc
	ocamlfind ocamldoc $(PACKAGES) -d doc -html webgl_plot_export.mli webgl_plot.mli

clean:
	rm -f $(NAME).js $(NAME).byte *.cm* $(GENERATED)

$(NAME).js: $(NAME).byte
	js_of_ocaml --pretty -o $(NAME).js +gen_js_api/ojs_runtime.js $(NAME).byte

$(NAME).byte: $(CMOS) $(JS_INTERFACE)
	ocamlfind ocamlc $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api $(CMOS) $(JS_INTERFACE) -linkpkg -o $@

$(NAME).cma: $(CMOS)
	ocamlfind ocamlc $(OCAMLFLAGS) -no-check-prims -package lwt -package gen_js_api -a $(CMOS) -o $@

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
	ocamlfind ocamlc $(OCAMLFLAGS) -c $< -o $@

.mli.cmi:
	ocamlfind ocamlc $(OCAMLFLAGS) -c $<

.depend: $(GENERATED) $(wildcard *.ml) $(wildcard *.mli)
	ocamlfind ocamldep $(INCLUDES) *.mli *.ml > .depend

.PHONY: install uninstall check_dependencies

INSTALL=META $(wildcard *.cmo) \
             $(wildcard *.cma) \
             $(wildcard *.cmi) \
             $(wildcard *.cmt) \
             $(wildcard *.cmti) \
             $(wildcard *.mli) \
             $(wildcard $(NAME).js)

check_dependencies:
	@ocamlfind printconf > /dev/null 2>&1 || (echo "[ERROR] Findlib is required." && exit 1)
	@ocamlfind query gen_js_api > /dev/null || (echo '[ERROR] The package `gen_js_api`.' && exit 1)

install: check_dependencies
	ocamlfind install $(NAME) $(INSTALL)

uninstall: check_dependencies
	ocamlfind remove $(NAME)

-include .depend
