# Dev Notes

## Organization of the code

### Javascript Bindings

We use `gen_js_api` to generate javascript bindings automatically from these interfaces:
* `webgl.mli`: Webgl bindings
* `Js_windows.mli`: Browsers' bindings
* `Js_array.mli`: Javascript typed arrays

### Modules

* `Webgl_plot_misc`: Various general-purpose functions,

* Mathematics: 
  * `webgl_plot_math`: Vector/matrix operations, 
  * `webgl_plot_geometry.ml`: Computing points, triangles and normals coordinates, 
  * `webgl_plot_intersection.ml`: Data-structure to compute intersections between a ray and lots of triangles.

* Webgl - scene:
  * `webgl_plot_shaders.ml`: Where all the available are defined, compiled and interfaced,
  * `webgl_plot_drawable.ml`: Defined what is a generic object in our scene, 
  * `webgl_plot_scene.ml`: Instantiate shaders, handles the view state and draw objects.
  
* 3D objects: 
  * System of axes: 
    * `webgl_plot_textures.ml`: Generate textures using 2d canvas for the axes,
    * `webgl_plot_repere.ml`: The 3d object representing axes.
  * `webgl_plot_surface.ml` : Drawing surfaces,
  * `webgl_plot_histogram.ml`: Drawing histograms.

* DOM :
  * `webgl_plot_dom_helper`: Some dom-related helper functions, 
  * `webgl_plot_component`: Setup the canvas, binds DOM events, handle the mouse/canvas related state.

* Public interfaces:
  * `webgl_plot_export.mli`: Interface to generate through `gen_js_api` the json export/import format used to describe plots,
  * `webgl_plot.ml`: Ocaml interface to create plots. 
