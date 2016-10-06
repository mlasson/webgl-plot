module Basic :
  sig
    class type shader =
      object
        method binds_colors : Webgl.buffer -> unit
        method binds_indexes : Webgl.buffer -> unit
        method binds_normals : Webgl.buffer -> unit
        method binds_positions : Webgl.buffer -> unit

        method set_ambient_light : float -> float -> float -> unit
        method set_light_position : float -> float -> float -> unit
        method set_object_matrix : Webgl.Float32Array.t -> unit
        method set_world_matrix : Webgl.Float32Array.t -> unit

        method use : unit
      end

    val init : Webgl.context -> shader

    class drawable : Webgl.context -> shader ->
      object
        method draw : unit
        method set_colors : Webgl.Float32Array.t -> unit
        method set_cull_face : bool -> unit
        method set_indexes : Geometry.indexes -> unit
        method set_indexes8 : Webgl.Uint8Array.t -> unit
        method set_indexes16 : Webgl.Uint16Array.t -> unit
        method set_indexes32 : Webgl.Uint32Array.t -> unit
        method set_mode : Webgl.Constant.t -> unit
        method set_normals : Webgl.Float32Array.t -> unit
        method set_positions : Webgl.Float32Array.t -> unit
      end
  end

module Texture :
  sig
    class type shader =
      object
        method binds_triangles : Webgl.buffer -> unit
        method binds_texcoords : Webgl.buffer -> unit
        method binds_texture: Webgl.texture -> unit

        method set_world_matrix : Webgl.Float32Array.t -> unit
        method use : unit
      end

    val init : Webgl.context -> shader

    class drawable : Webgl.context -> shader ->
      object
        method draw : unit
        method set_triangles : Webgl.Float32Array.t -> unit
        method set_texcoords : Webgl.Float32Array.t -> unit
        method set_texture : Js_bindings.Element.t -> unit
      end
  end
