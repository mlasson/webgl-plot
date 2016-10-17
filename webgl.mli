[@@@js.stop]
open Js_array
[@@@js.start]
[@@@js.implem open Js_array ]

module Constant : sig
  type t = int
  val t_of_js: Ojs.t -> t
  val t_to_js: t -> Ojs.t

  val _ACTIVE_ATTRIBUTES_ : t [@@js.custom let _ACTIVE_ATTRIBUTES_ = 35721]
  val _ACTIVE_ATTRIBUTE_MAX_LENGTH_ : t [@@js.custom let _ACTIVE_ATTRIBUTE_MAX_LENGTH_ = 35722]
  val _ACTIVE_TEXTURE_ : t [@@js.custom let _ACTIVE_TEXTURE_ = 34016]
  val _ACTIVE_UNIFORMS_ : t [@@js.custom let _ACTIVE_UNIFORMS_ = 35718]
  val _ACTIVE_UNIFORM_MAX_LENGTH_ : t [@@js.custom let _ACTIVE_UNIFORM_MAX_LENGTH_ = 35719]
  val _ALIASED_LINE_WIDTH_RANGE_ : t [@@js.custom let _ALIASED_LINE_WIDTH_RANGE_ = 33902]
  val _ALIASED_POINT_SIZE_RANGE_ : t [@@js.custom let _ALIASED_POINT_SIZE_RANGE_ = 33901]
  val _ALPHA_ : t [@@js.custom let _ALPHA_ = 6406]
  val _ALPHA_BITS_ : t [@@js.custom let _ALPHA_BITS_ = 3413]
  val _ALWAYS_ : t [@@js.custom let _ALWAYS_ = 519]
  val _ARRAY_BUFFER_ : t [@@js.custom let _ARRAY_BUFFER_ = 34962]
  val _ARRAY_BUFFER_BINDING_ : t [@@js.custom let _ARRAY_BUFFER_BINDING_ = 34964]
  val _ATTACHED_SHADERS_ : t [@@js.custom let _ATTACHED_SHADERS_ = 35717]
  val _BACK_ : t [@@js.custom let _BACK_ = 1029]
  val _BLEND_ : t [@@js.custom let _BLEND_ = 3042]
  val _BLEND_COLOR_ : t [@@js.custom let _BLEND_COLOR_ = 32773]
  val _BLEND_DST_ALPHA_ : t [@@js.custom let _BLEND_DST_ALPHA_ = 32970]
  val _BLEND_DST_RGB_ : t [@@js.custom let _BLEND_DST_RGB_ = 32968]
  val _BLEND_EQUATION_ : t [@@js.custom let _BLEND_EQUATION_ = 32777]
  val _BLEND_EQUATION_ALPHA_ : t [@@js.custom let _BLEND_EQUATION_ALPHA_ = 34877]
  val _BLEND_EQUATION_RGB_ : t [@@js.custom let _BLEND_EQUATION_RGB_ = 32777]
  val _BLEND_SRC_ALPHA_ : t [@@js.custom let _BLEND_SRC_ALPHA_ = 32971]
  val _BLEND_SRC_RGB_ : t [@@js.custom let _BLEND_SRC_RGB_ = 32969]
  val _BLUE_BITS_ : t [@@js.custom let _BLUE_BITS_ = 3412]
  val _BOOL_ : t [@@js.custom let _BOOL_ = 35670]
  val _BOOL_VEC2_ : t [@@js.custom let _BOOL_VEC2_ = 35671]
  val _BOOL_VEC3_ : t [@@js.custom let _BOOL_VEC3_ = 35672]
  val _BOOL_VEC4_ : t [@@js.custom let _BOOL_VEC4_ = 35673]
  val _BROWSER_DEFAULT_WEBGL_ : t [@@js.custom let _BROWSER_DEFAULT_WEBGL_ = 37444]
  val _BUFFER_SIZE_ : t [@@js.custom let _BUFFER_SIZE_ = 34660]
  val _BUFFER_USAGE_ : t [@@js.custom let _BUFFER_USAGE_ = 34661]
  val _BYTE_ : t [@@js.custom let _BYTE_ = 5120]
  val _CCW_ : t [@@js.custom let _CCW_ = 2305]
  val _CLAMP_TO_EDGE_ : t [@@js.custom let _CLAMP_TO_EDGE_ = 33071]
  val _COLOR_ATTACHMENT0_ : t [@@js.custom let _COLOR_ATTACHMENT0_ = 36064]
  val _COLOR_BUFFER_BIT_ : t [@@js.custom let _COLOR_BUFFER_BIT_ = 16384]
  val _COLOR_CLEAR_VALUE_ : t [@@js.custom let _COLOR_CLEAR_VALUE_ = 3106]
  val _COLOR_WRITEMASK_ : t [@@js.custom let _COLOR_WRITEMASK_ = 3107]
  val _COMPILE_STATUS_ : t [@@js.custom let _COMPILE_STATUS_ = 35713]
  val _COMPRESSED_TEXTURE_FORMATS_ : t [@@js.custom let _COMPRESSED_TEXTURE_FORMATS_ = 34467]
  val _CONSTANT_ALPHA_ : t [@@js.custom let _CONSTANT_ALPHA_ = 32771]
  val _CONSTANT_COLOR_ : t [@@js.custom let _CONSTANT_COLOR_ = 32769]
  val _CONTEXT_LOST_WEBGL_ : t [@@js.custom let _CONTEXT_LOST_WEBGL_ = 37442]
  val _CULL_FACE_ : t [@@js.custom let _CULL_FACE_ = 2884]
  val _CULL_FACE_MODE_ : t [@@js.custom let _CULL_FACE_MODE_ = 2885]
  val _CURRENT_PROGRAM_ : t [@@js.custom let _CURRENT_PROGRAM_ = 35725]
  val _CURRENT_VERTEX_ATTRIB_ : t [@@js.custom let _CURRENT_VERTEX_ATTRIB_ = 34342]
  val _CW_ : t [@@js.custom let _CW_ = 2304]
  val _DECR_ : t [@@js.custom let _DECR_ = 7683]
  val _DECR_WRAP_ : t [@@js.custom let _DECR_WRAP_ = 34056]
  val _DELETE_STATUS_ : t [@@js.custom let _DELETE_STATUS_ = 35712]
  val _DEPTH_ATTACHMENT_ : t [@@js.custom let _DEPTH_ATTACHMENT_ = 36096]
  val _DEPTH_BITS_ : t [@@js.custom let _DEPTH_BITS_ = 3414]
  val _DEPTH_BUFFER_BIT_ : t [@@js.custom let _DEPTH_BUFFER_BIT_ = 256]
  val _DEPTH_CLEAR_VALUE_ : t [@@js.custom let _DEPTH_CLEAR_VALUE_ = 2931]
  val _DEPTH_COMPONENT_ : t [@@js.custom let _DEPTH_COMPONENT_ = 6402]
  val _DEPTH_COMPONENT16_ : t [@@js.custom let _DEPTH_COMPONENT16_ = 33189]
  val _DEPTH_FUNC_ : t [@@js.custom let _DEPTH_FUNC_ = 2932]
  val _DEPTH_RANGE_ : t [@@js.custom let _DEPTH_RANGE_ = 2928]
  val _DEPTH_STENCIL_ : t [@@js.custom let _DEPTH_STENCIL_ = 34041]
  val _DEPTH_STENCIL_ATTACHMENT_ : t [@@js.custom let _DEPTH_STENCIL_ATTACHMENT_ = 33306]
  val _DEPTH_TEST_ : t [@@js.custom let _DEPTH_TEST_ = 2929]
  val _DEPTH_WRITEMASK_ : t [@@js.custom let _DEPTH_WRITEMASK_ = 2930]
  val _DITHER_ : t [@@js.custom let _DITHER_ = 3024]
  val _DONT_CARE_ : t [@@js.custom let _DONT_CARE_ = 4352]
  val _DST_ALPHA_ : t [@@js.custom let _DST_ALPHA_ = 772]
  val _DST_COLOR_ : t [@@js.custom let _DST_COLOR_ = 774]
  val _DYNAMIC_DRAW_ : t [@@js.custom let _DYNAMIC_DRAW_ = 35048]
  val _ELEMENT_ARRAY_BUFFER_ : t [@@js.custom let _ELEMENT_ARRAY_BUFFER_ = 34963]
  val _ELEMENT_ARRAY_BUFFER_BINDING_ : t [@@js.custom let _ELEMENT_ARRAY_BUFFER_BINDING_ = 34965]
  val _EQUAL_ : t [@@js.custom let _EQUAL_ = 514]
  val _FASTEST_ : t [@@js.custom let _FASTEST_ = 4353]
  val _FLOAT_ : t [@@js.custom let _FLOAT_ = 5126]
  val _FLOAT_MAT2_ : t [@@js.custom let _FLOAT_MAT2_ = 35674]
  val _FLOAT_MAT3_ : t [@@js.custom let _FLOAT_MAT3_ = 35675]
  val _FLOAT_MAT4_ : t [@@js.custom let _FLOAT_MAT4_ = 35676]
  val _FLOAT_VEC2_ : t [@@js.custom let _FLOAT_VEC2_ = 35664]
  val _FLOAT_VEC3_ : t [@@js.custom let _FLOAT_VEC3_ = 35665]
  val _FLOAT_VEC4_ : t [@@js.custom let _FLOAT_VEC4_ = 35666]
  val _FRAGMENT_SHADER_ : t [@@js.custom let _FRAGMENT_SHADER_ = 35632]
  val _FRAMEBUFFER_ : t [@@js.custom let _FRAMEBUFFER_ = 36160]
  val _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_ : t [@@js.custom let _FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_ = 36049]
  val _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_ : t [@@js.custom let _FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_ = 36048]
  val _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_ : t [@@js.custom let _FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_ = 36051]
  val _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_ : t [@@js.custom let _FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_ = 36050]
  val _FRAMEBUFFER_BINDING_ : t [@@js.custom let _FRAMEBUFFER_BINDING_ = 36006]
  val _FRAMEBUFFER_COMPLETE_ : t [@@js.custom let _FRAMEBUFFER_COMPLETE_ = 36053]
  val _FRAMEBUFFER_INCOMPLETE_ATTACHMENT_ : t [@@js.custom let _FRAMEBUFFER_INCOMPLETE_ATTACHMENT_ = 36054]
  val _FRAMEBUFFER_INCOMPLETE_DIMENSIONS_ : t [@@js.custom let _FRAMEBUFFER_INCOMPLETE_DIMENSIONS_ = 36057]
  val _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_ : t [@@js.custom let _FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_ = 36055]
  val _FRAMEBUFFER_UNSUPPORTED_ : t [@@js.custom let _FRAMEBUFFER_UNSUPPORTED_ = 36061]
  val _FRONT_ : t [@@js.custom let _FRONT_ = 1028]
  val _FRONT_AND_BACK_ : t [@@js.custom let _FRONT_AND_BACK_ = 1032]
  val _FRONT_FACE_ : t [@@js.custom let _FRONT_FACE_ = 2886]
  val _FUNC_ADD_ : t [@@js.custom let _FUNC_ADD_ = 32774]
  val _FUNC_REVERSE_SUBTRACT_ : t [@@js.custom let _FUNC_REVERSE_SUBTRACT_ = 32779]
  val _FUNC_SUBTRACT_ : t [@@js.custom let _FUNC_SUBTRACT_ = 32778]
  val _GENERATE_MIPMAP_HINT_ : t [@@js.custom let _GENERATE_MIPMAP_HINT_ = 33170]
  val _GEQUAL_ : t [@@js.custom let _GEQUAL_ = 518]
  val _GREATER_ : t [@@js.custom let _GREATER_ = 516]
  val _GREEN_BITS_ : t [@@js.custom let _GREEN_BITS_ = 3411]
  val _HIGH_FLOAT_ : t [@@js.custom let _HIGH_FLOAT_ = 36338]
  val _HIGH_INT_ : t [@@js.custom let _HIGH_INT_ = 36341]
  val _INCR_ : t [@@js.custom let _INCR_ = 7682]
  val _INCR_WRAP_ : t [@@js.custom let _INCR_WRAP_ = 34055]
  val _INFO_LOG_LENGTH_ : t [@@js.custom let _INFO_LOG_LENGTH_ = 35716]
  val _INT_ : t [@@js.custom let _INT_ = 5124]
  val _INT_VEC2_ : t [@@js.custom let _INT_VEC2_ = 35667]
  val _INT_VEC3_ : t [@@js.custom let _INT_VEC3_ = 35668]
  val _INT_VEC4_ : t [@@js.custom let _INT_VEC4_ = 35669]
  val _INVALID_ENUM_ : t [@@js.custom let _INVALID_ENUM_ = 1280]
  val _INVALID_FRAMEBUFFER_OPERATION_ : t [@@js.custom let _INVALID_FRAMEBUFFER_OPERATION_ = 1286]
  val _INVALID_OPERATION_ : t [@@js.custom let _INVALID_OPERATION_ = 1282]
  val _INVALID_VALUE_ : t [@@js.custom let _INVALID_VALUE_ = 1281]
  val _INVERT_ : t [@@js.custom let _INVERT_ = 5386]
  val _KEEP_ : t [@@js.custom let _KEEP_ = 7680]
  val _LEQUAL_ : t [@@js.custom let _LEQUAL_ = 515]
  val _LESS_ : t [@@js.custom let _LESS_ = 513]
  val _LINEAR_ : t [@@js.custom let _LINEAR_ = 9729]
  val _LINEAR_MIPMAP_LINEAR_ : t [@@js.custom let _LINEAR_MIPMAP_LINEAR_ = 9987]
  val _LINEAR_MIPMAP_NEAREST_ : t [@@js.custom let _LINEAR_MIPMAP_NEAREST_ = 9985]
  val _LINES_ : t [@@js.custom let _LINES_ = 1]
  val _LINE_LOOP_ : t [@@js.custom let _LINE_LOOP_ = 2]
  val _LINE_STRIP_ : t [@@js.custom let _LINE_STRIP_ = 3]
  val _LINE_WIDTH_ : t [@@js.custom let _LINE_WIDTH_ = 2849]
  val _LINK_STATUS_ : t [@@js.custom let _LINK_STATUS_ = 35714]
  val _LOW_FLOAT_ : t [@@js.custom let _LOW_FLOAT_ = 36336]
  val _LOW_INT_ : t [@@js.custom let _LOW_INT_ = 36339]
  val _LUMINANCE_ : t [@@js.custom let _LUMINANCE_ = 6409]
  val _LUMINANCE_ALPHA_ : t [@@js.custom let _LUMINANCE_ALPHA_ = 6410]
  val _MAX_COMBINED_TEXTURE_IMAGE_UNITS_ : t [@@js.custom let _MAX_COMBINED_TEXTURE_IMAGE_UNITS_ = 35661]
  val _MAX_CUBE_MAP_TEXTURE_SIZE_ : t [@@js.custom let _MAX_CUBE_MAP_TEXTURE_SIZE_ = 34076]
  val _MAX_FRAGMENT_UNIFORM_VECTORS_ : t [@@js.custom let _MAX_FRAGMENT_UNIFORM_VECTORS_ = 36349]
  val _MAX_RENDERBUFFER_SIZE_ : t [@@js.custom let _MAX_RENDERBUFFER_SIZE_ = 34024]
  val _MAX_TEXTURE_IMAGE_UNITS_ : t [@@js.custom let _MAX_TEXTURE_IMAGE_UNITS_ = 34930]
  val _MAX_TEXTURE_SIZE_ : t [@@js.custom let _MAX_TEXTURE_SIZE_ = 3379]
  val _MAX_VARYING_VECTORS_ : t [@@js.custom let _MAX_VARYING_VECTORS_ = 36348]
  val _MAX_VERTEX_ATTRIBS_ : t [@@js.custom let _MAX_VERTEX_ATTRIBS_ = 34921]
  val _MAX_VERTEX_TEXTURE_IMAGE_UNITS_ : t [@@js.custom let _MAX_VERTEX_TEXTURE_IMAGE_UNITS_ = 35660]
  val _MAX_VERTEX_UNIFORM_VECTORS_ : t [@@js.custom let _MAX_VERTEX_UNIFORM_VECTORS_ = 36347]
  val _MAX_VIEWPORT_DIMS_ : t [@@js.custom let _MAX_VIEWPORT_DIMS_ = 3386]
  val _MEDIUM_FLOAT_ : t [@@js.custom let _MEDIUM_FLOAT_ = 36337]
  val _MEDIUM_INT_ : t [@@js.custom let _MEDIUM_INT_ = 36340]
  val _MIRRORED_REPEAT_ : t [@@js.custom let _MIRRORED_REPEAT_ = 33648]
  val _NEAREST_ : t [@@js.custom let _NEAREST_ = 9728]
  val _NEAREST_MIPMAP_LINEAR_ : t [@@js.custom let _NEAREST_MIPMAP_LINEAR_ = 9986]
  val _NEAREST_MIPMAP_NEAREST_ : t [@@js.custom let _NEAREST_MIPMAP_NEAREST_ = 9984]
  val _NEVER_ : t [@@js.custom let _NEVER_ = 512]
  val _NICEST_ : t [@@js.custom let _NICEST_ = 4354]
  val _NONE_ : t [@@js.custom let _NONE_ = 0]
  val _NOTEQUAL_ : t [@@js.custom let _NOTEQUAL_ = 517]
  val _NO_ERROR_ : t [@@js.custom let _NO_ERROR_ = 0]
  val _NUM_COMPRESSED_TEXTURE_FORMATS_ : t [@@js.custom let _NUM_COMPRESSED_TEXTURE_FORMATS_ = 34466]
  val _ONE_ : t [@@js.custom let _ONE_ = 1]
  val _ONE_MINUS_CONSTANT_ALPHA_ : t [@@js.custom let _ONE_MINUS_CONSTANT_ALPHA_ = 32772]
  val _ONE_MINUS_CONSTANT_COLOR_ : t [@@js.custom let _ONE_MINUS_CONSTANT_COLOR_ = 32770]
  val _ONE_MINUS_DST_ALPHA_ : t [@@js.custom let _ONE_MINUS_DST_ALPHA_ = 773]
  val _ONE_MINUS_DST_COLOR_ : t [@@js.custom let _ONE_MINUS_DST_COLOR_ = 775]
  val _ONE_MINUS_SRC_ALPHA_ : t [@@js.custom let _ONE_MINUS_SRC_ALPHA_ = 771]
  val _ONE_MINUS_SRC_COLOR_ : t [@@js.custom let _ONE_MINUS_SRC_COLOR_ = 769]
  val _OUT_OF_MEMORY_ : t [@@js.custom let _OUT_OF_MEMORY_ = 1285]
  val _PACK_ALIGNMENT_ : t [@@js.custom let _PACK_ALIGNMENT_ = 3333]
  val _POINTS_ : t [@@js.custom let _POINTS_ = 0]
  val _POLYGON_OFFSET_FACTOR_ : t [@@js.custom let _POLYGON_OFFSET_FACTOR_ = 32824]
  val _POLYGON_OFFSET_FILL_ : t [@@js.custom let _POLYGON_OFFSET_FILL_ = 32823]
  val _POLYGON_OFFSET_UNITS_ : t [@@js.custom let _POLYGON_OFFSET_UNITS_ = 10752]
  val _RED_BITS_ : t [@@js.custom let _RED_BITS_ = 3410]
  val _RENDERBUFFER_ : t [@@js.custom let _RENDERBUFFER_ = 36161]
  val _RENDERBUFFER_ALPHA_SIZE_ : t [@@js.custom let _RENDERBUFFER_ALPHA_SIZE_ = 36179]
  val _RENDERBUFFER_BINDING_ : t [@@js.custom let _RENDERBUFFER_BINDING_ = 36007]
  val _RENDERBUFFER_BLUE_SIZE_ : t [@@js.custom let _RENDERBUFFER_BLUE_SIZE_ = 36178]
  val _RENDERBUFFER_DEPTH_SIZE_ : t [@@js.custom let _RENDERBUFFER_DEPTH_SIZE_ = 36180]
  val _RENDERBUFFER_GREEN_SIZE_ : t [@@js.custom let _RENDERBUFFER_GREEN_SIZE_ = 36177]
  val _RENDERBUFFER_HEIGHT_ : t [@@js.custom let _RENDERBUFFER_HEIGHT_ = 36163]
  val _RENDERBUFFER_INTERNAL_FORMAT_ : t [@@js.custom let _RENDERBUFFER_INTERNAL_FORMAT_ = 36164]
  val _RENDERBUFFER_RED_SIZE_ : t [@@js.custom let _RENDERBUFFER_RED_SIZE_ = 36176]
  val _RENDERBUFFER_STENCIL_SIZE_ : t [@@js.custom let _RENDERBUFFER_STENCIL_SIZE_ = 36181]
  val _RENDERBUFFER_WIDTH_ : t [@@js.custom let _RENDERBUFFER_WIDTH_ = 36162]
  val _RENDERER_ : t [@@js.custom let _RENDERER_ = 7937]
  val _REPEAT_ : t [@@js.custom let _REPEAT_ = 10497]
  val _REPLACE_ : t [@@js.custom let _REPLACE_ = 7681]
  val _RGB_ : t [@@js.custom let _RGB_ = 6407]
  val _RGB5_A1_ : t [@@js.custom let _RGB5_A1_ = 32855]
  val _RGB565_ : t [@@js.custom let _RGB565_ = 36194]
  val _RGBA_ : t [@@js.custom let _RGBA_ = 6408]
  val _RGBA4_ : t [@@js.custom let _RGBA4_ = 32854]
  val _SAMPLER_2D_ : t [@@js.custom let _SAMPLER_2D_ = 35678]
  val _SAMPLER_CUBE_ : t [@@js.custom let _SAMPLER_CUBE_ = 35680]
  val _SAMPLES_ : t [@@js.custom let _SAMPLES_ = 32937]
  val _SAMPLE_ALPHA_TO_COVERAGE_ : t [@@js.custom let _SAMPLE_ALPHA_TO_COVERAGE_ = 32926]
  val _SAMPLE_BUFFERS_ : t [@@js.custom let _SAMPLE_BUFFERS_ = 32936]
  val _SAMPLE_COVERAGE_ : t [@@js.custom let _SAMPLE_COVERAGE_ = 32928]
  val _SAMPLE_COVERAGE_INVERT_ : t [@@js.custom let _SAMPLE_COVERAGE_INVERT_ = 32939]
  val _SAMPLE_COVERAGE_VALUE_ : t [@@js.custom let _SAMPLE_COVERAGE_VALUE_ = 32938]
  val _SCISSOR_BOX_ : t [@@js.custom let _SCISSOR_BOX_ = 3088]
  val _SCISSOR_TEST_ : t [@@js.custom let _SCISSOR_TEST_ = 3089]
  val _SHADER_COMPILER_ : t [@@js.custom let _SHADER_COMPILER_ = 36346]
  val _SHADER_SOURCE_LENGTH_ : t [@@js.custom let _SHADER_SOURCE_LENGTH_ = 35720]
  val _SHADER_TYPE_ : t [@@js.custom let _SHADER_TYPE_ = 35663]
  val _SHADING_LANGUAGE_VERSION_ : t [@@js.custom let _SHADING_LANGUAGE_VERSION_ = 35724]
  val _SHORT_ : t [@@js.custom let _SHORT_ = 5122]
  val _SRC_ALPHA_ : t [@@js.custom let _SRC_ALPHA_ = 770]
  val _SRC_ALPHA_SATURATE_ : t [@@js.custom let _SRC_ALPHA_SATURATE_ = 776]
  val _SRC_COLOR_ : t [@@js.custom let _SRC_COLOR_ = 768]
  val _STATIC_DRAW_ : t [@@js.custom let _STATIC_DRAW_ = 35044]
  val _STENCIL_ATTACHMENT_ : t [@@js.custom let _STENCIL_ATTACHMENT_ = 36128]
  val _STENCIL_BACK_FAIL_ : t [@@js.custom let _STENCIL_BACK_FAIL_ = 34817]
  val _STENCIL_BACK_FUNC_ : t [@@js.custom let _STENCIL_BACK_FUNC_ = 34816]
  val _STENCIL_BACK_PASS_DEPTH_FAIL_ : t [@@js.custom let _STENCIL_BACK_PASS_DEPTH_FAIL_ = 34818]
  val _STENCIL_BACK_PASS_DEPTH_PASS_ : t [@@js.custom let _STENCIL_BACK_PASS_DEPTH_PASS_ = 34819]
  val _STENCIL_BACK_REF_ : t [@@js.custom let _STENCIL_BACK_REF_ = 36003]
  val _STENCIL_BACK_VALUE_MASK_ : t [@@js.custom let _STENCIL_BACK_VALUE_MASK_ = 36004]
  val _STENCIL_BACK_WRITEMASK_ : t [@@js.custom let _STENCIL_BACK_WRITEMASK_ = 36005]
  val _STENCIL_BITS_ : t [@@js.custom let _STENCIL_BITS_ = 3415]
  val _STENCIL_BUFFER_BIT_ : t [@@js.custom let _STENCIL_BUFFER_BIT_ = 1024]
  val _STENCIL_CLEAR_VALUE_ : t [@@js.custom let _STENCIL_CLEAR_VALUE_ = 2961]
  val _STENCIL_FAIL_ : t [@@js.custom let _STENCIL_FAIL_ = 2964]
  val _STENCIL_FUNC_ : t [@@js.custom let _STENCIL_FUNC_ = 2962]
  val _STENCIL_INDEX_ : t [@@js.custom let _STENCIL_INDEX_ = 6401]
  val _STENCIL_INDEX8_ : t [@@js.custom let _STENCIL_INDEX8_ = 36168]
  val _STENCIL_PASS_DEPTH_FAIL_ : t [@@js.custom let _STENCIL_PASS_DEPTH_FAIL_ = 2965]
  val _STENCIL_PASS_DEPTH_PASS_ : t [@@js.custom let _STENCIL_PASS_DEPTH_PASS_ = 2966]
  val _STENCIL_REF_ : t [@@js.custom let _STENCIL_REF_ = 2967]
  val _STENCIL_TEST_ : t [@@js.custom let _STENCIL_TEST_ = 2960]
  val _STENCIL_VALUE_MASK_ : t [@@js.custom let _STENCIL_VALUE_MASK_ = 2963]
  val _STENCIL_WRITEMASK_ : t [@@js.custom let _STENCIL_WRITEMASK_ = 2968]
  val _STREAM_DRAW_ : t [@@js.custom let _STREAM_DRAW_ = 35040]
  val _SUBPIXEL_BITS_ : t [@@js.custom let _SUBPIXEL_BITS_ = 3408]
  val _TEXTURE_ : t [@@js.custom let _TEXTURE_ = 5890]
  val _TEXTURE0_ : t [@@js.custom let _TEXTURE0_ = 33984]
  val _TEXTURE1_ : t [@@js.custom let _TEXTURE1_ = 33985]
  val _TEXTURE2_ : t [@@js.custom let _TEXTURE2_ = 33986]
  val _TEXTURE3_ : t [@@js.custom let _TEXTURE3_ = 33987]
  val _TEXTURE4_ : t [@@js.custom let _TEXTURE4_ = 33988]
  val _TEXTURE5_ : t [@@js.custom let _TEXTURE5_ = 33989]
  val _TEXTURE6_ : t [@@js.custom let _TEXTURE6_ = 33990]
  val _TEXTURE7_ : t [@@js.custom let _TEXTURE7_ = 33991]
  val _TEXTURE8_ : t [@@js.custom let _TEXTURE8_ = 33992]
  val _TEXTURE9_ : t [@@js.custom let _TEXTURE9_ = 33993]
  val _TEXTURE10_ : t [@@js.custom let _TEXTURE10_ = 33994]
  val _TEXTURE11_ : t [@@js.custom let _TEXTURE11_ = 33995]
  val _TEXTURE12_ : t [@@js.custom let _TEXTURE12_ = 33996]
  val _TEXTURE13_ : t [@@js.custom let _TEXTURE13_ = 33997]
  val _TEXTURE14_ : t [@@js.custom let _TEXTURE14_ = 33998]
  val _TEXTURE15_ : t [@@js.custom let _TEXTURE15_ = 33999]
  val _TEXTURE16_ : t [@@js.custom let _TEXTURE16_ = 34000]
  val _TEXTURE17_ : t [@@js.custom let _TEXTURE17_ = 34001]
  val _TEXTURE18_ : t [@@js.custom let _TEXTURE18_ = 34002]
  val _TEXTURE19_ : t [@@js.custom let _TEXTURE19_ = 34003]
  val _TEXTURE20_ : t [@@js.custom let _TEXTURE20_ = 34004]
  val _TEXTURE21_ : t [@@js.custom let _TEXTURE21_ = 34005]
  val _TEXTURE22_ : t [@@js.custom let _TEXTURE22_ = 34006]
  val _TEXTURE23_ : t [@@js.custom let _TEXTURE23_ = 34007]
  val _TEXTURE24_ : t [@@js.custom let _TEXTURE24_ = 34008]
  val _TEXTURE25_ : t [@@js.custom let _TEXTURE25_ = 34009]
  val _TEXTURE26_ : t [@@js.custom let _TEXTURE26_ = 34010]
  val _TEXTURE27_ : t [@@js.custom let _TEXTURE27_ = 34011]
  val _TEXTURE28_ : t [@@js.custom let _TEXTURE28_ = 34012]
  val _TEXTURE29_ : t [@@js.custom let _TEXTURE29_ = 34013]
  val _TEXTURE30_ : t [@@js.custom let _TEXTURE30_ = 34014]
  val _TEXTURE31_ : t [@@js.custom let _TEXTURE31_ = 34015]
  val _TEXTURE_2D_ : t [@@js.custom let _TEXTURE_2D_ = 3553]
  val _TEXTURE_BINDING_2D_ : t [@@js.custom let _TEXTURE_BINDING_2D_ = 32873]
  val _TEXTURE_BINDING_CUBE_MAP_ : t [@@js.custom let _TEXTURE_BINDING_CUBE_MAP_ = 34068]
  val _TEXTURE_CUBE_MAP_ : t [@@js.custom let _TEXTURE_CUBE_MAP_ = 34067]
  val _TEXTURE_CUBE_MAP_NEGATIVE_X_ : t [@@js.custom let _TEXTURE_CUBE_MAP_NEGATIVE_X_ = 34070]
  val _TEXTURE_CUBE_MAP_NEGATIVE_Y_ : t [@@js.custom let _TEXTURE_CUBE_MAP_NEGATIVE_Y_ = 34072]
  val _TEXTURE_CUBE_MAP_NEGATIVE_Z_ : t [@@js.custom let _TEXTURE_CUBE_MAP_NEGATIVE_Z_ = 34074]
  val _TEXTURE_CUBE_MAP_POSITIVE_X_ : t [@@js.custom let _TEXTURE_CUBE_MAP_POSITIVE_X_ = 34069]
  val _TEXTURE_CUBE_MAP_POSITIVE_Y_ : t [@@js.custom let _TEXTURE_CUBE_MAP_POSITIVE_Y_ = 34071]
  val _TEXTURE_CUBE_MAP_POSITIVE_Z_ : t [@@js.custom let _TEXTURE_CUBE_MAP_POSITIVE_Z_ = 34073]
  val _TEXTURE_MAG_FILTER_ : t [@@js.custom let _TEXTURE_MAG_FILTER_ = 10240]
  val _TEXTURE_MIN_FILTER_ : t [@@js.custom let _TEXTURE_MIN_FILTER_ = 10241]
  val _TEXTURE_WRAP_S_ : t [@@js.custom let _TEXTURE_WRAP_S_ = 10242]
  val _TEXTURE_WRAP_T_ : t [@@js.custom let _TEXTURE_WRAP_T_ = 10243]
  val _TRIANGLES_ : t [@@js.custom let _TRIANGLES_ = 4]
  val _TRIANGLE_FAN_ : t [@@js.custom let _TRIANGLE_FAN_ = 6]
  val _TRIANGLE_STRIP_ : t [@@js.custom let _TRIANGLE_STRIP_ = 5]
  val _UNPACK_ALIGNMENT_ : t [@@js.custom let _UNPACK_ALIGNMENT_ = 3317]
  val _UNPACK_COLORSPACE_CONVERSION_WEBGL_ : t [@@js.custom let _UNPACK_COLORSPACE_CONVERSION_WEBGL_ = 37443]
  val _UNPACK_FLIP_Y_WEBGL_ : t [@@js.custom let _UNPACK_FLIP_Y_WEBGL_ = 37440]
  val _UNPACK_PREMULTIPLY_ALPHA_WEBGL_ : t [@@js.custom let _UNPACK_PREMULTIPLY_ALPHA_WEBGL_ = 37441]
  val _UNSIGNED_BYTE_ : t [@@js.custom let _UNSIGNED_BYTE_ = 5121]
  val _UNSIGNED_INT_ : t [@@js.custom let _UNSIGNED_INT_ = 5125]
  val _UNSIGNED_SHORT_ : t [@@js.custom let _UNSIGNED_SHORT_ = 5123]
  val _UNSIGNED_SHORT_4_4_4_4_ : t [@@js.custom let _UNSIGNED_SHORT_4_4_4_4_ = 32819]
  val _UNSIGNED_SHORT_5_5_5_1_ : t [@@js.custom let _UNSIGNED_SHORT_5_5_5_1_ = 32820]
  val _UNSIGNED_SHORT_5_6_5_ : t [@@js.custom let _UNSIGNED_SHORT_5_6_5_ = 33635]
  val _VALIDATE_STATUS_ : t [@@js.custom let _VALIDATE_STATUS_ = 35715]
  val _VENDOR_ : t [@@js.custom let _VENDOR_ = 7936]
  val _VERSION_ : t [@@js.custom let _VERSION_ = 7938]
  val _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ = 34975]
  val _VERTEX_ATTRIB_ARRAY_ENABLED_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_ENABLED_ = 34338]
  val _VERTEX_ATTRIB_ARRAY_NORMALIZED_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_NORMALIZED_ = 34922]
  val _VERTEX_ATTRIB_ARRAY_POINTER_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_POINTER_ = 34373]
  val _VERTEX_ATTRIB_ARRAY_SIZE_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_SIZE_ = 34339]
  val _VERTEX_ATTRIB_ARRAY_STRIDE_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_STRIDE_ = 34340]
  val _VERTEX_ATTRIB_ARRAY_TYPE_ : t [@@js.custom let _VERTEX_ATTRIB_ARRAY_TYPE_ = 34341]
  val _VERTEX_SHADER_ : t [@@js.custom let _VERTEX_SHADER_ = 35633]
  val _VIEWPORT_ : t [@@js.custom let _VIEWPORT_ = 2978]
  val _ZERO_ : t [@@js.custom let _ZERO_ = 0]
end

type context
val context_of_js: Ojs.t -> context
val context_to_js: context -> Ojs.t

type fragment
val fragment_of_js: Ojs.t -> fragment
val fragment_to_js: fragment -> Ojs.t

type vertex
val vertex_of_js: Ojs.t -> vertex
val vertex_to_js: vertex -> Ojs.t

type element
val element_of_js: Ojs.t -> element
val element_to_js: element -> Ojs.t

type array_buffer
val array_buffer_of_js: Ojs.t -> array_buffer
val array_buffer_to_js: array_buffer -> Ojs.t

type shader = private Ojs.t
val shader_of_js: Ojs.t -> shader
val shader_to_js: shader -> Ojs.t

val get_shader_bool_parameter: context -> shader -> Constant.t -> bool [@@js.call "getShaderParameter"]
val create_shader: context -> Constant.t -> shader
val shader_source: context -> shader -> string -> unit
val compile_shader: context -> shader -> unit
val get_shader_info_log: context -> shader -> string

type program = private Ojs.t
val program_of_js: Ojs.t -> program
val program_to_js: program -> Ojs.t
val create_program: context -> program[@@js.call]
val attach_shader: context -> program -> shader -> unit
val link_program: context -> program -> unit
val get_program_info_log: context -> program -> string
val use_program: context -> program -> unit

val get_program_bool_parameter: context -> program -> Constant.t -> bool [@@js.call "getProgramParameter"]
val get_attrib_location: context -> program -> string -> int

type uniform_location = private Ojs.t
val uniform_location_of_js: Ojs.t -> uniform_location
val uniform_location_to_js: uniform_location -> Ojs.t
val get_uniform_location: context -> program -> string -> uniform_location option

type buffer = private Ojs.t
val buffer_of_js: Ojs.t -> buffer
val buffer_to_js: buffer -> Ojs.t
val create_buffer: context -> buffer[@@js.call]

type data = private Ojs.t
val data_of_js: Ojs.t -> data
val data_to_js: data -> Ojs.t

val bind_buffer: context -> Constant.t -> buffer -> unit
val buffer_data: context -> Constant.t -> Ojs.t -> Constant.t -> unit

val enable_vertex_attrib_array: context -> int -> unit
val disable_vertex_attrib_array: context -> int -> unit
val vertex_attrib_pointer: context -> int -> int -> Constant.t -> bool -> int -> int -> unit

val clear: context -> Constant.t -> unit
val clear_color: context -> float -> float -> float -> float -> unit

val line_width: context -> float -> unit

val draw_arrays: context -> Constant.t -> int -> int -> unit
val draw_elements: context -> Constant.t -> int -> Constant.t -> int -> unit

val uniform4f: context -> uniform_location -> float -> float -> float -> float -> unit
val uniform3f: context -> uniform_location -> float -> float -> float -> unit
val uniform2f: context -> uniform_location -> float -> float -> unit
val uniform1f: context -> uniform_location -> float -> unit
val uniform_matrix4fv: context -> uniform_location -> bool -> Float32Array.t -> unit
val uniform_matrix3fv: context -> uniform_location -> bool -> Float32Array.t -> unit

val enable: context -> Constant.t -> unit
val is_enabled: context -> Constant.t -> bool
val disable: context -> Constant.t -> unit
val depth_func: context -> Constant.t -> unit

val cull_face: context -> Constant.t -> unit

val blend_func: context -> Constant.t -> Constant.t -> unit
val depth_mask: context -> bool -> unit
val color_mask: context -> bool -> bool -> bool -> bool -> unit

type framebuffer = private Ojs.t
val framebuffer_of_js: Ojs.t -> framebuffer
val framebuffer_to_js: framebuffer -> Ojs.t
val create_framebuffer: context -> framebuffer[@@js.call]
val bind_framebuffer: context -> Constant.t -> framebuffer option -> unit

type renderbuffer = private Ojs.t
val renderbuffer_of_js: Ojs.t -> renderbuffer
val renderbuffer_to_js: renderbuffer -> Ojs.t
val create_renderbuffer: context -> renderbuffer[@@js.call]
val bind_renderbuffer: context -> Constant.t -> renderbuffer -> unit
val renderbuffer_storage: context -> Constant.t -> Constant.t -> int -> int -> unit
val framebuffer_renderbuffer: context -> Constant.t -> Constant.t -> Constant.t -> renderbuffer -> unit


type texture = private Ojs.t
val texture_of_js: Ojs.t -> texture
val texture_to_js: texture -> Ojs.t
val create_texture: context -> texture[@@js.call]

val bind_texture: context -> Constant.t -> texture option -> unit
val tex_image_2D: context -> Constant.t -> int -> Constant.t -> Constant.t -> Constant.t -> ([`Canvas of (* <canvas> *) Js_windows.Element.t | `Bytes of Uint8Array.t][@js.union]) -> unit
val tex_image_2D_array: context -> Constant.t -> int -> Constant.t -> int -> int -> int -> Constant.t -> Constant.t -> ([`Bytes of Uint8Array.t][@js.union]) option -> unit [@@js.call "texImage2D"]
val framebuffer_texture_2D: context -> Constant.t -> Constant.t -> Constant.t -> texture -> int -> unit
val generate_mipmap: context -> Constant.t -> unit

val tex_parameteri: context -> Constant.t -> Constant.t -> Constant.t -> unit

type context_type =
  | WebGl [@js "webgl"]
  | Experimental [@js "experimental-webgl"]
  [@@js.enum]

type context_attribute = {
  alpha: bool option;
  depth: bool option;
  stencil: bool option;
  antialias: bool option;
}

val get_supported_extensions: context -> string array
val get_extension: context -> string -> Ojs.t option
val viewport: context -> int -> int -> int -> int -> unit

val get_context: ?context_attribute:context_attribute -> (* <canvas> *) Js_windows.Element.t -> context_type -> context option
  [@@js.custom
      val get_context: Js_windows.Element.t -> context_type -> context_attribute -> context option[@@js.call]
      let default_context_attribute = {
        alpha = Some true;
        depth = Some true;
        stencil = None;
        antialias = Some false;
      }
      let get_context ?(context_attribute = default_context_attribute) canvas context_type =
        get_context canvas context_type context_attribute]
