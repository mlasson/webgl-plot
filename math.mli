val debug : bool
module List :
  sig
    val length : 'a list -> int
    val cons : 'a -> 'a list -> 'a list
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list
    val nth : 'a list -> int -> 'a
    val rev : 'a list -> 'a list
    val append : 'a list -> 'a list -> 'a list
    val rev_append : 'a list -> 'a list -> 'a list
    val concat : 'a list list -> 'a list
    val iter : ('a -> unit) -> 'a list -> unit
    val iteri : (int -> 'a -> unit) -> 'a list -> unit
    val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
    val rev_map : ('a -> 'b) -> 'a list -> 'b list
    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
    val fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
    val iter2 : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
    val map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val rev_map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
    val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b list -> 'c list -> 'a
    val fold_right2 :
      ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
    val for_all : ('a -> bool) -> 'a list -> bool
    val exists : ('a -> bool) -> 'a list -> bool
    val for_all2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val exists2 : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val mem : 'a -> 'a list -> bool
    val memq : 'a -> 'a list -> bool
    val find : ('a -> bool) -> 'a list -> 'a
    val filter : ('a -> bool) -> 'a list -> 'a list
    val find_all : ('a -> bool) -> 'a list -> 'a list
    val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
    val assoc : 'a -> ('a * 'b) list -> 'b
    val assq : 'a -> ('a * 'b) list -> 'b
    val mem_assoc : 'a -> ('a * 'b) list -> bool
    val mem_assq : 'a -> ('a * 'b) list -> bool
    val remove_assoc : 'a -> ('a * 'b) list -> ('a * 'b) list
    val remove_assq : 'a -> ('a * 'b) list -> ('a * 'b) list
    val split : ('a * 'b) list -> 'a list * 'b list
    val combine : 'a list -> 'b list -> ('a * 'b) list
    val sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val stable_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val fast_sort : ('a -> 'a -> int) -> 'a list -> 'a list
    val sort_uniq : ('a -> 'a -> int) -> 'a list -> 'a list
    val merge : ('a -> 'a -> int) -> 'a list -> 'a list -> 'a list
    val choose : ('a -> 'b option) -> 'a list -> 'b list
    val flatten : 'a list list -> 'a list
    val map : ('a -> 'b) -> 'a list -> 'b list
  end
type four
type three
type two
val pi : float
val sq : float -> float
module Vector :
  sig
    type 'a vector = private float array
    type ('a, 'b) times
    type ('a, 'b) matrix = ('a, 'b) times vector
    type 'a square_matrix = ('a, 'a) matrix
    val of_two : float * float -> two vector
    val of_three : float * float * float -> three vector
    val of_four : float * float * float * float -> four vector
    val of_array :
      float array ->
      [ `Four of four vector | `Three of three vector | `Two of two vector ]
    val to_two : two vector -> float * float
    val to_three : three vector -> float * float * float
    val to_four : four vector -> float * float * float * float
    val to_array : 'a vector -> float array
    val to_string : 'a vector -> string
    val add : 'a vector -> 'a vector -> 'a vector
    val sub : 'a vector -> 'a vector -> 'a vector
    val mul : 'a vector -> 'a vector -> 'a vector
    val sum : 'a vector -> float
    val dot : 'a vector -> 'a vector -> float
    val norm : 'a vector -> float
    val dist : 'a vector -> 'b vector -> float
    val scale : float -> 'a vector -> 'a vector
    val normalize : 'a vector -> 'a vector
    val four_to_three : four vector -> three vector
    val cross : three vector -> three vector -> three vector
    val flatten : 'a vector list -> float array
    val multiply :
      (four, four) matrix -> (four, four) matrix -> (four, four) matrix
    val multiply_vector : (four, four) matrix -> four vector -> four vector
    module Const :
      sig
        val scale : three vector -> four square_matrix
        val translation : three vector -> four square_matrix
        val scale_translation :
          three vector -> three vector -> four square_matrix
        val x_rotation : float -> four square_matrix
        val y_rotation : float -> four square_matrix
        val z_rotation : float -> four square_matrix
        val identity : four square_matrix
        val flip : four square_matrix
        val projection :
          fov:float ->
          aspect:float -> near:float -> far:float -> four square_matrix
        val inverse_projection :
          fov:float ->
          aspect:float -> near:float -> far:float -> four square_matrix
      end
  end
type vec2 = two Vector.vector
type vec3 = three Vector.vector
type vec4 = four Vector.vector
type mat4 = (four, four) Vector.matrix
module Buffer :
  sig
    val iteri : (int -> float -> unit) -> Webgl.Float32Array.t -> unit
    val iter_generic :
      int -> Webgl.Float32Array.t -> (float array -> unit) -> unit
    val vec3_of_array : float array -> three Vector.vector
    val iter3 : Webgl.Float32Array.t -> (three Vector.vector -> unit) -> unit
    val number_of_triangles : Webgl.Uint16Array.t -> int
    val iter_triangles :
      ?chunk_size:int ->
      Webgl.Uint16Array.t ->
      (int * int * int -> unit) -> unit Asynchronous_computations.t
    val get_generic : float array -> Webgl.Float32Array.t -> int -> unit
    val get3 : Webgl.Float32Array.t -> int -> three Vector.vector
  end
module Color :
  sig val hsv : float -> float -> float -> three Vector.vector end
