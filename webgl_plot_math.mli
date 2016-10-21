(* This file is released under the terms of an MIT-like license.     *)
(* See the attached LICENSE file.                                    *)
(* Copyright 2016 by LexiFi.                                         *)

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

module Color :
  sig
    val hsv : float -> float -> float -> float * float * float
    val cold_to_hot: float -> float * float * float
    val white_cold_to_hot: float -> float * float * float
  end
