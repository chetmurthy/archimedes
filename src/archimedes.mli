(** A 2D plotting library with various backends.

    @version 0.3.2
    @author Christophe Troestler
    @author Pierre Hauweele
    @author Fabian Pijcke
    @author No�mie Meunier
    @author Bertrand Desmons
*)

(** {2 Introduction}

    {[
    module A = Archimedes
    let vp = A.init "graphic hold" in
    A.Axes.box vp;
    A.fx vp sin 0. 10.;
    A.close vp
    ]}

*)

(** Abstract representation of colors (suitable for RGBA). *)
module Color :
sig

  type t
  (** The type for colors*)

  val rgb : float -> float -> float -> t
  (** [rgb r g b] creates the color with transparency [~a], red
      component [r], green component [g] and blue component [b]. All
      values must be between [0.] and [1.]; raises [Invalid_argument]
      otherwise. *)

  val rgba : float -> float -> float -> float -> t
  (** [rgba r g b a] creates the color with transparency [~a], red
      component [r], green component [g] and blue component [b]. All values
      must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

  val r : t -> float
  (** Returns the red component of a color.*)

  val g : t -> float
  (** Returns the green component of a color.*)

  val b : t -> float
  (** Returns the blue component of a color.*)

  val a : t -> float
  (** Returns the transparency (alpha) component of a color.*)

  val get_rgb : t -> float * float * float
  (** Equivalent to ([r t],[g t],[b t]).*)

  val get_rgba : t -> float * float * float * float
  (** Equivalent to ([r t],[g t],[b t], [a t]).*)

  val black : t
  val red : t
  val green : t
  val blue : t
  val yellow : t
  val magenta : t
  val cyan : t
  val white : t
  (** Predefined colors.*)

  (** {2 Merging colors} *)

  (** Different ways of merging colors.  See
      http://cairographics.org/operators/ for more explanations.*)
  type operator =
  | Over (** Transparency and color components are mixed in such a way
             that it corresponds to putting the second color over the first*)
  | Source (** First color completely ignored. *)
  | Clear (** Inhibits all colors *)
  | In (** RGB components as the second color, A component product of
           the two A components. So, a transparent color result if the
           first one was transparent.*)
  | Out (** RGB components as the second color, A component product of
            the second A component with (1 - A) first component. So, a
            transparent color result if the first one was opaque.*)
  | Atop (** Transparency of the first color is the final transparency;
             mixes RGB components.*)
  | Dest (** Second color completely ignored. (<-> SOURCE)*)
  | Dest_Over (** Transparency and color components are mixed in such a
                  way that it corresponds to putting the first color over the
                  second. (<-> OVER)*)
  | Dest_In (** RGB components as the first color, A component product of
                the two A components. So, a transparent color result if the
                second one was transparent. (<-> IN)*)
  | Dest_Out (** RGB components as the first color, A component product
                 of the first A component with (1 - A) second
                 component. So, a transparent color result if the
                 second one was opaque. (<-> OUT)*)
  | Dest_Atop (** Transparency of the second color is the final transparency;
                  mixes RGB components. (<-> ATOP)*)
  | Xor (** Same mix of color than OVER, but transparency will be more
            important.*)
  | Add (** RGB components: ponderated sum of RGB components, with
            transparency. Resulting A is the sum of transparencies
            (bounded to 1. if necessary).*)
  | Saturate (** Same as ADD, but the sum for RGB components shrinks
                 the ponderation the first color components (coeff:
                 min (first A, 1 - second A)) *)


  val add : ?op:operator -> t -> t -> t
  (** Adds the first color to the second color, according to the
      operator [op] (default : [Over]).*)



end
(*----------------------------------------------------------------------*)
(** {2 Affine transformations} *)


(** Module implementing affine transformations and various operations
    on them. *)
module Matrix :
sig


  (** Holds an affine transformation, such as a scale, rotation, shear,
      or a combination of those. The transformation of a point (x, y) is
      given by:
      {[
      x_new = xx *. x +. xy *. y +. x0;
      y_new = yx *. x +. yy *. y +. y0;      ]} *)
  type t = { mutable xx: float; mutable yx: float;
             mutable xy: float; mutable yy: float;
             mutable x0: float; mutable y0: float; }

  exception Not_invertible

  val make_identity : unit -> t
  (** [make_identity()] returns the identity transformation. *)

  val make_translate : x:float -> y:float -> t
  (** [make_translate tx ty] returns a transformation that translates
      by [tx] and [ty] in the X and Y dimensions, respectively. *)

  val make_scale : x:float -> y:float -> t
  (** [make_scale sx sy] returns a transformation that scales by [sx]
      and [sy] in the X and Y dimensions, respectively. *)

  val make_rotate : angle:float -> t
  (** [make_rotate radians] returns a transformation that rotates
      by [radians]. *)

  val set_to_identity : t -> unit
  (** Sets the current transformation to the identity transformation. *)

  val copy: t -> t
  (** [copy matrix] returns a copy of [matrix]. *)

  val blit : t -> t -> unit
  (** [blit m1 m2] copies the content of [m1] into [m2]. *)

  val translate : t -> x:float -> y:float -> unit
  (** [translate m tx ty] applies a translation by [tx], [ty] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} translate the coordinates by [tx] and [ty],
      then apply the original transformation to the coordinates. *)

  val scale : t -> x:float -> y:float -> unit
  (** [scale m sx sy] applies scaling by [sx], [sy] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} scale the coordinates by [sx] and [sy], then
      apply the original transformation to the coordinates. *)

  val rotate : t -> angle:float -> unit
  (** [rotate m radians] applies rotation by [radians] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} rotate the coordinates by [radians], then
      apply the original transformation to the coordinates. *)

  val invert : t -> unit
  (** [invert m] changes [matrix] to be the inverse of it's original
      value.  Not all transformation matrices have inverses; if the
      matrix collapses points together (it is degenerate), then it
      has no inverse and this function will raise
      {!Matrix.Not_invertible}. *)

  val det : t -> float
  (** [det m] returns the determinant of the linear part of [m].  It
      is the (signed) area that gets the unit square after
      transformation.  *)

  val mul : t -> t -> t
  (** [multiply b a] multiplies the affine transformations in [a]
      and [b] together and return the result.  The effect of the
      resulting transformation is to {i first} apply the
      transformation in [a] to the coordinates and then apply the
      transformation in [b] to the coordinates.

      BEWARE that the order of the arguments is different from
      e.g. [Cairo.Matrix.multiply]. *)

  val mul_in : t -> t -> t -> unit
  (** [mul_in c b a] computes [mul b a] and put the result in [c]. *)

  val transform_point : t -> x:float -> y:float -> float * float
  (** [transform_point m x y] transforms the point ([x], [y]) by [m]. *)

  val transform_distance : t -> dx:float -> dy:float -> float * float
  (** [transform_distance m dx dy] transforms the distance vector
      ([dx],[dy]) by [m].  This is similar to
      {!Matrix.transform_point} except that the translation
      components of the transformation are ignored.  The calculation
      of the returned vector is as follows:
      {[
      dx2 = dx1 * xx + dy1 * xy;
      dy2 = dx1 * yx + dy1 * yy;
      ]}
      Affine transformations are position invariant, so the same
      vector always transforms to the same vector.  If (x1,y1)
      transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
      (x2+dx2,y2+dy2) for all values of dx1 and dy1.  *)

  val inv_transform_point : t -> x:float -> y:float -> float * float
  (** Makes the inverse transformation of a point. *)

  val inv_transform_distance : t -> dx:float -> dy:float -> float * float
  (** Makes the inverse transformation of a distance. *)

  val has_shear: t -> bool
  (** Tests whether the transformation has shears.  This is also the
      case if the transformation does a rotation.  *)

  (** A data structure for holding a rectangle. *)
  type rectangle = {
    x:float;   (** X coordinate of the left side of the rectangle *)
    y:float;   (** Y coordinate of the the top side of the rectangle  *)
    w:float;   (** width of the rectangle *)
    h:float;   (** height of the rectangle  *)
  }

  val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
  (** Transformation of rectangles. This returns the smallest
      rectangle containing the transformation of the rectangle argument
      by the matrix. The optional argument [dist_basepoint] has the
      following meaning:

      - Not specified: transform the base point as a point.
      - Specified as [true]: transform the base point as a distance.
      - Specified as [false]: no transformation of the base point.*)



end
(*----------------------------------------------------------------------*)
(** {2 Registering backends and extending the library} *)


(** Module providing a uniform interface and managing the dynamic
    loading of the backends.  This modules is only useful to create
    new backends and should not be used for plotting data. *)
module Backend :
sig

  type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

  type line_join =
  | JOIN_MITER (** use a sharp (angled) corner *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

  type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
  | CB  (** center horizontally and align bottom vertically *)
  | LT  (** align left horizontally and top vertically *)
  | LB  (** align left horizontally and bottom vertically *)
  | RT  (** align right horizontally and top vertically *)
  | RB  (** align right horizontally and bottom vertically *)


  (** Specifies variants of a font face based on their slant. *)
  type slant = Upright | Italic

  (** Specifies variants of a font face based on their weight. *)
  type weight = Normal | Bold

  (** The interface that backends must provide to be registered. *)
  module type T =
  sig
    type t
    (** Handle to a backend. *)

    val set_color : t -> Color.t -> unit
    (** [set_color bk c] sets the color of the backend [bk] to [c]. *)
    val set_line_width : t -> float -> unit
    (** [set_line_width bk w] sets the line width of the backend [bk]
        to [w].  The line width is expressed in the current backend
        coordinates (at the time of stroking). *)
    val set_line_cap : t -> line_cap -> unit
    (** [set_line_cap bk c] sets the line cap for the backend [bk] to [c]. *)
    val set_dash : t -> float -> float array -> unit
    (** [set_dash bk ofs pattern] *)
    val set_line_join : t -> line_join -> unit
    (** [set_line_join bk j] sets the line join for the backend [bk]
        to [j]. *)

    val get_line_width: t -> float
    val get_line_cap: t -> line_cap
    val get_dash: t -> float array * float
    val get_line_join: t -> line_join

    val move_to : t -> x:float -> y:float -> unit
    val line_to : t -> x:float -> y:float -> unit
    val rel_move_to : t -> x:float -> y:float -> unit
    val rel_line_to : t -> x:float -> y:float -> unit

    val curve_to : t ->
      x1:float -> y1:float ->
      x2:float -> y2:float ->
      x3:float -> y3:float -> unit
    (** [curve_to bk x1 y1 x2 y2 x3 y3] adds an Bezier curve to the
        path, starting at the current point, ending at point
        [(x3,y3)], with control points [(x1,y1)] and [(x2,y2)]. *)

    val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (** [rectangle bk x y w h] adds to the current path of [bk] a
        rectangle whose lower left corner is at [(x,y)] and width
        and height are respectively [w] and [h]. *)

    val arc : t -> r:float -> a1:float -> a2:float -> unit
    (** [arc bk r a1 a2] add an arc to the current path starting from
        the current point with a radius [r], starting at angle [a1]
        and going clockwise to angle [a2]. *)

    val close_path : t -> unit
    (** Adds a line segment to the path from the current point to
        the beginning of the current sub-path (the most recent point
        passed to {!Archimedes.Backend.T.move_to}) and closes this
        sub-path. *)
    val clear_path : t -> unit
    (** Clears the current path. After this call there will be no
        path.  Nothing is guaranteed about the current point (it may
        not be preserved). *)
    val path_extents : t -> Matrix.rectangle

    val stroke : t -> unit
    val stroke_preserve : t -> unit
    val fill : t -> unit
    val fill_preserve : t -> unit

    val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (** Establishes a new clip rectangle by intersecting the current
        clip rectangle.  This {i may clear} the current path. *)

    val save : t -> unit
    (** Save the current state of the backend.  Note that
        save/restore must not affect the current path. *)
    val restore : t -> unit
    (** Restore the saved state of the backend. *)

    val translate : t -> x:float -> y:float -> unit
    (** [translate cr tx ty] modifies the current transformation
        matrix by translating the user-space origin by ([tx],[ty]). *)
    val scale : t -> x:float -> y:float -> unit
    (** [scale sx sy] modifies the current transformation matrix by
        scaling the X and Y user-space axes by [sx] and [sy]
        respectively. *)
    val rotate : t -> angle:float -> unit
    (** Modifies the current transformation matrix by rotating the
        user-space axes by [angle] radians. *)
    val set_matrix : t -> Matrix.t -> unit
    (** Set the current transformation matrix which is the matrix
        transorming user to device coordinates. *)
    val get_matrix : t -> Matrix.t
    (** Return the current transformation matrix.  Modifying this
        matrix does not affect the matrix held in [t]. *)
    val flipy : t -> bool
    (** [true] iff this kind of device has its Y axis pointing
        downwards.
        FIXME: really needed ?  Beware that on some devices, the font
        display happens in the current coordinates. *)

    val select_font_face : t -> slant -> weight -> string -> unit
    (** [select_font_face t slant weight family] selects a family
        and style of font from a simplified description as a family
        name, slant and weight.  Family names are bakend dependent.
        Raise an exception if the face is not supported. *)
    val set_font_size : t -> float -> unit
    (** Set the scaling of the font. *)
    val text_extents : t -> string -> Matrix.rectangle
    (** Returns a rectangle whose width and height specify
        respectively the length and the height of the text. The x and
        y values give the lower bottom point of the rectangle as if
        the text was placed at the origin.*)
    val show_text : t -> rotate:float -> x:float -> y:float ->
      text_position -> string -> unit
        (** [show_text t angle x y pos txt] displays [txt] at the point
            ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
            the current coordinate system but the current transformation
            matrix will NOT be applied to the text itself.  [angle]
            indicates by how many radians the text must be rotated
            w.r.t. the x-axis (in the current coordinate system, assuming
            it is orthonormal) -- not all device support rotations of
            angles [<> 0.] (in device coordinates).  This is an immediate
            operation: no [stroke] nor [fill] are required (nor will have
            any effect).  *)
  end

  type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string * Dynlink.error
  | Nonexistent of string  (** Cannot find the backend in the directories *)
  | Not_loadable of string * Dynlink.error
      (** Cannot load the backend because of the dynlink error. *)
  | Not_registering of string (** Not applying the {!Backend.Register}
                                  functor. *)

  exception Error of error * string
  (** Exception raised when a backend cannot be loaded. *)

  include T

  val make : ?dirs:string list -> string -> float -> float -> t
    (** [make backend width height] creates a new backend of the given
        dimensions.  The units of the dimensions are backend dependent.

        [backend] is the name of the underlying engine, followed by one
        or several options separated by spaces.  For example, "Graphics"
        for the graphics backend or "Cairo PNG filename" for the Cairo
        backend, using a PNG surface to be saved to [filename]. *)

  val close : t -> unit
    (** Close the handle.  For some backends, the output will not be
        complete until this function is called. *)

  val height : t -> float
    (** Returns the width of the backend canvas. *)

  val width : t -> float
    (** Returns the height of the backend canvas. *)


  val registered: unit -> string list
    (** Return the list of registered (i.e. loaded) backends. *)

  val available : dirs:string list -> string list
      (** Return the list of available backends in the given directories. *)


  (************************************************************************)
  (** {2 Registering new modules} *)

  module type Capabilities =
  sig
    include T

    val name : string
      (** Name under which to register the backend. *)

    val make : options:string list -> float -> float -> t
      (** [create options width height] must creates a new handle of
          size [width]�[height] (in units proper to the module) on which
          the subsequent drawing functions operate.  [options] allows to
          pass options to the backend (this is backend specific). *)

    val close : options:string list -> t -> unit
    (** Close the handle.  This function will be given the options
        specified at backend creation so it can react appropriately if
        some final work need to be done for some of them. *)
  end

    module Register(B: Capabilities) : sig end
      (** The {i side effect} of this functor application is to register
          the functions of the backend [B] under the name [B.name].

          A backend [B] must be declared in a file archimedes_[B.name]
          (compiled to a .cmo and/or .cmxs library) and the functor
          application must be executed as part of the initialisation code.
          We recommend the use of [let module U = Register(B) in ()] to
          perform the registration.  *)

end

(** Affine systems of coordinates relative to other coordinate systems
    with automatic updates.  The automatic update refers to the fact
    that, if a coordinate system is upated, all coordinate systems
    which depend on it (possibly through several intermediate
    coordinate systems), they will use the updated version. *)
module Coordinate :
sig

  type t
  (** Mutable affine coordinate system. *)

  type ctm
  (** Current transformation matrix of the backend (to be able to
      restore it with {!Coordinate.restore}. *)

  val use : Backend.t -> t -> ctm
  (** After a call to [use b c], all backend operations will be
      performed in the coordinates [c].  It returns the current
      coordinate system so one can restore it with
      {!Coordinate.restore}. *)

  val restore : Backend.t -> ctm -> unit
  (** [restore b c] restore the coordinate transformation matrix [ctm]
      for the backend [b]. *)


  (** {2 Transforming coordinates} *)

  val to_parent : t -> x:float -> y:float -> float * float
  (** [to_parent coord x y] returns the location of the point [(x,y)]
      in parent's coordinates.*)

  val from_parent : t -> x:float -> y:float -> float * float
  (** [from_child coord x y] returns the location of the point [(x,y)]
      from parent's coordinates. *)

  val to_device : t -> x:float -> y:float -> float * float
  (** [to_device coord x y] returns the location of the point [(x,y)]
      in device coordinates.*)

  val to_device_distance : t -> dx:float -> dy:float -> float * float
  (** [to_device coord dx dy] returns the distance [(dx,dy)] in device
      coordinates (i.e. the translation in [coord] is ignored).  *)

  val to_coord : t -> x:float -> y:float -> float * float
  (** [to_coord coord x y] converts the (device) point [(x,y)] into
      the corresponding point, expressed in [coord] coordinates. *)

  val to_coord_distance : t -> dx:float -> dy:float -> float * float
  (** [to_coord coord x y] converts the (device) distance [(dx,dy)]
      into the corresponding distance, expressed in [coord]
      coordinates. *)


  (** {2 Creating new coordinate systems} *)

  val make_root : Matrix.t -> t
  (** [make_root m] make a system of coordinates which, when used,
      amounts to use [m].  This coordinate system depends on no
      other  so will never be updated.  It can be modified however
      (the matrix [m] is copied so no modification will affect [m]). *)

  val make_identity : t -> t
  (** [make_identity coord] defines a new system of coordinates that
      initially consist in the identity transformation to [coord]. *)

  val make_translate : t -> x:float -> y:float -> t
  (** [make_translate coord x y] defines a new coordinate system that
      consists in moving the origin of [coord] to the point [(x,y)]
      (understood as coordinates in the system [coord]).  If [coord]
      is modified, the new system will be updated as well. *)

  val make_scale : t -> x:float -> y:float -> t
  (** [make_scale coord x y] defines a new coordinate system that
      consists in dilating axis X and Y of [coord] by a factor of [x]
      and [y] respectively.  If [coord] is modified, the new system
      will be updated as well. *)

  val make_rotate : t -> angle:float -> t
  (** [make_rotate coord a] defines a new coordinate system that
      consists in rotating the axis X and Y of [coord] by [a] radians
      (assuming the axis of the system [coord] are orthonormal).  If
      [coord] is modified, the new system will be updated as well.  *)

  val make_from_transform : t -> Matrix.t -> t
  (** [make_from_transform coord tm] defines a new coordinate system
      that consists first in applying [tm] and then the tranformation
      in [coord].  In other words, [tm] is the affine transformation
      from the desired coordinate system to [coord].  If [coord] is
      modified, the new system will be updated as well. *)

  val copy : t -> t
  (** Returns a completely independent copy of the current coordinate
      system. *)


  (** {2 Modifying this coordinate system} *)

  val translate : t -> x:float -> y:float -> unit
  (** [translate coord x y] modifies the coordinate system [coord]
      translating its origin to the point [(x,y)] (understood as
      coordinates in the system [coord]). *)

  val scale : t -> x:float -> y:float -> unit
  (** [scale coord x y] modifies the coordinate system [coord]
      dilating its axis X and Y by a factor of [x] and [y]
      respectively. *)

  val rotate : t -> angle:float -> unit
  (** [rotate coord a] modifies the coordinate system [coord] rotating
      its axis X and Y by [a] radians (assuming the axis of the system
      [coord] are orthonormal). *)

  val transform : t -> Matrix.t -> unit
  (** [transform coord tm] modifies the coordinate system [coord]
      changing the transformation matrix to its parent (the one it was
      created from) to [tm]. *)


  (** {2 Monitoring coordinate systems for updates} *)

  type monitor
  (** Handle to monitor the updates to a coordinate system. *)

  val monitor : t -> monitor
  (** [monitor coord] creates a new monitor for changes to [coord]
      (initially not set). *)

  val reset : monitor -> unit
  (** [reset m] reset the monitor.  See {!Coordinate.changed}. *)

  val changed : monitor -> bool
  (** [changed m] tell whether the coordinate system [m] is attached
      to was updated (possibly because of one of the coordinate systems
      it (transitively) depends on was mofidied) since the last [reset]. *)

end

(** Module handling point styles and marks. *)
module Pointstyle :
sig

  exception Error of string
    (**Raised for undefined point styles.*)

  type name = string
  (** Point styles are identified by strings. *)

  val add : name:name -> (Backend.t -> unit) -> Matrix.rectangle -> unit
  (** [add name f extents] adds to the existing point styles, a new
      point style, referenced under the name [name]. This point style
      is made using the function [f]; the extents it takes is given by
      [extents]. The behaviour of adding a new point style whose name
      is already used by another is the same as the core [Map.S.add]
      (that is, the previous binding disappears).*)

  val names : unit -> name list
  (** @return a list of all names declared. *)

end



(** Creating abstract paths. *)
module Path :
sig

  type t

  val make: unit -> t
  (** [make ()] creates a new empty path *)

  val copy: t -> t
  (** [copy p] copies a path *)

  val make_at: float -> float -> t
  (** [make x y] creates a new empty path and moves it to ([x], [y]) *)

  val clear: t -> unit
  (** [clear p] clears the path (removes all operations) *)

  val extents: t -> Matrix.rectangle
  (** [extents p] returns a copy of the path's extents *)

  val move_to: t -> x:float -> y:float -> unit
  (** [move_to p x y] moves the path's current point to ([x], [y]) *)

  val line_to: t -> x:float -> y:float -> unit
  (** [line_to p x y] draws a line from the path's current point to ([x],
      [y]) and sets the current point to ([x], [y]) *)

  val rel_move_to: ?rot:float -> t -> x:float -> y:float -> unit
  (** [rel_move_to p x y] shifts the path's current point of [x]
      horizontally and [y] vertically.

      @param rot to consider a rotation of [rot] radians (default: [0.]). *)

  val rel_line_to: ?rot:float -> t -> x:float -> y:float -> unit
  (** [rel_line_to p x y] shifts the path's current point of [x]
      horizontally and [y] vertically and draws a line between the
      current and the new point.

      @param rot (default: 0.) to consider a rotation of [rot] radians *)

  val rectangle: t -> x:float -> y:float -> w:float -> h:float -> unit
  (** [rectangle p x y w h] draws a rectangle specified by ([x], [y], [w],
      [h]) but does not move the path's current point. *)

  val curve_to: t -> x1:float -> y1:float -> x2:float -> y2:float ->
    x3:float -> y3:float -> unit
  (** [curve_to p x1 y1 x2 y2 x3 y3] draws a cubic Bezier curve using the
      path's current point as first point (x0, y0) if it is set, else
      ([x1, y1]).  Sets the path's current point to ([x3], [y3]) *)

  val arc: t -> r:float -> a1:float -> a2:float -> unit
  (** [arc p r a1 a2] draws an arc starting at the path's current
      point. The starting angle is [a1], the radius [r] and the arc is
      drawn clockwise to the angle [a2]. The angles are given in
      radians.  *)

  val close: t -> unit
  (** [close p] Closes the path. It is usually not required to close a
      path, this is useful only to ensure the path won't be extended. *)

  val stroke_on_backend: ?limits:float * float * float * float ->
    t -> Backend.t -> unit
  (** [stroke_on_backend p bk] strokes the path [p] on the backend
      [bk]. It will not clear the path [p] but will clear the path of
      [bk].

      @param limits a quartet of float indicating where to clip the
      path. Default: the unit rectangle. One can use (-infinity,
      -infinity, infinity, infinity) for no clip. *)

  val fill_on_backend: ?limits:float * float * float * float ->
    t -> Backend.t -> unit
  (** [fill_on_backend p bk] fills in the path [p] on the backend
      [bk]. It will not clear the path [p] but will clear the path of
      [bk].

      @param limits a quartet of float indicating where to clip the
      path. Default: the unit rectangle. One can use (neg_infinity,
      neg_infinity, infinity, infinity) for no clip. *)

  val current_point: t -> float * float
  (** [current_point p] returns the current point of the path *)

  val transform: t -> (float * float -> float * float) -> t
  (** [transform p f] returns a new path that is the path [p] transformed by
      function [f]. It only modifies end points of paths primitives and
      extents are leaved the same. *)

  val print_path: t -> unit
  (** [print_path p] Debug function (TODO hide it) *)

  val add: t -> t -> unit
  (** [add p to_add] Adds the path [to_add] to the end of [p] *)

end


(** Tics position and labels. *)
module Tics :
sig


  type labels =
  | No_label
  | Text of (string * float) array (* TODO use lists *)
  | Number of int
  | Expnumber of float
  | Expnumber_named of float * string
  | Custom of (float -> string option) (* TODO no option needed *)

  type tic =
  | Major of string option * float
  | Minor of float

  type t =
  | Fixed of labels * float list
  | Fixed_norm of labels * float list
  | Equidistants of labels * float * float * int
  | Auto of labels

  val tics: bool -> float -> float -> t -> tic list

end



(** Area on which graphs can be made. *)
module Viewport :
sig

  type t
  (** Viewport handle. *)

  type coord_name = Device | Graph | Data | Orthonormal

  val get_coord_from_name : t -> coord_name -> Coordinate.t
  (** [get_coord_from_name viewport coord_name] returns one of the
      coordinate systems of the viewport *)

  (** {2 Create new viewports} *)

  val make : ?lines:float -> ?text:float -> ?marks:float ->
    t -> coord_name -> float -> float -> float -> float ->
    (t -> float -> float -> unit) -> t
  (** [make parent coord_name xmin xmax ymin ymax] creates and returns a
      viewport on top of [parent] with top left corner (xmin, ymin) and
      bottom right corner (xmax, ymax) using parent's [coord_name]
      coordinate system.

      @param lines see {!init}

      @param text see {!init}

      @param marks see {!init}
  *)

  val get_backend : t -> Backend.t
  (** [get_backend vp] returns the backend associated to [vp], if vp is
      built over another viewport, the same backend is used. *)

  val desync_ratio : t -> unit
  (** [desync_ratio vp] make [vp] single. The ratio used will be the one
      used before desync. *)

  val sync_ratio : t -> t -> unit
  (** [sync_ratio vp vp_base] synchronizes [vp]'s ratio with the
      [vp_base]'s one. *)

  val desync_range : ?x:bool -> ?y:bool -> t -> unit
  (** [desync_range vp] make [vp] single. The range used will be the one
      used before desync.

      @param x desync the x axis (default: true)

      @param y desync the y axis (default: true)
  *)

  val sync_range : ?x:bool -> ?y:bool -> t -> t -> unit
  (** [sync_range vp vp_base] synchronizes [vp]'s ranges (according
      to ?x and ?y params) with the ranges of [vp_base]. The range
      consists of a xmin and a xmax values, which defines the bounds of
      the viewport in Coordinate data.

      @param x sync the x axis (default: true)

      @param y sync the y axis (default: true)
  *)

  val desync_unit_size : ?x:bool -> ?y:bool -> t -> unit
  (** [desync_unit_size vp] make [vp] single. The unit size used will be
      the one used before desync.

      @param x desync the x axis (default: true)

      @param y desync the y axis (default: true)
  *)

  val sync_unit_size : ?x:bool -> ?y:bool -> t -> t -> unit
  (** [sync_unit_size vp vp_base] synchronizes [vp]'s unit sizes
      (according to ?x and ?y params) with the sizes of [vp_base].

      @param x sync the x axis (default: true)

      @param y sync the y axis (default: true)
  *)

  val sync : ?x:bool -> ?y:bool -> t -> t -> unit

  val layout_grid : ?syncs:(bool * bool * bool * bool) ->
    t -> int -> int -> t array
  (** [layout_grid parent n_cols n_rows] creates [n_cols] * [n_rows]
      viewports layouted in a grid and returns them in an array of
      viewports

      @param syncs (cx, cy, rx, ry) should we synchronize the x axis
      along the columns ? The y axis alon the columns ? The x axis
      along the rows ? The y axis along the rows ?
  *)
  val layout_rows : ?syncs:(bool * bool) -> t ->
    int -> t array
  (** [layout_rows parent n_rows] creates [n_rows] viewports layouted in a
      column and returns them in an array of viewpors

      @param syncs the axes to synchronize (x, y)
  *)
  val layout_columns : ?syncs:(bool * bool) -> t ->
    int -> t array
  (** [layout_cols parent n_cols] creates [n_cols] viewports layouted in a
      row and returns them in an array of viewports

      @param syncs the axes to synchronize (x, y)
  *)
  (*val fixed_left : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_right : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_top : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_bottom : ?axes_sys:bool -> float -> t -> viewport * viewport*)
  val layout_borders : ?north:float -> ?south:float -> ?west:float ->
    ?east:float -> t -> t * t * t * t * t
  (** [layout_borders parent] returns a 5-uple of viewports where the 4
      first viewports are fixed in size towards the center while the fifth
      one is extensible. The viewports are north, south, west, east, center
      and are placed conformally to their names.

      @param north the size of the north's viewport; if zero (default),
      this viewport is unused (the north viewport will be the same as
      the center one)

      @param south the size of the south's viewport; same behaviour as
      north if zero

      @param west the size of the west's viewport; same behaviour as
      north if zero

      @param east the size of the east's viewport; same behaviour as
      north if zero
  *)

  val ortho_from : t -> coord_name -> float * float -> float * float
  val data_from : t -> coord_name -> float * float -> float * float

  val set_line_width : t -> float -> unit
  val set_font_size : t -> float -> unit
  val set_mark_size : t -> float -> unit
  val set_rel_line_width : t -> float -> unit
  val set_rel_font_size : t -> float -> unit
  val set_rel_mark_size : t -> float -> unit
  val get_color : t -> Color.t
  val get_line_width : t -> float
  val get_font_size : t -> float
  val get_mark_size : t -> float

  val lower_left_corner  : t -> float * float
  (** The device's coordinates of the viewport's lower left corner *)
  val upper_right_corner : t -> float * float
  (** The device's coordinates of the viewport's upper right corner *)
  val dimensions : t -> float * float
  (** The device's width and height of the viewport *)

  val set_color : t -> Color.t -> unit
  val set_global_line_cap : t -> Backend.line_cap -> unit
  val set_global_dash : t -> float -> float array -> unit
  val set_global_line_join : t -> Backend.line_join -> unit
  val get_line_cap : t -> Backend.line_cap
  val get_dash : t -> float array * float
  val get_line_join : t -> Backend.line_join
  val move_to : t -> x:float -> y:float -> unit
  val line_to : t -> x:float -> y:float -> unit
  val rel_move_to : t -> x:float -> y:float -> unit
  val rel_line_to : t -> x:float -> y:float -> unit
  val curve_to :
    t ->
    x1:float ->
    y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
  val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  val arc : t -> r:float -> a1:float -> a2:float -> unit
  val close_path : t -> unit
  val clear_path : t -> unit
  (*val path_extents : t -> rectangle*)
  val stroke_preserve : ?path:Path.t -> t -> coord_name -> unit
  (** strokes the path (default: viewport's path) on the specified
      coordinate system, doesn't clear the viewport's path if no path
      given *)
  val stroke : ?path:Path.t -> t -> coord_name -> unit
  (** strokes the path (default: viewport's path) on the specified
      coordinate system, does clear the viewport's path if no path given *)
  val fill_preserve : ?path:Path.t -> t -> coord_name -> unit
  val fill : ?path:Path.t -> t -> coord_name -> unit
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit
  val show_text :
    t -> coord_name ->
    ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit
  val mark : t -> x:float -> y:float -> string -> unit

  val axes_ratio : t -> float -> unit
  (** [axes_ratio vp ratio] forces axes to keep [ratio] ([w / h]). *)
  val xrange : t -> float -> float -> unit
  val yrange : t -> float -> float -> unit

  val xmin : t -> float
  val xmax : t -> float
  val ymin : t -> float
  val ymax : t -> float

  val xlog : t -> bool
  val ylog : t -> bool
  val set_xlog : t -> bool -> unit
  val set_ylog : t -> bool -> unit

  val set_line_width_direct : t -> float -> unit -> unit
  val set_font_size_direct : t -> float -> unit -> unit
  val set_mark_size_direct : t -> float -> unit -> unit
  val set_rel_line_width_direct : t -> float -> unit -> unit
  val set_rel_font_size_direct : t -> float -> unit -> unit
  val set_rel_mark_size_direct : t -> float -> unit -> unit
  val set_color_direct : t -> Color.t -> unit -> unit
  val set_line_cap_direct : t -> Backend.line_cap -> unit -> unit
  val set_dash_direct : t -> float -> float array -> unit -> unit
  val set_line_join_direct : t -> Backend.line_join -> unit -> unit
  val stroke_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
  val fill_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
  val clip_rectangle_direct : t -> x:float -> y:float -> w:float ->
    h:float -> unit -> unit
  val select_font_face_direct : t -> Backend.slant -> Backend.weight ->
    string -> unit -> unit
  val show_text_direct : t -> coord_name -> ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit -> unit
  val mark_direct : t -> x:float -> y:float -> string -> unit -> unit
  val save_direct : t -> unit -> unit
  val restore_direct : t -> unit -> unit


  val add_instruction : (unit -> unit) -> t -> unit
  val do_instructions : t -> unit

  val auto_fit : t -> float -> float -> float -> float -> unit
  (** [auto_fit vp x0 y0 x1 y1] ensures that the rectangle delimited by
      (x0, y0) and (x1, y1) is included into the axes' ranges *)

  val save : t -> unit
  val restore : t -> unit

end



(** Arrow styles  *)
module Arrows :
sig

  (** Style of the arrow end. Below are textual representations of
      those endings *)
  type style =
  | Unstyled      (** ------- *)
  | Simple        (** ------> *)
  | Double        (** ----->> *)
  | Triple        (** ---->>> *)
  | Diamond       (** -----<> *)
  | Circle        (** ------O *)
  | Stop          (** ------| *)
  | Custom of (Path.t -> unit) (** It is also possible to give a path in
                                  the Custom style, leading to a
                                  completely customised arrow *)

  val path_line_to : ?size:float -> ?head:style -> ?tail:style ->
    Path.t -> float -> float -> unit
  (** [path_line_to p x y] Adds an arrow to ([x], [y]) into the path
      [p].  See {!line} for explantation on the optional arguments. *)

  val line_direct : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> unit -> unit
  (** [line_direct vp x0 y0 x y ()] draws a line directly on the
      viewport, withtout using an instruction (see {!line} for usage) *)

  val line : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> unit
  (** [line vp x0 y0 x y] Draws a arrowed line on the viewport [vp] from
      ([x0], [y0]) to ([x], [y]) using an instruction (the drawing of
      the line is put on the queue of elements to draw on the viewport)

      @param size the size of the endings, in marks size

      @param head the head ending style

      @param tail the tail ending style *)

  val arc_direct : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> float -> unit -> unit
  (** [arc_direct vp x0 y0 r a1 a2 ()] draws an arc directly on the
      viewport, withtout using an instruction (see {!arc} for usage) *)

  val arc : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> float -> unit
  (** [arc vp x0 y0 r a1 a2] Draws a arrowed arc on the viewport [vp]
      from ([x0], [y0]) with a starting angle [a1], a ending angle [a2] and
      a radius [r]. Note that the starting point ([x0], [y0]) is called the
      tail of the arrow.

      @param size the size of the endings, in marks size

      @param head the head ending style

      @param tail the tail ending style *)
end


(** Routines to draw basic axes systems in a 2-dimensional space. One can
    either draw axes separately using add_(x|y)_axis or use a full default
    axes system with box or cross. *)
module Axes :
sig

  (** The axis can be padded using an offset. It is used to control
      where to place the axis (according to the other axis) *)
  type offset =
  | Relative of float
  (** A relative offset is given in the Data coordinate system. So
      you can ensure that the axis is drawn at the other axis'
      origin (offset: Relative 0.) *)
  | Absolute of float
  (** An absolute offset is given in the Graph coordinate system and
      should have a value between 0 and 1. Using this kind of
      offset, one can ensure to always get the same rendering *)

  val add_x_axis : ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
  (** [add_x_axis vp] adds an x-axis to the viewport [vp].

      @param start the arrow ending style on the left (x0) (see the
      Arrows module)

      @param stop the arrow ending style on the right (xend) (see the
      Arrows module)

      @param tics the "tics policy" for this axis (see the Tics module)

      @param offset where to place the axis (y-coordinate) *)

  val add_y_axis : ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
  (** [add_y_axis vp] adds an y-axis to the viewport [vp].

      @param start the arrow ending style on the bottom (y0) (see the
      Arrows module)

      @param stop the arrow ending style on the top (yend) (see the
      Arrows module)

      @param tics the "tics policy" for this axis (see the Tics module)

      @param offset where to place the axis (x-coordinate) *)

  val box : ?tics:Tics.t -> ?tics_alt:Tics.t -> Viewport.t -> unit
  (** [box vp] A default axes system consisting of four axes, one at
      each border of the viewport [vp], resulting in a box surrounding
      the viewport.

      @param tics the "tics policy" for the left and bottom axes (see
      the Tics module for more information over tics policies)

      @param tics_alt the "tics policy" for the right and top axes (see
      the Tics module for more information over tics policies) *)

  val cross : ?tics:Tics.t -> Viewport.t -> unit
  (** [cross vp] A default axes system consisting of two axes, centered
      on the origin ((0, 0) in Data coordinates).

      @param tics the "tics policy" of the axes (see the Tics module for
      more information over tics policies) *)
end


(** Some utils to sample a function. The main routine, samplefxy, is
    mainly based on an article from Graphics Gems vol. 5, page 173:
    Adaptive Sampling of Parametric Curves. *)
module Sampler :
sig

  type strategy = float -> float -> float
  (** A strategy is a function [f t1 t2] that returns an internal
      point tm between t1 and t2 which will be used to decide if we
      need to increment precision or not. *)

  type criterion = float -> float -> float -> float -> float -> float -> bool
  (** A criterion is a function [f x1 y1 xm ym x2 y2] which returns true
      if we need to increment precision. *)

  val strategy_midpoint : strategy
  (** The default strategy: choose the middle point *)

  val strategy_random : strategy
  (** A strategy that avoids aliasing sampling, but not efficient:
      chooses randomly a point between t1 and t2 *)

  val strategy_center_random : strategy
  (** A more efficient strategy that avoids aliasing sampling: chooses
      randomly a points between t1 and t2 in its 10% center interval *)

  val criterion_none : criterion
  (** A criterion that tells to never increment precision *)

  val criterion_angle : ?threshold:float -> criterion
  (** A criterion that tells to increment precision only if the angle
      xMy is leather than threshold

      @param threshold the minimal angle under which no more precision
      is needed
  *)

  val criterion_angle_log : bool -> bool -> ?threshold:float -> criterion
  (** Same criterion as criterion_angle, but one can tell that the x/y
      axis is logarithmic, and increment precision in a more wise way *)

  type t
  (** A sampler *)

  val create : ?tlog:bool -> ?min_step:float -> ?nsamples:int ->
    ?strategy:strategy -> ?criterion:criterion ->
    (float -> float * float) -> float -> float -> t
  (** [create f t1 t2] samples the parametric function f from t1 to t2,
      returning a list of the points in the sample.

      @param tlog do we need to step in a logarithmic way ?

      @param min_step don't increment precision more than this threshold

      @param nsamples base number of samples wanted (cut the space
      between t1 and t2 in nsamples fragments of equivalent size,
      depending on tlog)

      @param strategy a customized strategy, which can be chosen among
      those in this module

      @param criterion a customized criterion, which can be chosen among
      those in this module
  *)

  val reset : t -> unit
  (** [reset s] Resets the sampler *)

  val next : t -> (float * float) option
  (** [next s] Returns the next point of the sampling *)

end


(** Iterations on points (internal module). *)
module Iterator :
sig

  type t
  (** An iterator, created by a "of_*" function, and manipulated
      through next, reset, iter and iter_cache (see below) *)

  exception EOI
  (** End Of Iterator, the exception raised when next is called on a
      terminated iterator *)

  val of_list : float list -> t
  (** [of_list l] Transforms a float list into an iterator which will
      return values (position in [l], value at that position) *)
  val of_array : float array -> t
  (** [of_array a] Transforms a float array into an iterator which will
      return values (position in [a], value at that position) *)
  val of_c : (float, Bigarray.float64_elt, Bigarray.c_layout)
    Bigarray.Array1.t -> t
  (** [of_c b] Transforms a bigarray with a C layout into an iterator
      which will return values (position in [b], value at that
      position) *)
  val of_fortran : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
    Bigarray.Array1.t -> t
  (** [of_fortran b] Transforms a bigarray with a Fortran layout into an
      iterator which will return values (position in [b], value at that
      position) *)

  val of_list2 : (float * float) list -> t
  (** [of_list2 l] Transforms a list of float couples into an iterator
      returning those couples *)
  val of_array2 : (float * float) array -> t
  (** [array_array2 a] Transforms an array of float couples into an
      iterator returning those couples *)
  val of_c2 : (float, Bigarray.float64_elt, Bigarray.c_layout)
    Bigarray.Array2.t -> t
  (** [of_c2 b] Transforms a bigarray of float couples with a C layout
      into an iterator returning those couples *)
  val of_fortran2 : (float, Bigarray.float64_elt, Bigarray.fortran_layout)
    Bigarray.Array2.t -> t
  (** [of_fortran2 b] Transforms a bigarray of float couples with a
      Fortran layout into an iterator returning those couples *)
  val of_function : ?tlog:bool -> ?min_step:float -> ?nsamples:int ->
    ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
    (float -> float * float) -> float -> float -> t
  (** [of_function f a b] Create an iterator from a function (R to R x
      R), refining when necessary to get a smooth curve

      @param tlog do we need to step in a logarithmic way ?

      @param min_step don't increment precision more than this threshold

      @param nsamples base number of samples wanted (cut the space
      between t1 and t2 in nsamples fragments of equivalent size,
      depending on tlog)

      @param strategy a customized strategy, which can be chosen among
      those in this module

      @param criterion a customized criterion, which can be chosen among
      those in this module
  *)
  val of_last : (float * float -> float * float) -> float * float -> t
  (** [of_last f start] Create an iterator which creates its next
      element from the last he has computed using the function [f], starting
      from [start] *)

  val next : t -> float * float
  (** [next iter] returns the next value of [iter] *)
  val reset : t -> unit
  (** [reset iter] put back [iter] to its initial state *)
  val iter : (float * float -> unit) -> t -> unit
  (** [iter f iter] apply the function [f] to all values left in the
      iterator (the iterator won't be resetted !) *)
  val iter_cache : (float * float -> unit) -> t -> (float * float) list
  (** [iter_cache f iter] apply the function [f] to all values left in
      the iterator. Those values are stored reversed in a list which is
      returned *)

  val constant_iterator : float -> t
  (** [constant_iterator c] Creates an iterator which starts at (0.,
      [c]) and just increment the x value *)
  val zero_iterator : unit -> t
  (** [zero_iterator ()] Alias for (constant_iterator 0.) *)

end

(** Plotting various datatypes. *)
module Plot :
sig
  type pathstyle =
    | Lines
    (** Data points are joined by a simple line *)
    | Points of string
    (** Data points are marked with the mark type given in argument of
        the Points constructor *)
    | Linespoints of string
    (** Data points are joined by a line and marked with the mark type
        given in argument *)
    | Impulses
    (** Data points are "hit" by lines starting from zero *)
    | Boxes of float
    (** Data points are the top of a box of custom width (from 0 to 1) *)
    | Interval of float
    (** Data points are represented with a line from a base
        point. That line is delimited by two small orthogonal lines *)

  val x : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    ?base:Iterator.t -> Viewport.t -> Iterator.t -> unit
  (** [x vp iter] Plots the values of [iter] on [vp] according to the
      fact that the x-values of iter are 0, 1, 2, etc. or 1, 2, 3,
      etc. or 42, 43, 44, etc. or ...

      @param fill fill the region between the iterator and its base ?
      (default: false)

      @param fillcolor which color to use for the fill

      @param pathstyle which pathstyle to use (see pathstyle type)

      @param base the base iterator is the other delimiter of the region
      to fill. Default: the zero iterator (giving (0, 0), (1, 0), (2,
      0), etc.) *)

  val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
    Viewport.t -> Iterator.t -> unit
  (** [xy vp iter] Plots the values of [iter] on [vp] with no
      constraints over the values of iter.

      @param fill fill the region delimited by the curve ? (default:
      false)

      @param fillcolor which color to use for the fill

      @param pathstyle which pathstyle to use (see pathstyle type) *)

  val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
    ?pathstyle:pathstyle -> Viewport.t -> Iterator.t array -> unit
  (** [stack vp iters] Stacks the iterators [iters] on [vp]; which means
      that the first iterator is plotted then used as the base for the
      second iterator, which is plotted and the sum of the two first
      iterators are used as the base for the third iterator,
      etc. Usually, the pathstyle used for a stack is Boxes, but one can
      use another pathstyle if he wants

      @param colors the colors to use for the iterators. If there are
      more iterators than colors available, a round-robin strategy is
      used to attribute colors

      @param fillcolors same as colors, but for filling colors

      @param pathstyle which pathstyle to use (see pathstyle type,
      default is [Boxes 0.5]) *)

  module Function : sig
    type 'a sampling
    (** A sampling, the only interresting values for 'a are float and
        float * float *)

    val sampling : ?tlog:bool -> ?strategy:Sampler.strategy ->
      ?criterion:Sampler.criterion -> ?min_step:float -> ?nsamples:int ->
      (float -> 'a) -> float -> float -> 'a sampling
    (** [sampling f a b] Creates a sampling for the function [f] between
        [a] and [b], see Sampler for more explanations over the
        optional arguments *)

    val x : ?pathstyle:pathstyle -> ?base:(float -> float) -> Viewport.t -> float sampling -> unit
    (** [x vp sampling] Plots [sampling] on [vp], the sampling needs to
        be a function sampling, and not a curve sampling

        @param pathstyle which pathstyle to use (see pathstyle type) *)

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> (float * float) sampling -> unit
    (** [xy vp sampling] Plots [sampling] on [vp], the sampling needs to
        be a curve sampling

        @fill fill the curve delimited by the function ? (default: false)

        @fillcolor color to use for the filling

        @param pathstyle which pathstyle to use (see pathstyle type) *)

    val fill : ?fillcolor:Color.t -> ?base:(float sampling) -> Viewport.t ->
      float sampling -> unit
    (** [fill vp sampling] Fills the region between two function samplings

        @param fillcolor the color to use for the filling

        @param base the other sampling used for the filling. The fill
        function will handle non concordant domains and samplings that
        "cross over" one another *)
  end

  module type Common = sig
    (** The Common module type is used by all the "standard" plot modules
        (Lists, Arrays, Bigarrays) *)

    type data
    (** The function type, e.g. float list *)
    type data2
    (** The curve type, e.g. (float * float) list *)

    val x : ?base:data -> ?fill:bool -> ?fillcolor:Color.t ->
      ?pathstyle:pathstyle -> Viewport.t -> data -> unit
    (** Same as the x function of the Plot module, but instead of
        applying to iterators, it applies to a particular data structure
        determined by the submodule which is used (Plot.Array,
        Plot.List, Plot.Fortran or Plot.C) *)

    val xy : ?fill:bool -> ?fillcolor:Color.t -> ?pathstyle:pathstyle ->
      Viewport.t -> data2 -> unit
    (** Same as the xy function of the Plot module, but instead of
        applying to iterators, it applies to a particular data structure
        determined by the submodule which is used (Plot.Array,
        Plot.List, Plot.Fortran or Plot.C) *)

    val stack : ?colors:(Color.t array) -> ?fillcolors:(Color.t array) ->
      ?pathstyle:pathstyle -> Viewport.t -> data array -> unit
    (** Same as the stack function of the Plot module, but instead of
        applying to iterators, it applies to a particular data structure
        determined by the submodule which is used (Plot.Array,
        Plot.List, Plot.Fortran or Plot.C) *)
  end

  module Array : sig
    include Common
      with type data = float array
      and type data2 = (float * float) array
  end

  module List : sig
    include Common
      with type data = float list
      and type data2 = (float * float) list
  end

  module Fortran : sig
    open Bigarray

    include Common
      with type data = (float, float64_elt, fortran_layout) Array1.t
      and type data2 = (float, float64_elt, fortran_layout) Array2.t
  end

  module C : sig
    open Bigarray

    include Common
      with type data = (float, float64_elt, c_layout) Array1.t
      and type data2 = (float, float64_elt, c_layout) Array2.t
  end
end

module Piechart :
sig
  type style =
  | Flat          (** A simple circle separated in regions *)
  | Separated     (** The regions are separated by a gap *)
  | HighlightFlat (** One of the regions is separated by a gap, and so
                      highlighted toward the others *)
  | Relief        (** A simple 3D pie *)

  type colorscheme =
  | Default    (** A set of colors that should render good for any data *)
  | Monochrome (** A gradient from black to white *)
  | None       (** No colors at all, the Overpie or Outer keyscheme should then
                   be used *)
  | CustomColors of (string * Color.t) list
  (** A color scheme associating a custom color to each data *)
  | ValueDependant of (float -> Color.t)
  (** Sometimes it is desirable to use color to express something on
      the piechart depending on the data values, this color scheme
      permits it *)
  | LevelValueDependant of (int -> int -> float -> Color.t -> float -> Color.t)
    (** For multi-levels pie charts, the color may depend of the
        level, the position (in the parent children's list), the
        parent data value/color and the actual value of the data to
        draw. The level 0 is the first level with at least two
        elements *)

  type keyplacement =
  | Rectangle (** A rectangle containing the color followed by the label
                  of each data *)
  | OverPie   (** The labels are drawn directly over the data *)
  | Outer     (** The labels are drawn around the pie, next to the data they
                  point out *)

  type keylabels =
  | Key          (** Just the name of the data *)
  | WithValues   (** The name followed by the value between parentheses *)
  | WithProcents (** The name followed by the procent among all data between
                     parentheses *)
  | CustomLabels of (string -> float -> float -> string)
  (** A custom label made of the name, the value and the percentage. *)

  val simple : ?style:style -> ?colorscheme:colorscheme ->
    ?keyplacement:keyplacement -> ?keylabels:keylabels ->
    ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
    Viewport.t -> (string * float) list -> unit
  (** [simple vp data] draws a pie chart on [vp].

      @param style the style, default is Relief

      @param colorscheme the color scheme, default is Default

      @param keyplacement where to place the key, default is Rectangle

      @param keylabels what are the labels, default is WithValues

      @param x0 the x-coordinate of the center (default: [0.]).

      @param y0 the y-coordinate of the center (default: [0.]).

      @param xend (default: [1.]).

      @param yend these parameters delimits the area of the pie chart
      over the viewport. They are given in Graph coordinates (i.e. from
      0 to 1). By default, some space is left on the top for a title for
      the pie chart *)

  type multidata = {
    name: string;
    value: float;
    children: multidata list
  }

  val multilevel : ?style:style -> ?colorscheme:colorscheme ->
    ?keyplacement:keyplacement -> ?keylabels:keylabels ->
    ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
    Viewport.t -> multidata list -> unit
  (** [multilevel vp data] draws a multilevel pie chart on [vp]. The
      default options are tuned for a multilevel pie chart

      @param style default is flat (better visualisation because there
      is usualy lots of data)

      @param colorscheme default is LevelValueDependant, colors of the
      first level to contain more than one data (= level 0) are chosen
      in the "Default" way, children colors are derived from their
      parent color and their value. Inner levels (those who contains
      only one data) are filled with blank

      @param keyplacement default is OverPie, this is usually the better
      way to visualize data over a multilevel pie chart

      @param keylabels default is Key, because the color scheme gives an
      idea of the values, it is preferable to save space by hiding the
      values / percentages of the data

      @param x0 the x-coordinate of the center (default: [0.]).

      @param y0 the y-coordinate of the center (default: [0.]).

      @param xend (default: [1.]).

      @param yend by default, space is left for the title, as for the
      simple pie charts *)

end
(** A 2D plotting library with various backends. *)


val init : ?lines:float -> ?text:float -> ?marks:float ->
  ?w:float -> ?h:float -> ?dirs:string list -> string -> Viewport.t
(** [init backend_name] initializes Archimedes and returns a main
    viewport using the backend specified.

    @param lines the width of the lines (default: 1. corresponds to
    filling a biggest square of the viewport with 500 lines)

    @param text the size of the text in (default: 12. corresponds to
    filling a biggest square of the viewport with about 42 lines of
    text)

    @param marks the size of the marks in pixels (default:
    1. corresponds to filling a biggest square the viewport with 100
    "lines of marks")

    @param w the width of the main viewport (in backend's unit)

    @param h the height of the main viewport (in backend's unit)

    @param dirs a list of directories where Archimedes looks for
    libraries (cma or cmxs) for dynamically loaded backends.  The
    default is the directory where the backends that come with
    Archimedes were installed.
*)

val close : Viewport.t -> unit


val fx : ?tlog:bool ->
  ?strategy:Sampler.strategy -> ?criterion:Sampler.criterion ->
  ?min_step:float -> ?nsamples:int ->
  ?fill:bool -> ?fill_base:(float -> float) -> ?fillcolor:Color.t ->
  ?pathstyle:Plot.pathstyle -> Viewport.t ->
  (float -> float) -> float -> float -> unit
