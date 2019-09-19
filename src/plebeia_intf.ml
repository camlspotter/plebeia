module type S = sig

  (** {2 Base} *)

  module Error : sig
    type t = string
  end
    
  module Result : module type of Result

  module Value : sig
    type t = Value.t
      
    val of_string : string -> t
    val to_string : t -> string
  end

  (** {2 Core} *)

  (** A segment represents a path from the root of a tree to a leaf or
      to the root of a sub-tree. *)
  module Segment : sig
    type side = Segment.side = Left | Right
    type t = side list

    val to_string : t -> string 
    (** LLRRLL *)

    val of_string : string -> t option
    (** LLRRLL *)
  end

  (** {2 Hash } *)

  module Hash : sig
    type t = Hash.t
    (** Root hash of a tree. *)

    val to_string : t -> string
    val of_string : string -> t
    val to_hex : t -> Hex.t
    val of_hex : Hex.t -> t
  end

  (** {2 High level} *)

  module Vc : module type of struct include Vc end

  module Roots : module type of struct include Roots end

  (** {2 Helper} *)
 
  module Stat : sig
    type t = Stat.t
    val create : unit -> t
    val pp : Format.formatter -> t -> unit
  end


  (** {2 Experimental} *)

  (** Human readable directory names *)
  module Key : sig
    type t = string
    val to_segments : t -> (Segment.t list, string) Result.t
    val of_segments : Segment.t list -> (t, string) Result.t
  end

end
