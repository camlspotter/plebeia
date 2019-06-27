open Stdint

type error = string
(** Error type.  Currently, simply error messages *)

module Index = Uint32
(** Position in the storage *)  
(* XXX In 64bit arch, we can simply use unboxed int *)

type indexed_implies_hashed =
  | Indexed_and_Hashed
  | Not_Indexed_Any
  (** Type used to prove that all indexed nodes have been hashed. *)
    
type hashed_is_transitive =
  | Hashed of Hash.t
  | Not_Hashed
  (** Type used to prove that if a node is hashed then so are its children.
      The type also provides the hash as a witness.*)

type indexing_rule =
  | Indexed of Index.t
  | Left_Not_Indexed (* Right may not be indexed either *)
  | Right_Not_Indexed (* Left may not be indexed either *)
  | Not_Indexed
  (** This rule expresses the following invariant : if a node is indexed, then
      its children are necessarily indexed. Less trivially, if an internal node is not
      indexed then at least one of its children is not yet indexed. The reason
      is that we never construct new nodes that just point to only existing nodes. 
      This property guarantees that when we write internal nodes on
      disk, at least one of the child can be written adjacent to its parent. *)

type extender_witness =
  | Maybe_Extender
  | Not_Extender  
  | Is_Extender   

type hashed_witness =
  | Hashed of Hash.t
  | Not_Hashed
