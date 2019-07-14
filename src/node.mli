(* XXX Constructors of view, trail, and cursor should be private, 
   to prevent invalid values formed 
*)

type hashed =
  | Hashed of Hash.t
  | Not_Hashed
  (** Type used to prove that if a node is hashed then so are its children.
      The type also provides the hash as a witness.*)

type indexed =
  | Indexed of Index.t
  | Left_Not_Indexed (* For Internal: Right may not be indexed either *)
  | Right_Not_Indexed (* For Internal: Left may not be indexed either *)
  | Not_Indexed (* For non Internals *) 
  (** This rule expresses the following invariant : if a node is indexed, then
      its children are necessarily indexed. Less trivially, if an internal node is not
      indexed then at least one of its children is not yet indexed. The reason
      is that we never construct new nodes that just point to only existing nodes. 
      This property guarantees that when we write internal nodes on
      disk, at least one of the child can be written adjacent to its parent. *)

type extender_witness =
  | Maybe_Extender (* Not sure it is an Extender or not *)
  | Not_Extender   (* Sure it is NOT an Extender *)
  | Is_Extender    (* Sure it is an Extender *)

type node =
  | Disk of Index.t * extender_witness
  (* Represents a node stored on the disk at a given index, the node hasn't
     been loaded yet. Although it's considered hash for simplicity's sake,
     reading the hash requires a disk access and is expensive. 
  
     extender_witness carries the information whether the node is 
     an Extender or not.
  *)

  | View of view
  (* A view node is the in-memory structure manipulated by programs in order
     to compute edits to the context. New view nodes can be commited to disk
     once the computations are done. *)

and view = private
  | Internal of node * node
               * indexed
               * hashed
  (* An internal node, left and right children and an internal path segment
     to represent part of the path followed by the key in the tree. 
  
     indexed carries the index if the node is indexed.  Otherwise,
     it carries XXX
     
     hashed carries the hash of the node if already computed.
  *)

  | Bud of node option
          * indexed
          * hashed
  (* Buds represent the end of a segment and the beginning of a new tree. They
     are used whenever there is a natural hierarchical separation in the key
     or, in general, when one wants to be able to grab sub-trees. For instance
     the big_map storage of a contract in Tezos would start from a bud. *)

  | Leaf of Value.t
          * indexed
          * hashed
  (* Leaf of a tree, the end of a path, contains or points to a value.
     The current implementation is a bit hackish and leaves are written
     on *two* cells, not one. This is important to keep in mind when
     committing the tree to disk.
  *)

  | Extender of Segment.t
                * node
                * indexed
                * hashed
  (* Extender node, contains a path to the next node. Represents implicitely
     a collection of internal nodes where one child is Null. *)

val indexed : node -> bool
val index : node -> Index.t option
val hashed : node -> bool
val hash_of_view : view -> Hash.t option

(** Constructors with invariant checks *)

val _Internal : node * node
               * indexed
               * hashed
               -> view

val _Bud : node option
        * indexed
        * hashed
        -> view

val _Leaf : Value.t
        * indexed
        * hashed
        -> view

val _Extender : Segment.t
               * node
               * indexed
               * hashed
               -> view

type modified =
  | Modified_Left
  | Modified_Right
  | Unmodified of
      indexed *
      hashed

(** A trail represents the content of the memory stack when recursively exploring a tree.
   Constructing these trails from closure would be easier, but it would make it harder
   to port the code to C. The type parameters of the trail keep track of the type of each
   element on the "stack" using a product type. *)

type trail = private
  | Top
  | Left of (* we took the left branch of an internal node *)
      trail
      * node (* the right node *)
      * modified

  | Right of (* we took the right branch of an internal node *)
      node (* the left node *)
      * trail
      * modified

  | Budded of
      trail
      * modified

  | Extended of
      trail
      * Segment.t
      * modified

(** Constructors with invariant checks *)

val _Top : trail
val _Left : trail
    * node
    * modified
    -> trail
val _Right : 
    node
    * trail
    * modified
    -> trail
val _Budded :
    trail
    * modified
    -> trail
val _Extended :
    trail
    * Segment.t
    * modified
    -> trail

val load_node_ref : (Context.t -> Index.t -> extender_witness -> view) ref
(** Placeholder of node loading from a context *)

val load_node : Context.t -> Index.t -> extender_witness -> view
(** Node loading from a context *)

val may_forget : node -> node option
(** If the node is indexed, forget the details *)
    
val view : Context.t -> node -> view
(** Obtain the view of the node.  If the view is not available in the memory,
    it is loaded from the storage. *)
  
type cursor = private
    Cursor of trail
              * node
              * Context.t
(** The cursor, also known as a zipper combines the information contained in a
   trail and a subtree to represent an edit point within a tree. This is a
   functional data structure that represents the program point in a function
   that modifies a tree. We use an existential type that keeps the .mli sane
   and enforces the most important: that the hole tags match between the trail
   and the Node *)

(** Constructor with invariant checks *)

val _Cursor : (trail * node * Context.t) -> cursor

val path_of_trail : trail -> Segment.side list list
(** Segment side list of the given trail, splitted by buds *)
