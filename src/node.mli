open Types

(* Constructors of view, trail, and cursor are private, 
   to prevent invalid values formed 
*)

type node =
  | Disk of Index.t * extender_witness
  (* Represents a node stored on the disk at a given index, the node hasn't
     been loaded yet. Although it's considered hash for simplicity's sake,
     reading the hash requires a disk access and is expensive. *)

  | View of view
  (* A view node is the in-memory structure manipulated by programs in order
     to compute edits to the context. New view nodes can be commited to disk
     once the computations are done. *)

and view = private
  | Internal of node * node
               * indexing_rule
               * hashed_is_transitive
               * indexed_implies_hashed

  (* An internal node , left and right children and an internal path segment
     to represent part of the path followed by the key in the tree. *)

  | Bud of node option
          * indexing_rule
          * hashed_is_transitive
          * indexed_implies_hashed
  (* Buds represent the end of a segment and the beginning of a new tree. They
     are used whenever there is a natural hierarchical separation in the key
     or, in general, when one wants to be able to grab sub-trees. For instance
     the big_map storage of a contract in Tezos would start from a bud. *)

  | Leaf of Value.t
          * indexing_rule
          * hashed_is_transitive
          * indexed_implies_hashed
  (* Leaf of a tree, the end of a path, contains or points to a value.
     The current implementation is a bit hackish and leaves are written
     on *two* cells, not one. This is important to keep in mind when
     committing the tree to disk.
  *)

  | Extender of Path.segment
                * node
                * indexing_rule
                * hashed_is_transitive
                * indexed_implies_hashed
  (* Extender node, contains a path to the next node. Represents implicitely
     a collection of internal nodes where one child is Null. *)

(* A trail represents the content of the memory stack when recursively exploring a tree.
   Constructing these trails from closure would be easier, but it would make it harder
   to port the code to C. The type parameters of the trail keep track of the type of each
   element on the "stack" using a product type. *)

val indexed : node -> bool
val index : node -> Index.t option
val hashed : node -> bool
val hash_of_view : view -> Hash.hash56 option

(* Constructors with invariant checks *)

val _Internal : node * node
               * indexing_rule
               * hashed_is_transitive
               * indexed_implies_hashed -> view
val _Bud : node option
        * indexing_rule
        * hashed_is_transitive
        * indexed_implies_hashed -> view

val _Leaf : Value.t
        * indexing_rule
        * hashed_is_transitive
        * indexed_implies_hashed -> view

val _Extender : Path.segment
               * node
               * indexing_rule
               * hashed_is_transitive
               * indexed_implies_hashed -> view

type modified_rule =
  | Modified_Left
  | Modified_Right
  | Unmodified of
      indexing_rule *
      hashed_is_transitive

type trail = private
  | Top
  | Left of (* we took the left branch of an internal node *)
      trail
      * node
      * modified_rule
      * indexed_implies_hashed

  | Right of (* we took the right branch of an internal node *)
      node
      * trail
      * modified_rule
      * indexed_implies_hashed

  | Budded of
      trail
      * modified_rule
      * indexed_implies_hashed

  | Extended of
      trail
      * Path.segment
      * modified_rule
      * indexed_implies_hashed

(* Constructors with invariant checks *)

val _Top : trail
val _Left : trail
    * node
    * modified_rule
    * indexed_implies_hashed -> trail
val _Right : 
    node
    * trail
    * modified_rule
    * indexed_implies_hashed -> trail
val _Budded :
    trail
    * modified_rule
    * indexed_implies_hashed -> trail
val _Extended :
    trail
    * Path.segment
    * modified_rule
    * indexed_implies_hashed -> trail

val load_node_ref : (Context.t -> Index.t -> extender_witness -> view) ref
val load_node : Context.t -> Index.t -> extender_witness -> view

val may_forget : node -> node option
(** If the node is indexed, forget the details *)
    
val view : Context.t -> node -> view

type cursor = private
    Cursor of trail
              * node
              * Context.t
(* The cursor, also known as a zipper combines the information contained in a
   trail and a subtree to represent an edit point within a tree. This is a
   functional data structure that represents the program point in a function
   that modifies a tree. We use an existential type that keeps the .mli sane
   and enforces the most important: that the hole tags match between the trail
   and the Node *)

(* Constructor with invariant checks *)

val _Cursor : (trail * node * Context.t) -> cursor

val path_of_trail : trail -> Path.side list list
(** Path side list of the given trail, splitted by buds *)
