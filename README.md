Status: early development, do not expect this to work or even compile at this stage.

# Plebeia

Plebeia is a functional implementation of a sparse Merkle tree through a Patricia trie persisted on disk. The library allows the manipulation of in-memory functional views of the trie and perform atomic commits of those views to the disk.

## Sub-trees

Plebeia supports sub-trees. That is, a leaf can either contain a value or the root of a sub-tree. Trees and sub-trees can be navigated like directories and sub-directories using a cursor implemented with a zipper.

## Storage

The trie is persisted to a file on disk by appending nodes. All nodes take up excatly 256 bits (32 bytes) of disk space making it easy to index them. Hashes are 448 bit long offering about 111.5 bits of security.

Leaf data is persisted in the same data file as the tree.  Garbage collection of the trie is implemented with a stop and copy approach. (This GC is not implemented.)

## Hash format

Let `H(x)== BLAKE2B(x,28)` be a hash function with a 224 bit digest.

The goal is to make the root hash independent of the Patricia implementation. The approach
is inspired from the one described by Vitalik Buterin [here](https://ethresear.ch/t/optimizing-sparse-merkle-trees/3751/14).

### Leaf

The hash of a leaf is `H(0x00 || v)` followed by 223bits of zeros and 1bit of one:

```
leaf      |<-      H(0x00 || v)        ->|
          |                              |0...........................01|
```

### Bud

The hash of the empty bud is all (448bits) zeros:

```
empty bud |0000000000000000000000000000000000000000000000000000000000000|
```

The hash of a bud with a child is the hash of the child:

```
bud       |<------------------ hash of its child ---------------------->|
```

### Internal

The hash of an internal node with children hashes `l` and `r` is `H(0x01 || l || h)` whose last 2bits are
reset to zero, followed by 223bits of zeros and 1bit of one:

```
internal  |<-     H(0x01 || l || h)    ->|
          |                          |0|0|0...........................01|
```

### Extender

The hash of an extender is the first 28bytes of its child's hash,
followed by 224bits of its segment encoding (see below).

```
extender  |<------------------ hash of its child ---------------------->|
          | The first 224bits of H_child |000...1|<- segment bits --->|1|
```

#### Segment encoding

*Segment encoding* is 224bit length data.  It is:

* ended by 1bit of one,
* prepended by bits of segments, top comes the first and bottom comes the last, 0 is for left and 1 for right,
* prepended 1bit of one,
* then prepended as many bits of zeros to make the entire length become 224bits:

```
          |000...1|<- segment bits --->|1|
```

From the definition of the segment encoding, the maximum length of segments is 224-2 = 222.

## Node storage format

All nodes are stored in an array with 256 bit (32 bytes) *cells*. The constant size makes it easy for nodes to refer to each other using an index in the array.

This leads to a bit of awkwardness (222 bit hashes, array size limited to 2^32 - 256) but the potential benefit is that two nodes can fit in a cache line in 64 bit architectures.  If it turns out this doesn't make a noticeable difference, it's straightforward to increase the size of cells to 320 bits, or introduce variable size cells, and remove much of the awkwardness.

### Integer encoding

Integers are embedded in cells using little endian.

### Index part

The last 32bits of a node cell are called the index part of the node, which often refers to a node linked from the node.  The possible index values are from 0 to 2^32 - 1 - 256.

### Tags

The index part value more than 2^32 - 256 to 2^32 - 1 are used for *tags*:

* 2^32 -1 to 2^32-32 : small leaves, whose contents are stored in the previous cell
* 2^32 -33 to 2^32-64 : medium leaves, whose contents are stored in the 2 previous cells
* 2^32 -256 : bud
* 2^32 -255 : large leaves in Plebeia.  Their contents are stored in Plebeia cells.
* Others from 2^32 -256 : reserved

### Layout table

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| (also refers to the previous cell)
extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
leaf (Z)  |<- first 224 of hash ------------>| |<- -65 ------------------------------->|
leaf (S)  |<- first 224 of hash ------------>| |<- -1 to -32 ------------------------->| (use the previous cell)
leaf (M)  |<- first 224 of hash ------------>| |<- -33 to -64 ------------------------>| (use the two previous cells)
link      |<- 192 0's ->|<-   child index  ->| |<- -254 ------------------------------>|
leaf (P)  |<- first 224 of hash ------------>| |<- -255 ------------------------------>| (use the previous cell and some others)
bud       |<- 192 0's ->|<-   child index  ->| |<- -256 ------------------------------>|
empty bud |<- 1111111111111111111111111111 ->| |<- -256 ------------------------------>|
reserved  |                                  | |<- Others between -256 and -1--------->|
```

This layout restricts the maximum index number to 2^32-257.  Which is about 4G cells, 128GB file size.

### Internal

* The first 222bits are the prefix of the node's hash.
* appended by 1bit `D`, which denotes which child's index is referred in the index part. 0: left, 1: right.
* appended by 1bit of zero
* followed by 32bits of the index of the one of the children specified by `D`

The other child which is not referred in the index part of the internal node storage always stored
in the previous cell of the internal node.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->|
```

### Extender

* The first 224bits are the segment encoding of the extender.
* The index part is the index of the child.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
```

### Leaf

* The first 224bits are the prefix of the hash of the leaf.

#### Zero leaf

This is for the leaf with the zero size value.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf      |<- first 224 of hash ------------>| |<- -65 ------------------------------->|
```

#### Small leaf

The tag of a small leaf is between 2^32-32 to 2^32-1 inclusive.
It denotes the length of the value in bytes, which should be stored in the previous cell of the leaf.

The value stored from the head of the previous cell.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf      |<- first 224 of hash ------------>| |<- -32 to -1 ------------------------->|

Previous cell:
          |< ----   256 bits --------------------------------------------------------->|
----------------------------------------------------------------------------------------
data      |<- value contents ------------------------->|                               |
```

#### Medium leaf

The tag of a small leaf is between 2^32-64 to 2^32-33 inclusive.
It denotes the length of the value in bytes, which should be stored in the previous cell of the leaf.

The value stored from the head of the previous cell.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf      |<- first 224 of hash ------------>| |<- -64 to -33 ------------------------>|

Cell at -2:
          |< ----   256 bits --------------------------------------------------------->|
----------------------------------------------------------------------------------------
data      |<- value contents --------------------------------------------------------->|

Previous cell (cell at -1):
          |< ----   256 bits --------------------------------------------------------->|
----------------------------------------------------------------------------------------
data      |<- value contents ------------------------->|                               |
```

The tag of a medium leaf is between 2^32-64 to 2^32-33 inclusive.
It denotes the length of the value in bytes, which should be stored in the two previous cells of the leaf.

The value stored from the head of the cell at the position -2 to the previous cell.

The medium leaves are introduced to store several public keys whose sizes are 33 and 34 bytes including
tags (of 1 byte) which do not fit with the small leaves.

#### Large leaf

The tag is 2^32-255.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf (P)  |<- first 224 of hash ------------>| |<- -255 ------------------------------>|
```

The contents of the value is stored in cells of the same storage.  They are stored as
a linked list of *cell chunks*.  A cell chunk is a contiguous cells with the following
format:

##### Cell chunk

(I just use the word 'chunk' since 'block' means different thing in the blockchain technology.)

A cell chunk is a contiguous cells.  There is a *footer fields* in the last bytes of each cell chunk:

* The last 4 bytes is the *cdr index* which points to the last cell of the next cell chunk
  in the chunk list.
  If the cdr index is 0, the cell chunk is the last chunk in the list.
* The 2 bytes prior to the cdr index is the *content length* in uint16.  It is the number of bytes the cell
  chunk carries for the value contents.
* The data for the value in a cell chunk is stored from the head of the first cell of the cell chunk.
  Number of the cells in the cell chunk is computable from the expression `(content_length + 6 + 31) / 32`. 
* A cell chunk can carry up to 65536 bytes, which consist of 2049 cells.

Cell chunk layout:

```
| cell #1 | cell #2 | .. | the last cell in the chunk (#n)                  | 
|         |         | .. |                            | footer fields       |
|         |         |    |                            |<-16bits->|<-32bits->|
------------------------------------------------------------------------------
|<- contents (size bytes) ------------------->|       |size      |cdr index |
```

The contents of a value are stored from the last cell chunk in the list whose cdr index is 0.
The head of cell chunk list carries the last part of the contents.

### Link

Hash-consing can break the invariant of the internal node: neither of its sub-nodes is stored
at the previous cell of it.  To work around this, a special Link node with tag -254 is 
introduced to keep the invariant.  One of the sub-nodes are connected to the internal node
via a link node which is placed in the previous cell of the internal node.

### Bud

The last 32bits is fixed to -256.

#### Non-empty bud

The index of the child node is stored from 193rd bits, following 192bits of zeros:

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
bud       |<- 192 0's ->|<-   child index  ->| |<- -256 ------------------------------>|
```

#### Empty bud

224bits of ones are prepended in front of the 32bits of 2^32-34

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
empty bud |<- 1111111111111111111111111111 ->| |<- -256 ------------------------------>|
```

#### Reserved

The other tags between 2^32-256 and 2^32-1 are reserved for future extension.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
empty bud |<- 1111111111111111111111111111 ->| |<- Unused tag between -1 and -256 ---->|
```
