# Plebeia

Plebeia is a functional implementation of a sparse Merkle tree through a Patricia trie persisted on disk. The library allows the manipulation of in-memory functional views of the trie and perform atomic commits of those views to the disk.

## Sub-trees

Plebeia supports sub-trees. That is, a leaf can either contain a value or the root of a sub-tree. Trees and sub-trees can be navigated like directories and sub-directories using a cursor implemented with a zipper.

## Storage

The trie is persisted to a file on disk by appending nodes. All nodes take up excatly 256 bits (32 bytes) of disk space making it easy to index them. Hashes are 448 bit long offering about 111.5 bits of security.

Leaf data is persisted in the same data file as the tree.  Garbage collection of the trie is implemented with a stop and copy approach. (This GC is not implemented.)

## Hash format

Let `H(x)== BLAKE2B(x,28)` be a hash function with a 224 bit digest.

The goal is to make the root hash independent of the Patricia implementation. The approach is inspired from the one described by Vitalik Buterin [here](https://ethresear.ch/t/optimizing-sparse-merkle-trees/3751/14).

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

The hash of an internal node with children hashes `l` and `r` is `H(0x01 || l || h)` whose last 2bits are reset to zero, followed by 223bits of zeros and 1bit of one:

```
internal  |<-     H(0x01 || l || h)    ->|
          |                          |0|0|0...........................01|
```

### Extender

The hash of an extender is the first 28bytes of its child's hash, followed by 224bits of its segment encoding (see below).

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

* 2^32 -1 to 2^32 -32 : small leaves, whose contents are stored in the previous cell
* 2^32 -33 to 2^32 -64 : medium leaves, whose contents are stored in the 2 previous cells
* 2^32 -65 : zero sized leaves.
* 2^32 -254 : link.  To workaround the restriction of internal node, one of the internal node must be always stored in the previous cell of it.
* 2^32 -255 : large leaves in Plebeia.  Their contents are stored in Plebeia cells.
* 2^32 -256 : bud
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

The other child which is not referred in the index part of the internal node storage always stored in the previous cell of the internal node.

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

The tag of a small leaf is between 2^32-32 to 2^32-1 inclusive.  It denotes the length of the value in bytes, which should be stored in the previous cell of the leaf.

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

The tag of a small leaf is between 2^32-64 to 2^32-33 inclusive.  It denotes the length of the value in bytes, which should be stored in the previous cell of the leaf.

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

The tag of a medium leaf is between 2^32-64 to 2^32-33 inclusive.  It denotes the length of the value in bytes, which should be stored in the two previous cells of the leaf.

The value stored from the head of the cell at the position -2 to the previous cell.

The medium leaves are introduced to store several public keys whose sizes are 33 and 34 bytes including tags (of 1 byte) which do not fit with the small leaves.

#### Large leaf

The tag is 2^32-255.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf (P)  |<- first 224 of hash ------------>| |<- -255 ------------------------------>|
```

The contents of the value is stored in cells of the same storage.  They are stored as a linked list of *cell chunks*.  A cell chunk is a contiguous cells with the following format:

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

Hash-consing can break the invariant of the internal node: neither of its sub-nodes is stored at the previous cell of it.  To work around this, a special Link node with tag -254 is introduced to keep the invariant.  One of the sub-nodes are connected to the internal node via a link node which is placed in the previous cell of the internal node.

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

## Header

The header of Plebeia data file consists of 3 cells:

* The cell #0 is for identification of the file format.
* The cell #1 and #2 are to record the state of Plebeia tree.  These two cells are the sole cells which are overwritten.

### The cell #0: identifier

```
       |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
-------------------------------------------------------------------------------------
Cell #0|PLEBEIA\000...................\000| |<-            version                ->|
```

The current version is `1`.

### The cells #1 and #2: header

The cells #1 and #2 are to store the same contents: the current state of Plebeia tree storage.  These cells are the only cells modified during the operation.  The state is written together with its hash:

```
              |0                   19|20         23|24        27|28        31|
--------------|---------------------------------------------------------------
Cell #1 and #2|< hash of the right ->|<- i-cache ->|<- i-root ->|<- i-next ->|
```

The first 20 bytes of the cell #1 and #2 are the hash of the rest of the cell.
The rest of the cell consists of the 3 indices:

* The last index of the cache record (from bytes 20)
* The last index of the root hash record (from bytes 24)
* The index for the next fresh cell (from bytes 28)

The system writes the same contents to these header cells to the Cell #1 and #2 for crash recovery.  If the system restarts from a crash, it checks the header cells and if:

* The both cells are valid (the hashes are correct with respect to the rest of the cells) and equal:
  the header information is valid.
* The both cells are valid but not equal: the system has crashed after finishing the write to the Cell #1
  and before writing the Cell #2.  We recover using the indices recorded in Cell #1.
* If the Cell #1 is valid but #2 is invalid, then the system has crashed during the write to the Cell #2.
  We recover using the indices recorded in Cell #1.
* If the Cell #1 is invalid but #2 is valid, then the system has crashed during the write to the Cell #1.
  We recover using the indicves recorded in Cell #2.
* If the both cells are invalid, there is no way to recover.  The system must refuse to restart.

For the performance, the header should not be updated for each DB write.  If the system crashes, then the updates to the DB after the last valid header write are lost, even though they are properly written to the file.

## Root hash records

Plebeia data file keeps the commits together with the tree data.  The information of commits are called "root records".  A root record consists of 2 contiguous cells:

```
Node pointed by i-root or prev:
|0        19|20      23|24        27|28      31|
|<- meta  ->|<- prev ->|<- parent ->|<- idx  ->|

Previous cell:
|0                                           31|
|<------------------- meta2 ------------------>|
```

* Meta is to store information about the commit, such as log message or timestamp.  The format is not specified. 
* Prev is the index to the previous root on the file.
* Parent is the index to the top bud of the parent commit, if exists.  If not, it is filled with 0's.
     Note that this is NOT the index for the parent's root record.
* Idx is the index to the top bud of this commit.
* Meta2 is to store the context hash of the commit.   Currently, the contex hash commit is NOT the Merkle hash of the committed tree but given from the outside.

The index of the last root hash record is recorded in the i-root field of the header.

## Cache record

Plebeia has a cache for small data (<= 36 bytes) to share leaves with the same data to reduce the size of the data file.

```
            |0        7|8            31|
------------|---------------------------
Cache record|<- prev ->|<- 7 indexes ->|
```

* `prev` is the index points to the previous cache record.  If it does not exist, it is 0-filled.
* The rest of the cell stores 7 indices to leaves of small sized data.

The index of the last cache record is recorded in the i-cache field of the header.

The format of the cache record is fixed, but how to use it to provide caching is still under heavy development.

