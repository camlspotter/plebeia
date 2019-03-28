## Hash format

Hash of a node has 448bit length.

`H(x) = Blake2B(28, x)`

```
leaf      |<-      H(0x00 || v)        ->|
          |                              |0...........................01|

empty bud |0000000000000000000000000000000000000000000000000000000000000|

bud       |<------------------ hash of its child ---------------------->|

internal  |<-     H(0x01 || l || h)    ->|
          |                          |0|0|0...........................01|

extender  |<-                       H_child                           ->|
          | The first 224bits of H_child |0*1|<- segment bits ------->|1|
```

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

The hash of an internal node is `H(0x01 || l || h)` whose last 2bits are
reset to zero, followed by 223bits of zeros and 1bit of one:

```
internal  |<-     H(0x01 || l || h)    ->|
          |                          |0|0|0...........................01|
```

### Extender

The hash of an extender is the first 28bytes of its child's hash,
followed by 224bits of its segment encoding (see below).

```
extender  |<-                       H_child                           ->|
          | The first 224bits of H_child |0*1|<- segment bits ------->|1|
```

#### Segment encoding

*Segment encoding* is 224bit length data.  It is:

* ended by 1bit of one,
* prepended by bits of segments, top comes the first and bottom comes the last, 0 is for left and 1 for right,
* prepended 1bit of one,
* then prepended as many bits of zeros to make the entire length become 224bits:

```
          |0*1|<- segment bits ------->|1|
```

From the definition of the segment encoding, the maximum length of segments is 224-2 = 222.

## Storage

Nodes are stored in disk storages using the following 256bit (32bytes) *cells*:

### Integer encoding

Integers are embedded in cells using little endian.

### Index part

The last 32bits of a node cell are called the index part of the node, which often
refers to a node linked from the node.  The possible index values are from 0
to 2^32 - 1 - 256.

### Tags

The index part value more than 2^32 - 256 to 2^32 - 1 are used for *tags*.

* 2^32 -1 to 2^32-32 : small leaves, whose contents are stored in the previous cell
* 2^32 -33 : bud
* 2^32 -34 : large leaves, whose contents are stored in an external KVS
* 2^32 -35 : large leaves in Plebeia.  Their contents are stored in Plebeia cells.
* Others from 2^32 -256 : reserved

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->| (also refers to the previous cell)
extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
leaf (S)  |<- first 224 of hash ------------>| |<- -1 to -32 ------------------------->| (use the previous cell)
leaf (K)  |<- first 224 of hash ------------>| |<- -33 ------------------------------->| (use the KVS)
leaf (P)  |<- first 224 of hash ------------>| |<- -35 ------------------------------->| (use the previous cell and some others)
bud       |<- 192 0's ->|<-   child index  ->| |<- -34 ------------------------------->|
empty bud |<- 1111111111111111111111111111 ->| |<- -34 ------------------------------->|
reserved  |                                  | |<- -256 to -36 ----------------------->|
```

This layout restricts the maximum index number to 2^32-257.

### Internal

* The first 222bits are the first 222bits of the node's hash.
* appended by 1bit `D`, which denotes which child's index is referred in the last 32bits. 0: left, 1: right.
* appended by 1bit of zero
* followed by 32bits of the index of the one of the children specified by `D`

The other child which is not referred in the last 32bits of the internal node storage always stored
in the previous cell.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->|
```

### Extender

* The first 224bits are the segment encoding of the extender.
* The following 32bits are the index of the child.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
```

### Leaf

* The first 224bits are the first 224 bits of the hash of the leaf.

#### Small leaf

The tag is between 2^32-32 to 2^32-1 inclusive.
It denotes the length of the value in bytes, which should be stored in the previous cell of the leaf.

The value stored from the head of the previous cell.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf      |<- first 224 of hash ------------>| |<- -32 to -1 ------------------------->|  (may use the previous cell)

Previous cell:
          |< ----   256 bits --------------------------------------------------------->|
----------------------------------------------------------------------------------------
data      |<- value contents ------------------------->|                               |
```

#### Large leaf in KVS

The tag is 2^32-33.  The value is stored in an external KVS.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf (K)  |<- first 224 of hash ------------>| |<- -33 ------------------------------->|
```

#### Large leaf in Plebeia

The tag is 2^32-35.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf (P)  |<- first 224 of hash ------------>| |<- -35 ------------------------------->|
```

The contents of the value is stored in cells of the same storage.  They are stored as
a linked list of *cell chunks*.  A cell chunk is a contiguous cells with the following
format:

##### Cell chunk

The first cell chunk ends at the previous cell of the value node.
The last 32bits of the chunk is the *cdr index*, the index of the last cell of the next chunk.
If there is none following, it is 0.

The 16bits before the cdr index is the number of the cells in this chunk;
The maximum size of a chunk is about 65KB.

The 32bits before the 16bits of the number of the cells is the total number of the bytes
of the value.  Theoretically, the maximum value size storable by this format is about 4.2GB.
(This is unnecessarily big, but only 65KB is stored with 16bits size information.)

```
         | cell #1 | cell #2 | .. | the last cell in the chunk (#n)                  | 
         |         |         |    |                 |<-32bits->|<-16bits->|<-32bits->|
---------------------------------------------------------------------------------------
1st chunk|<- contents ----------------------------->|size      | n cells  |cdr index |
```

The following cell chunks have almost the same format as the first, but the area of
the number of the total bits is used to store the contents:

```
         | cell #1 | cell #2 | .. | the last cell in the chunk (#n)                  | 
         |         |         |    |                            |<-16bits->|<-32bits->|
---------------------------------------------------------------------------------------
1st chunk|<- contents ---------------------------------------->| n cells  |cdr index |
```

The content bits are stored from the head of the first cell of the first cell chunk,
then following to the first cell of the next chunk, and so on.

### Bud

The last 32bits is fixed to 0.

#### Non-empty bud

The index of the child node is stored from 193rd bits, following 192bits of zeros:

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
bud       |<- 192 0's ->|<-   child index  ->| |<- -34 ------------------------------->|
```

#### Empty bud

224bits of ones are prepended in front of the 32bits of 2^32-34

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
empty bud |<- 1111111111111111111111111111 ->| |<- -34 ------------------------------->|
```

#### Reserved

The other tags down to 2^32-256 are reserved for future extension.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
empty bud |<- 1111111111111111111111111111 ->| |<- Unused tag down to -256 ----------->|
```

