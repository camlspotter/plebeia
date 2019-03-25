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

```
|< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
internal  |<- first 222 of hash -------->|D|0| |<- the index of one of the child ----->|
extender  |0*1|<- segment ---------------->|1| |<- the index of the child ------------>|
leaf (S)  |<- first 224 of hash ------------>| |<- 2^32 - 33 to 2^32 - 1 ------------->|  (use the previous cell)
leaf (L)  |<- first 224 of hash ------------>| |<- 2^32 - 33 ------------------------->|
bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 34 ------------------------->|
empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 34 ------------------------->|
```

This layout restricts the maximum index number to 2^32-35.

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

The last 32bits are between 2^32 - 32 to 2^32 - 1 inclusive.  Its value `x` denotes 
the length of the value, which should be stored in the previous cell of the leaf.
The length of the value is 2^32 + 33 - `x`, from 1 to 32.

The value stored from the head of the previous cell.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf      |<- first 224 of hash ------------>| |<- 2^32 - 32 to 2^32 - 1 ------------->|  (may use the previous cell)
```

#### Large leaf

The last 32bits is 2^32 - 33.  The value is stored in the KVS.

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
leaf (L)  |<- first 224 of hash ------------>| |<- 2^32 - 33 ------------------------->|
```

### Bud

The last 32bits is fixed to 2^32 - 34.

#### Non-empty bud

The index of the child node is stored from 193rd bits, following 192bits of zeros:

```
          |< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
bud       |<- 192 0's ->|<-   child index  ->| |<- 2^32 - 34 ------------------------->|
```

#### Empty bud

224bits of ones are prepended in front of the 32bits of 2^32-34

```
|< ----   224 bits --------------->| |<------- 32 bits --------------------->|
----------------------------------------------------------------------------------------
empty bud |<- 1111111111111111111111111111 ->| |<- 2^32 - 34 ------------------------->|
```
