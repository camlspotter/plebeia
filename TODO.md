# Plebeia TODOs

## Hashcons cache size

### Problem

Small sized values (from 1 to 36bytes) on leaves are cached in memory to share the nodes, but the size of the cache table should be now pretty huge without any restriction.  The cache information is also saved to the data file for persistency, but it is not for random query.

ノードの総数を減らすために、hashconsing をしているが、今のところ、Leaf ノードの小さいデータ(36bytesまで)に限っている。一部のデータサイズに関しては有効であろうと思われるが、全体を全てメモリ上にキャッシュしているとメモリ使用量がバカにならない。キャッシュに関するデータはファイルに保存しているので、再立ち上げ時にもその情報は利用できるが、この構造はランダムアクセスには全く向いていない。

### Current status

A heuristic is installed to wipe out non frequently appearing values from the cache table.  But this is done in a very ad-hoc way.

現在、あまり頻繁に登場しない値についてはメモリ上のテーブルから追放することでメモリ使用量の増加を抑えている。ただし、アルゴリズムは適当で、それを正当化する根拠が無い。

### Tasks

* Check how much leaves we need if there is no caching.
* Check how much leaves we can reduce if there is a perfect caching, and how much memory is required for the cache table.
* Redesign the heuristics and implement and benchmark them.
* Think about storing part of cache table in other DB, such as Irmin2.

* キャッシュがなければどれくらい性能が悪くなるのかちゃんとベンチをとる。
* 完全にキャッシュした場合どれくらい性能がよくなるのか、どれくらいのメモリがそのために必要なのかベンチをとる。
* 以上の結果と追加の実験からより良いキャッシュ法を考える。
* 一部キャッシュテーブルを Irmin2 などの他の高速なDBに蓄えることを考える。

## Segment optimization

### Problem

The segment is implemented as a list of `Left` and `Right`.  Trivial, but inefficient.

現在、セグメントは `Left` と `Right` のリストで実装されている。メモリ使用的に非常に効率が悪い。

### Current status

Segment decoding from bits to the list of `Left` and `Right` is now done lazily, which has significantly improved the traversal speed of nodes to 200%.  The correctness is not very assured, however.  Especially, the equality of segments are no longer possible by simple polymorhpic equality of OCaml.

セグメントはビット列としてファイルに保存されている。これを `Left`と`Right`の列に変換することをlazyに行なった結果、ノードを回るスピードが二倍になった。が、正しさはテストしていない。この最適化により、セグメントの等値関係には OCaml の `=` を使えなくなった。

### Tasks

* Check all the comparison of segments and make sure no polymorphic comparison is used.
* Write tests of segments with this lazy segment decoding.
* Consider an efficient representation of segments to reduce the cost of deconding and encoding.

* セグメント比較を全て調べ、`=`を使っていないことを確かめる。
* ちゃんとテストを書く
* 単にデコーディングをlazyにするだけでなく、サイズ的、スピード的により良いセグメントの実装を考える。

## Copy GC

### Problem

There is no GC at this moment.

### Current status

For GC, we first need node traversal.  I built a fast node traversal using the following properties:

GC のためにはノードの高速なトラバーサルが必要。次の性質をつかって割と早い実装を書くことができた:

Nodes reachable from a commit are either:

* Leaves with small data under 36 bytes (inclusive), managed by the hashcons caching,
* Nodes reachable from the previous commit.  Their indices are smaller than the index of the root node of the previous commit,
* Or, new node created for the commit.  Their indices are strictly larget than the index of the root node of the previous commit.

This propery holds only if each commit is recorded on the disk atomicly, from parents to children.  Otherwise, it takes days.

あるコミットから到達可能なノードは次のうちいずれかである:

* 36bytes 以下の小さい値を持つ Leaf ノード。これはhashconsのキャッシュシステムが管理している。
* 親コミットから到達可能なノード。これらのindex(ディスク位置)は必ず親コミットの根ノードのindexよりも等しいか小さい(古い)。
* それ以外は全てこのコミットで作られたノード。これらのindexは必ず親コミットの根ノードのindexより大きい(新しい)。

この性質は各コミットが必ず親子関係の順序を保ってアトミックにディスクに書き込まれる、現在の実装に依存している。

By the help of this property, the current node traversal algorithm visits all the 671 million nodes in 1846 seconds.

この性質を利用して、6.71億ノードを1846秒で巡回することができている(Core i7, 32GB memory, 1TB SSD)。使用しないと何日もかかる。

### Tasks

* Write a copy function from one file to the other, using this node traversal.  The correctness is checkable by comparing top Merkle hashes.
* Elaborate the above copy algorithm to a copy GC, which only keeps the data of specified set of commits.
* Concurrent GC, or GC with less stop-the-world time.
* Non-copy, in-place GC, or GC requires less extra space.  Should we keep the above property?

* まず、このトラバーサルを使って単なるデータのコピーツールを書く。正しさは各コミットのroot Merkle hashを比べれば確かめられる。
* このコピーを copy GC に進化させる。与えられたコミットについてのみのデータを残して、残りを捨てる。
* Stop-the-world GC から Concurrent GC への発展。もしくは stop time の短い stop-the-world GC。
* Copy しない in-place GC。うまくやらないと上の性質が崩れる恐れがある。崩してでも利があればやる価値はあるが。

# Tezos node integration

Our goal is to integrate Plebeia as a storage system to Tezos node.

## Current status

A node with 2 storage systems, the original and Plebeia, is working properly.  No conflicts are observed between the systems.

現在、オリジナルとPlebeiaの二つのストレージを並行に動かすノードを作って、正常に動いている。この二つのシステムの読み出し結果を比べているが、齟齬が発見されたことはない。

* Context hash is calculated by the original system.  Plebeia cannot provide the same, so if Plebeia is going to override it, the node has to change the calculation from the specified block level.
* Plebeia performs path compression: it requires the schema of the paths of blockchain state file sytstem.  Currenlty it is given in an ad-hoc way, by a hard coded table.  This does not work if new uses of paths are introduced by updates of the node, especially the protocol amendment.

* Context hash (Merkle hashのようなもの)はオリジナルのシステムが計算している。Plebeia は違う格納方式を使っているので同じ値を生成することはできない。もしPlebeiaがストレージシステムを担うとすると、あるブロックレベルからcontext hashを Plebeia の Merkle hash に切り替えるなどしなければいけない。
* Plebeia は path 名の圧縮を行なっている。これにはブロックチェーンの状態を扱うファイルシステムのスキーマが必要。現在はプロトコルの実装からこのスキーマを抜き出したテーブルを手作りしてPlebeiaに与えている。プロトコルが更新されるなど、ノードに変更があって新しい path が作られた場合、この手作りスキーマでは対応できず、例外を吐いて停止する。これは実際に Babylon プロトコルのテストで発生した。

## Tasks

* The protocol and the storage must talk in a better path format, not in simple strings but in more typeful data.
* Should the protocol reveal all the possible paths to the storage layer?  Is it possible in the current protocol design without much modifications?
* The protocol amendment itself should not migrate the data file from the original to Plebeia at once, since it takes too long time.  It should be better to give the users an option when to migrate.  The migration itself also should not stop the node too long. 

* プロトコルとストレージは文字列ではなく、もっと情報量のあるデータで話さなければいけない。
* プロトコルは使用する可能性のあるすべての path のデータを schema としてストレージシステムに提供すべきか？現在のプロトコル実装を大きく変えずに、そんな「可能性のあるすべてのpathの形」を事前提供できるか？
* プロトコルアップデートによって、一気に現在のストレージからPlebeiaにノードを切り替えた場合、非常に長い時間がかかる恐れがある。ユーザーが切り替え時期を選べるべきか。また移行するにしてもそれによってノードのダウンタイムが長くなっては困る。

# Tezos protocol amendment for better storage

The current protocol speaks only in simple paths to the storage subsystem, which misses lots of optimization opportunity in the storage.  The storage code is too dependent on the current LMDB+Irmin storage implementation and is hard to change/try other storage subsystems.

現在のプロトコルはストレージサブシステムとは文字列でしか話さないため、ストレージシステムは多くの最適化機会を失っている。また、ストレージ周辺のコードは現在のLMDB+Irminの実装にあまりに特化した書き方をされているため、他のストレージシステムに変更したり、テストすることが非常に難しい。

Our protocol amendement should be the following general things, not limited to Plebeia:

* Typeful paths talked between the protocol and the storage subsystem.
* The schema of the blockchain state filesystem. (???)
* Clean-up of APIs around Context storage.
* Seemless transition from one to other storage subsytems. (What about contex hashes? They are different in subsystems.)

プロトコル変更は、Plebeia に特定されたものではなく、より一般化されたものになるだろう:

* プロトコルとストレージはより型情報のある path を話すようにする
* プロトコルが全 path に関するスキーマをストレージに提供できるよにする(???)
* ブロックチェーンステートに関するストレージAPIの整理
* ダウンタイムの無いストレージサブシステムの移行(異なるサブシステム間でのContext hashはどうするのか?)
