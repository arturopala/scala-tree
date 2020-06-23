![Build](https://github.com/arturopala/scala-tree/workflows/Build/badge.svg)

Tree\[+T]
===

This is a micro-library for Scala providing 
a general-purpose, covariant, immutable, low overhead, 
efficient, monadic tree-like data structure with comprehensive API.

Motivation
---

A Tree is one of the most versatile data structure concepts with numerous flavours, applications and implementations.

While the concept is dead simple, practical implementation details of immutable tree pose significant challenges, e.g.

- traversing and transforming the tree using stack-safe algorithms
- using reasonable amount of memory resources to represent large trees
- keeping children distinct when transforming or merging trees
- supporting numerous use cases in the balanced API

Design
---

Conceptually, apart from an empty, each node of the tree has:
- a head value
- a collection of subtrees (children).

Internally, there are three main implementations of the `Tree`:

- `Tree.empty` an empty tree singleton, i.e. `Tree[Nothing]`,
- `Tree.NodeTree` a classic, deeply-nested hierarchy of immutable nodes (inflated tree),
- `Tree.ArrayTree` the tree encoded in an ultra-compact flat format of twin linear arrays of structure and values (deflated tree).

The reason for having an inflated and deflated variants of the tree
is such that each one exhibits different performance and memory
consumption characteristics, making it possible to experiment and optimize
for individual targets while facing the same API. 
Further optimization is possible with using `MutableTree` for series of transformations.

Distinct and Lax operations
---

By the design choice, every node of the tree can have duplicated children values,
although the default API methods preserve the uniqueness of modified elements.

If the data is distinct by itself, or you don't care about uniqueness, there is 
a matching set of `LaxTree` API operations supplied as extensions methods in `LaxTreeOps`.

Lax operations are more performant as they do not have to perform additional checks and merges.

To enable lax operations, just `import com.github.arturopala.tree.LaxTreeOps._`.

MutableTree\[+T]
---

`MutableTree` is not a `Tree` per se, but a special `TreeLike` type to handle series of heavy operations on a tree without 
having to pay a price of intermediate immutable representations. 

This works the best by using the following scenario:

- call `.mutable` on a `Tree` to access mutable copy
- make a series of operations on `MutableTree`
- call `.immutable` to get back immutable `Tree`

As both `.mutable` and `.immutable` methods have to make a copy of a Tree representation,
it saves time and resources only for two or more operations in a row, comparing with a `Tree`.

Warning: Iterating over subtrees has much worse performance on `MutableTree` because each subtree must be a copy, 
prefer using light iteration available on the `Tree` instead.


Dependencies
--- 

Depends on:

- a standard built-in Scala library,
- [`com.github.arturopala.bufferandslice`](https://github.com/arturopala/buffer-and-slice).

Cross-compiles to Scala versions `2.13.1`, `2.12.11`, `2.11.12`, and Dotty `0.25.0-RC1`.

API
---

Provided API allows for a rich set of queries and operations on the tree. 

For more details, see [Scaladoc](https://arturopala.github.io/scala-tree/latest/api/com/github/arturopala/tree/Tree.html).
