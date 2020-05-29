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
- a value
- a collection of subtrees (children).

By the design choice, every node possibly have duplicated children values,
although default set of modifications methods assumes and preserve uniqueness.

If the data is distinct by itself, or you don't care about uniqueness, there is 
a matching set of lax operations supplied as extensions methods in `LaxTreeOps`.

Internally, there are three main implementations of the `Tree`:

- `Tree.empty` an empty tree singleton, i.e. `Tree[Nothing]`,
- `Tree.NodeTree` a classic, deeply-nested hierarchy of immutable nodes (inflated tree),
- `Tree.ArrayTree` the tree encoded in an ultra-compact flat format of twin linear arrays of structure and values (deflated tree).

The reason for having an inflated and deflated variants of the tree
is such that each one exhibits different performance and memory
consumption characteristics, making it possible to experiment and optimize
for individual targets while facing the same API.

Dependencies
---

Depends on:

- a standard built-in Scala library,
- [`com.github.arturopala.bufferandslice`](https://github.com/arturopala/buffer-and-slice).

Cross-compiles to Scala versions `2.13.1`, `2.12.11`, `2.11.12`, and Dotty `0.23.0-RC1`.

API
---

Provided API allows for a rich set of queries and operations on the tree. 

For more details, see [Scaladoc](https://arturopala.github.io/scala-tree/latest/api/com/github/arturopala/tree/Tree.html).
