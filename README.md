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

This library implements the tree which can be either empty 
or a node having a value, and linking to zero or more subtrees.

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
