![Scala CI](https://github.com/arturopala/scala-tree/workflows/Scala%20CI/badge.svg)

Tree\[+T]
===

This is a micro-library for Scala providing 
a general-purpose, covariant, immutable, low overhead, 
efficient tree-like data structure with rich API.

Motivation
---

A Tree is one of the most useful data structure concepts with numerous flavours and implementations.

While the concept is simple, practical implementation details of immutable tree pose significant challenges, e.g.

    - traversing and transforming the tree using stack-safe algorithms
    - using reasonable amount of memory resources to represent large trees
    - keeping children distinct when inserting or merging trees
    - supporting numerous use cases in the balanced API

Design
---

This library implements the tree which can be either empty 
or a node having a value, and linking to zero or more subtrees.

Dependencies
---

Depends only on a standard, built-in Scala library.

Cross-compilation to Scala versions `2.13.1`, `2.12.11`, and `2.11.12` is provided.

API
---

Provided API allows for a rich set of queries and operations on the tree. 

For details, consult [Scaladoc](https://arturopala.github.io/scala-tree/latest/api/com/github/arturopala/tree/Tree.html).