/*
 * Copyright 2020 Artur Opala
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.arturopala.tree

import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.TreeOptions.TraversingMode
import com.github.arturopala.tree.TreeOptions.TraversingMode._

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * Common interface of [[Tree]] operations.
  *
  * @groupprio properties 0
  * @groupname properties Properties
  * @groupdesc properties Basic tree properties.
  * @groupprio values 10
  * @groupname values Values
  * @groupdesc values Access to values.
  * @groupprio sub-trees 20
  * @groupname sub-trees Trees
  * @groupdesc sub-trees Access to sub-trees.
  * @groupprio branches 30
  * @groupname branches Branches
  * @groupdesc branches Access to branches.
  * @groupprio checks 40
  * @groupname checks Check
  * @groupprio selection 41
  * @groupname selection Select
  * @groupprio search 42
  * @groupname search Search
  * @groupprio transformation 50
  * @groupname transformation Transform
  * @groupprio aggregation 60
  * @groupname aggregation Aggregate
  * @groupprio composition 65
  * @groupname composition Compose
  * @groupprio insertion 70
  * @groupname insertion Insert
  * @groupprio modification 72
  * @groupname modification Modify (map)
  * @groupprio removal 74
  * @groupname removal Remove
  * @groupprio optimization 80
  * @groupname optimization Optimize
  * @groupprio serialization 90
  * @groupname serialization Serialize
  * @groupprio visualization 100
  * @groupname visualization Visualize
  */
trait TreeLike[+T] {

  // PROPERTIES

  /** The number of the concrete nodes in tree.
    * @group properties */
  def size: Int

  /** The number of leafs (concrete nodes without subtrees) in tree,
    * same as a number of distinct branches starting at the root.
    * @group properties */
  def width: Int

  /** The length of the longest branch of the tree.
    * @group properties */
  def height: Int

  /** Returns true if this is a node without subtrees, otherwise false.
    * @group properties */
  def isLeaf: Boolean

  /** Returns true if this is an empty tree, otherwise false.
    * @group properties */
  def isEmpty: Boolean

  /** Returns false if this is an empty tree , otherwise true.
    * @group properties */
  final def nonEmpty: Boolean = !isEmpty

  /** Returns number of direct children trees, i.e. subtrees.
    * @group properties */
  def childrenCount: Int

  // VALUES

  /** Returns value of this node.
    * @throws NoSuchElementException if an empty tree
    * @group values */
  def head: T

  /** Returns option of a value of this node, if any.
    * @group values */
  def headOption: Option[T]

  /** Returns direct children values, i.e. values of the subtree nodes, if any.
    * @group values */
  def childrenValues: Iterable[T]

  /** Iterates over all the node's values in this tree.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @group values */
  def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T]

  /** Iterates over all the leaf's values in this tree.
    * @note Leaf is the node without children.
    * @group values */
  def leafs: Iterable[T] = ???

  /** Iterates over filtered node's values, top-down, depth-first.
    * @param pred return true to include the value in the result, false otherwise.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @param maxDepth number of levels to go inside the tree, default to max
    * @group values */
  def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T]

  /** Iterates over filtered node's values, paired with the node's level and isLeaf flag.
    * @param pred return true to include the value in the result, false otherwise.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @param maxDepth number of levels to go inside the tree, default to max
    * @return an iterable of (level, value, isLeaf)
    * @group values */
  def valuesAndLevelsWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, T, Boolean)]

  // TREES

  /** Returns direct children trees, i.e. the subtrees.
    * @group sub-trees */
  def children: Iterable[Tree[T]]

  /** Iterates over all the possible subtrees in this tree inclusive.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @group sub-trees */
  def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[T]]

  /** Iterates over filtered trees in this tree inclusive.
    * @param pred return true to include the tree in the result, false otherwise.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @param maxDepth number of levels to go inside the tree, default to max
    * @group sub-trees */
  def treesWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[T]]

  /** Iterates over filtered trees in this tree inclusive, paired with the node's level.
    * @param pred return true to include the tree in the result, false otherwise.
    * @param mode tree traversing mode, either depth-first or breadth-first
    * @param maxDepth number of levels to go inside the tree, default to max
    * @group sub-trees */
  def treesAndLevelsWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[T])]

  // BRANCHES

  /** Iterates over all the branches in this tree.
    * @group branches */
  def branches: Iterable[Iterable[T]]

  /** Iterates over filtered branches in this tree.
    * @param pred return true to include the branch in the result, false otherwise.
    * @param maxDepth maximum length of the returned branches, default to max
    * @group branches
    */
  def branchesWithFilter(pred: Iterable[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterable[Iterable[T]]

  // CHECKS

  /** Checks if given value exists in the whole tree.
    * @param value value to look for
    * @group checks */
  def containsValue[T1 >: T](value: T1): Boolean = ???

  /** Checks if value fulfilling the predicate exists in the whole tree.
    * @param pred function returning true for the searched value
    * @group checks */
  def existsValue(pred: T => Boolean): Boolean = ???

  /** Checks for the existence of the direct child holding the value.
    * @param value value to look for
    * @group checks */
  def containsChild[T1 >: T](value: T1): Boolean = ???

  /** Checks for the existence of the direct child fulfilling the predicate.
    * @param pred function returning true for the searched value
    * @group checks */
  def existsChild(pred: T => Boolean): Boolean = ???

  /** Checks if the tree contains provided branch (full path).
    * @param branch list of values forming a branch from the root to the leaf.
    * @group checks */
  def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean

  /** Checks if the tree contains provided branch (full path).
    * @param branch list of K path items forming a path from the root to the leaf
    * @param f extractor of the K path item from the tree's node value
    * @group checks */
  def containsBranch[K](branch: Iterable[K], f: T => K): Boolean

  /** Checks if the tree contains branch (full path) fulfilling the predicate.
    * @param branch list of values forming a branch from the root to the leaf.
    * @group checks */
  def existsBranch(branch: Iterable[T] => Boolean): Boolean = ???

  /** Checks if the tree contains branch (full path) fulfilling the predicate.
    * @param branch list of K path items forming a path from the root to the leaf
    * @param f extractor of the K path item from the tree's node value
    * @group checks */
  def existsBranch[K](branch: Iterable[K] => Boolean, f: T => K): Boolean = ???

  /** Checks if the tree contains provided path (prefix).
    * @param path list of values forming a path from the root to the node.
    * @group checks */
  def containsPath[T1 >: T](path: Iterable[T1]): Boolean

  /** Checks if the tree contains provided path (prefix).
    * @param path list of K path items forming a path from the root to the node
    * @param f extractor of the K path item from the tree's node value
    * @group checks */
  def containsPath[K](path: Iterable[K], f: T => K): Boolean

  /** Checks if the tree contains path (prefix) fulfilling the predicate.
    * @param path list of values forming a branch from the root to the node.
    * @group checks */
  def existsPath(path: Iterable[T] => Boolean): Boolean = ???

  /** Checks if the tree contains path (prefix) fulfilling the predicate.
    * @param path list of K path items forming a path from the root to the node
    * @param f extractor of the K path item from the tree's node value
    * @group checks */
  def existsPath[K](path: Iterable[K] => Boolean, f: T => K): Boolean = ???

  // SELECTION

  /** Selects a value of the node reachable by the provided path, if any.
    * @param path list of K path items forming a path from the root to the node
    * @param f extractor of the K path item from the tree's node value
    * @tparam K type of path item
    * @group selection */
  def selectValue[K](path: Iterable[K], f: T => K): Option[T]

  /** Selects a sub-tree anchored at the node reachable by the provided path, if any.
    * @param path list of node's values forming a path from the root to the node.
    * @group selection */
  def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]]

  /** Selects a sub-tree anchored at the node reachable by the provided path, if any.
    * @param path list of K path items forming a path from the root to the node.
    * @param f extractor of the K path item from the tree's node value
    * @tparam K type of path item
    * @group selection */
  def selectTree[K](path: Iterable[K], f: T => K): Option[Tree[T]]

  // SEARCH

  /** Finds paths leading to a given value from the root of the tree.
    * @param value value to look for.
    * @group search */
  def findPathsLeadingTo[T1 >: T](value: T1): Iterator[Iterable[T]] = ???

  // TRANSFORMATION

  /** Maps every node of the tree using provided function and returns a new tree.
    * @group transformation */
  def map[K: ClassTag](f: T => K): Tree[K]

  /** Flat-maps every node of the tree using provided function and returns a new tree.
    * Keeps all the node's children distinct.
    * @group transformation */
  def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] = ???

  /** Maps every branch of the tree using provided function and returns a new tree.
    * Keeps all the node's children distinct.
    * @group transformation */
  def mapBranches[K: ClassTag](f: Iterable[T] => Iterable[K]): Tree[K] = ???

  /** Maps every children sequence of the tree using provided function and returns a new tree.
    * @group transformation */
  def mapChildren[K: ClassTag](f: Iterable[Tree[T]] => Iterable[Tree[K]]): Tree[K] = ???

  /** Filters the tree node's values, any node which doesn't satisfy a predicate is removed,
    * and its children merged with the remaining siblings of the parent,
    * and eventually returns a whole tree updated.
    * @param keepDistinct keep combined children distinct
    * @group transformation */
  def filterValues(f: T => Boolean, keepDistinct: Boolean): Tree[T] = ???

  /** Filters all the sub-trees, this tree inclusive, any tree which doesn't satisfy a predicate is removed,
    * and eventually returns a whole tree updated.
    * @param keepDistinct keep combined children distinct
    * @group transformation */
  def filterTrees(f: Tree[T] => Boolean, keepDistinct: Boolean): Tree[T] = ???

  /** Filters all the branches, any branch which doesn't satisfy a predicate is removed,
    * and eventually returns a whole tree updated.
    * @group transformation */
  def filterBranches(f: Iterable[T] => Boolean): Tree[T] = ???

  /** Attempts to trim the branches of the tree to keep height at the given limit.
    * Trims only branches longer then the limit.
    * If the tree is already lower then the limit then returns the tree intact.
    * @param height maximum length of the branch prefix to keep
    * @group transformation */
  def trim(height: Int): Tree[T] = ???

  /** Drops all the leaves (nodes without children).
    * @group transformation */
  def dropLeaves: Tree[T] = ???

  // AGGREGATE

  /** Folds all the values of tree's nodes using provided function.
    * Starts accumulator with the initial value.
    * @param initial initial value
    * @param maxDepth number of levels to go inside the tree, default to max
    * @param f function to fold values with, accepts accumulator and a value
    * @group aggregation */
  def foldValues[A](initial: A, maxDepth: Int = Int.MaxValue)(f: (A, T) => A): A = ???

  /** Folds all the values of tree's nodes, paired with node's level, using provided function.
    * Starts accumulator with the initial value.
    * @param initial initial value
    * @param maxDepth number of levels to go inside the tree, default to max
    * @param f function to fold values with, accepts accumulator, level and a tree
    * @group aggregation */
  def foldValuesWithLevel[A](initial: A, maxDepth: Int = Int.MaxValue)(f: (A, Int, T) => A): A = ???

  /** Folds all the sub-trees, this tree inclusive, using provided function.
    * Starts accumulator with the initial value.
    * @param initial initial value
    * @param maxDepth number of levels to go inside the tree, default to max
    * @param f function to fold values with, accepts accumulator and a value
    * @group aggregation */
  def foldTrees[A](initial: A, maxDepth: Int = Int.MaxValue)(f: (A, Tree[T]) => A): A = ???

  /** Folds all the sub-trees, this tree inclusive, paired with root's level, using provided function.
    * Starts accumulator with the initial value.
    * @param initial initial value
    * @param maxDepth number of levels to go inside the tree, default to max
    * @param f function to fold values with, accepts accumulator, level and a tree
    * @group aggregation */
  def foldTreesWithLevel[A](initial: A, maxDepth: Int = Int.MaxValue)(f: (A, Int, Tree[T]) => A): A = ???

  /** Folds all the tree's branches using provided function.
    * Starts accumulator with the initial value.
    * @param initial initial value
    * @param maxDepth number of levels to go inside the tree, default to max
    * @param f function to fold values with, accepts accumulator and a value
    * @group aggregation */
  def foldBranches[A](initial: A, maxDepth: Int = Int.MaxValue)(f: (A, Iterable[T]) => A): A = ???

  /** Returns the number of distinct branches accepted by the filter, starting at the root of the tree.
    * @param pred return true to count the branch, false otherwise.
    * @group aggregation */
  def countBranches(pred: Iterable[T] => Boolean): Int

  // COMPOSITION

  /** Merges this tree with the other tree.
    * The resulting tree will is guaranteed to contain ALL values and branches existing in either input tree.
    * @param other the tree to merge with
    * @param keepDistinct keep resulting tree distinct
    * @group composition */
  def union[T1 >: T](other: Tree[T1], keepDistinct: Boolean): Tree[T1] = ???

  /** Intersects this tree with the other tree.
    * The resulting tree will is guaranteed to contain ONLY branches existing in both input trees.
    * @param other the tree to intersect with
    * @group composition */
  def intersect[T1 >: T](other: Tree[T1]): Tree[T1] = ???

  /** Subtracts the other tree from this tree.
    * The resulting tree will is guaranteed to NOT contain any branches existing in the other tree.
    * @param other the tree to subtract
    * @group composition */
  def diff[T1 >: T](other: Tree[T1]): Tree[T1] = ???

  // DISTINCT INSERTIONS

  /** Creates a new node holding the value with this tree as its single child.
    * @param value new top node value
    * @group insertion */
  def prepend[T1 >: T: ClassTag](value: T1): Tree[T1]

  /** Inserts a new node holding the value and returns updated tree.
    * Keeps all the node's children distinct.
    * @group insertion */
  def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1]

  /** Inserts, at the given path, a new child node holding the value and returns a whole tree updated.
    * If path doesn't fully exist in the tree then remaining suffix will be created.
    * Keeps all the node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param value a value to insert as a new child
    * @group insertion */
  def insertValueAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1]

  /** Attempts to insert, at the given path, a new child node holding the value and returns a whole tree updated.
    * If path doesn't fully exist in the tree then tree will remain NOT updated.
    * Keeps all the node's children distinct.
    * @param path list of K items forming a path from the root to the parent node.
    * @param f extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with existing unmodified tree
    * @group insertion */
  def insertValueAt[K, T1 >: T: ClassTag](path: Iterable[K], value: T1, f: T => K): Either[Tree[T], Tree[T1]]

  /** Inserts a new sub-tree and returns updated tree.
    * Keeps all the node's children distinct.
    * @group insertion */
  def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1]

  /** Inserts, at the given path, a new sub-tree and returns a whole tree updated.
    * If path doesn't fully exist in the tree then remaining suffix will be created.
    * Keeps all the node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @group insertion */
  def insertTreeAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1]

  /** Attempts to insert, at the given path, a new sub-tree and return a whole tree updated.
    * If path doesn't fully exist in the tree then tree will remain NOT updated.
    * Keeps all the node's children distinct.
    * @param path list K items forming a path from the root to the parent node.
    * @return either right of modified tree or left with existing unmodified tree
    * @group insertion */
  def insertTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K
  ): Either[Tree[T], Tree[T1]]

  /** Inserts a new branch of values and returns updated tree.
    * Keeps all the node's children distinct.
    * @param branch list of values forming a path from the root to the leaf.
    * @note New branch must start with the existing root element of the tree, otherwise the tree will stay intact.
    * @group insertion */
  def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1]

  // DISTINCT MODIFICATIONS

  /** Modifies the value of a child node holding a given value.
    * Keeps all the node's children distinct.
    * @param value value of the child node
    * @param modify function to modify the value
    * @return modified tree if contains the value
    * @group modification */
  def modifyValue[T1 >: T: ClassTag](value: T1, modify: T => T1): Tree[T1]

  /** Modifies the value selected by the given path, and returns a whole tree updated.
    * Keeps all the node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param modify function to modify the value
    * @return either right of modified tree or left with the tree intact
    * @group modification */
  def modifyValueAt[T1 >: T: ClassTag](path: Iterable[T1], modify: T => T1): Either[Tree[T], Tree[T1]]

  /** Modifies the value selected by the given path, and returns a whole tree updated.
    * Keeps all the node's children distinct.
    * @param path list of K items forming a path from the root to the parent node.
    * @param modify function to modify the value
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with the tree intact
    * @group modification */
  def modifyValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]]

  /** Modifies the child holding a given value.
    * Keeps all the node's children distinct.
    * @param value value of the child node
    * @param modify function to modify the value
    * @return modified tree if contains the value
    * @group modification */
  def modifyTree[T1 >: T: ClassTag](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1]

  /** Modifies the tree selected by the given path, and returns a whole tree updated.
    * Keeps all the node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param modify function transforming the tree
    * @return either right of modified tree or left with the tree intact
    * @group modification */
  def modifyTreeAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]]

  /** Modifies the tree selected by the given path, and returns a whole tree updated.
    * Keeps all the node's children distinct.
    * @param path list K items forming a path from the root to the parent node.
    * @param modify function transforming the tree
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with the tree intact
    * @group modification */
  def modifyTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]]

  // DISTINCT REMOVALS

  /** Removes direct child node holding a value, re-inserts nested children into this tree.
    * Keeps children distinct, merges down if necessary.
    * @return modified tree
    * @group removal */
  def removeValue[T1 >: T: ClassTag](value: T1): Tree[T]

  /** Removes the value selected by the given path, inserts nested children into the parent,
    * and returns a whole tree updated.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @param path list of node's values forming a path from the root to the parent node.
    * @return modified tree
    * @group removal */
  def removeValueAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T]

  /** Removes the value selected by the given path, merges node's children with remaining siblings,
    * and returns a whole tree updated.
    * @param path list of K items forming a path from the root to the parent node.
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return modified tree
    * @group removal */
  def removeValueAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T]

  /** Removes completely direct child node holding a value.
    * @return modified tree
    * @group removal */
  def removeTree[T1 >: T: ClassTag](value: T1): Tree[T]

  /** Removes the tree selected by the given path.
    * @param path list of node's values forming a path from the root to the parent node.
    * @return modified tree
    * @group removal */
  def removeTreeAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T]

  /** Removes the tree selected by the given path.
    * @param path list K items forming a path from the root to the parent node.
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return modified tree
    * @group removal */
  def removeTreeAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T]

  // SERIALIZATION

  /** Iterates over tree linearisation as pairs of (numberOfChildren, value).
    *
    * @note It is possible to build a tree back using [[TreeBuilder.fromSizeAndValuePairsIterator]] method.
    *
    *       Properties of the generated output:
    *       - every children's node value comes before parent's node value
    *       - every subtree comes before the parent node
    *       - children node's values comes in the reverse order
    *       - sum of the numberOfChildren is the size of the tree minus 1
    * @group serialization */
  def toPairsIterator: Iterator[(Int, T)]

  /** Outputs the tree linearisation as a pair of arrays.
    * - First array contains the structure.
    * - Second array contains node's values.
    *
    * @note It is possible to build a tree back using [[TreeBuilder.fromIterables]] method.
    *
    *       Properties of the generated arrays:
    *       - every children's node value/size comes before parent's node value
    *       - every subtree comes before the parent node
    *       - children node's values/sizes are listed in the reverse order (left to right)
    *       - sum of the structure array is the size of the tree minus 1
    *
    * @group serialization */
  def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1])

  /** Outputs tree linearisation as a pair of slices.
    * @group serialization */
  def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1])

  /** Outputs tree linearisation as a pair of buffers.
    * @group serialization */
  def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1])

  /** Outputs the tree's structure linearisation as an array.
    *
    *       Properties of the generated array:
    *       - every children node comes before parent node
    *       - every subtree comes before the parent node
    *       - children node's are listed in the reverse order (left to right)
    *       - sum of the array is the size of the tree minus 1
    *
    * @group serialization */
  def toStructureArray: Array[Int]

  // OPTIMIZATION

  /** Computes new version of the tree where each node have distinct children.
    * @group optimization */
  def distinct: Tree[T] = ???

  // VISUALIZATION

  /** Makes a String representation of the tree by enumerating all branches, up to the `maxDepth`.
    * @param show function to render a node value
    * @param valueSeparator string to separate nodes
    * @param branchStart string to add at the start of each branch
    * @param branchEnd string to add at the end of each branch
    * @param branchSeparator string to separate branches
    * @param maxDepth maximum path length (or tree depth) to reveal
    * @group visualization
    */
  def mkStringFromBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String

}

/** [[TreeLike]] companion object. */
object TreeLike {

  /** Useful extensions of tree interface. */
  implicit class TreeLikeExtensions[T](tree: TreeLike[T]) {
    def showAsGraph(separator: String = "\n"): String = TreeFormat.showAsGraph(tree, separator)
    def showAsArrays(separator: String = ","): String = TreeFormat.showAsArrays(tree, separator)
    def showAsPaths(separator: String): String = TreeFormat.showAsPaths(tree, separator)
  }

}
