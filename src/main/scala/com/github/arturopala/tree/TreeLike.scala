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

import com.github.arturopala.tree.util.{Buffer, IntBuffer, IntSlice, Slice}

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * Common interface of [[Tree]] operations.
  *
  * @groupprio properties 0
  * @groupname properties Properties
  * @groupdesc properties Basic tree properties.
  * @groupprio values 10
  * @groupname values Values
  * @groupprio sub-trees 20
  * @groupname sub-trees Trees
  * @groupprio branches 30
  * @groupname branches Branches
  * @groupprio modifications 40
  * @groupname modifications Modify
  * @groupprio transformations 50
  * @groupname transformations Transform
  * @groupprio paths 60
  * @groupname paths Path-based operations
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

  /** Returns value of the tree's node, if any.
    * @group values */
  def valueOption: Option[T]

  /** Returns direct children values, i.e. values of the subtree nodes, if any.
    * @group values */
  def childrenValues: List[T]

  /** Lists all the node's values in the tree.
    * @group values */
  def values: List[T]

  /** Lists all the node's values in the tree.
    *
    * @note Uses unsafe nested recursions, result same as [[valueIterator()]].
    * @group values */
  def valuesUnsafe: List[T]

  /** Iterates over filtered node's values, top-down, depth-first.
    * @param pred return true to include the value in the result, false otherwise.
    * @group values */
  def valueIterator(pred: T => Boolean): Iterator[T]

  /** Lazy stream of the node's values in the tree.
    * @group values */
  def valueStream: Stream[T]

  /** Filtered lazy stream of the node's values in the tree.
    * @param pred return true to include the value in the result, false otherwise.
    * @group values */
  def valueStream(pred: T => Boolean): Stream[T]

  // TREES

  /** Returns direct children trees, i.e. the subtrees.
    * @group sub-trees */
  def children: List[Tree[T]]

  /** Lists all the possible subtrees in the tree inclusive.
    * Top tree is listed first, then children depth-first.
    * @group sub-trees */
  def trees: List[Tree[T]]

  /** Lists all the possible subtrees in the tree inclusive.
    * Top tree is listed first, then children depth-first.
    *
    * @note Uses unsafe nested recursions, result same as [[treeIterator()]].
    * @group sub-trees */
  def treesUnsafe: List[Tree[T]]

  /** Iterates over filtered subtrees in the tree inclusive, top-down, depth-first.
    * Top tree is returned first, then children depth-first.
    *
    * @param pred return true to include the tree in the result, false otherwise.
    * @group sub-trees */
  def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]]

  /** Lazy stream of the possible subtrees of the tree inclusive.
    * Top tree is streamed first, then children depth-first.
    *
    * @group sub-trees */
  def treeStream: Stream[Tree[T]]

  /** Filtered lazy stream of the possible subtrees in the tree inclusive.
    * Top tree is streamed first, then children depth-first.
    *
    * @param pred return true to include the subtree in the result, false otherwise.
    * @group sub-trees */
  def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]]

  // BRANCHES

  /** Lists all the branches of the tree starting at the root.
    *
    * @note Uses unsafe nested recursions, result same as [[branchIterator()]].
    * @group branches */
  def branches: List[List[T]]

  /** Lists all the branches of the tree starting at the root.
    * @group branches */
  def branchesUnsafe: List[List[T]]

  /** Iterates over filtered branches of the tree starting at the root.
    * @param pred return true to include the branch in the result, false otherwise.
    * @group branches
    */
  def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]]

  /** Lazy stream of all the branches of the tree starting at the root.
    * @group branches */
  def branchStream: Stream[List[T]]

  /** Filtered lazy stream of all the branches of the tree starting at the root.
    * @param pred return true to include the branch in the result, false otherwise.
    * @group branches */
  def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]]

  /** Returns the number of distinct branches accepted by the filter, starting at the root of the tree.
    * @param pred return true to count the branch, false otherwise.
    * @group branches */
  def countBranches(pred: Iterable[T] => Boolean): Int

  // MODIFICATION

  /** Inserts a new node holding the value and returns updated tree.
    * @group modifications */
  def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1]

  /** Inserts a new sub-tree and returns updated tree.
    * @group modifications */
  def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1]

  /** Inserts a new branch of values and returns an updated tree.
    * Keeps children distinct by default.
    * @param branch list of values forming a path from the root to the leaf.
    * @note New branch must start with the existing root element of the tree, otherwise the tree will stay intact.
    * @group modifications */
  def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1]

  // TRANSFORMATION

  /** Maps all nodes of the tree using provided function and returns a new tree.
    * @group transformations */
  def map[K: ClassTag](f: T => K): Tree[K]

  /** Maps all nodes of the tree using provided function and returns a new tree.
    * @note Uses nested recursions.
    * @group transformations */
  def mapUnsafe[K: ClassTag](f: T => K): Tree[K]

  /** Flat-maps all nodes of the tree using provided function and returns a new tree.
    * @group transformations */
  def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K]

  // PATH-BASED OPERATIONS

  /** Selects a value of the node reachable by the provided path, if any.
    * @param path list of values forming a path from the root to the node.
    * @group paths */
  def selectValue[T1 >: T](path: Iterable[T1]): Option[T]

  /** Selects a sub-tree anchored at the node reachable by the provided path, if any.
    * @param path list of values forming a path from the root to the node.
    * @group paths */
  def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]]

  /** Checks if the tree contains provided branch (full match).
    *
    * @param branch list of values forming a branch from the root to the leaf.
    * @group paths */
  def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean

  /** Checks if the tree contains provided path (prefix match).
    *
    * @param path list of values forming a path from the root to the node.
    * @group paths */
  def containsPath[T1 >: T](path: Iterable[T1]): Boolean

  // SERIALIZATION

  /** Iterates over tree linearisation as pairs of (numberOfChildren, value).
    *
    * @note It is possible to build a tree back using [[TreeBuilder.fromPairsIterator]] method.
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
  def mkStringUsingBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String

}
