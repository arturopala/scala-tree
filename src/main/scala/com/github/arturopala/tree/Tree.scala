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

import com.github.arturopala.tree.util._

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/** A general-purpose, covariant, immutable, low overhead,
  * efficient tree-like data structure with rich API.
  *
  * Conceptually, apart from empty, each node of the tree have:
  *   - a value, and
  *   - a collection of subtrees.
  *
  * Child is a value of a subtree of a node.
  * Tree algorithms enforce children of a node to be DISTINCT.
  *
  * Internally, there are three implementations of the Tree:
  *   - [[Tree.empty]], an empty tree singleton,
  *   - [[Tree.NodeTree]], nested hierarchy of immutable nodes (inflated tree),
  *   - [[Tree.ArrayTree]], a linear array (deflated tree).
  *
  * The idea behind having an inflated and deflated variant of the tree
  * is such that each of them exhibits different performance and memory
  * consumption characteristics.
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
sealed trait Tree[+T] {

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

  // NODES

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

  // SUB-TREES

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

  /** Outputs tree linearisation as a pair of slices. */
  def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1])

  /** Outputs tree linearisation as a pair of buffers. */
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

  // OPTIMIZATION

  /** Inflates the tree, if deflated,
    * to be represented internally by the hierarchy of linked objects.
    * @group optimization
    */
  def inflated: Tree[T]

  /** Deflates the tree, if inflated,
    * to be represented internally by two linear arrays.
    * @group optimization
    */
  def deflated[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1]

  // EQUALITY, HASH CODE, AND TO_STRING

  final override def equals(obj: Any): Boolean = obj match {
    case otherTree: Tree[T] => Tree.equals(this, otherTree)
    case _                  => false
  }

  final override def hashCode(): Int = hashcode

  // Tree is immutable so should calculate hashcode once
  protected lazy val hashcode: Int = Tree.hashCodeOf(this)

  override def toString: String =
    if (size < 50)
      s"Tree(${valueOption.get}${if (size > 1) s", ${children.map(_.toString).mkString(",")}" else ""})"
    else s"Tree(size=$size, width=$width, height=$height, hashCode=${hashCode()})"
}

/**
  * Tree companion object.
  * Hosts factory methods and concrete tree implementations.
  */
object Tree {

  /** Creates an empty Tree, same as [[Tree.empty]]. */
  final def apply[T](): Tree[T] = empty

  /** Creates a leaf tree. */
  final def apply[T](value: T): NodeTree[T] = new Leaf(value)

  /** Creates a tree having a single subtree. */
  final def apply[T](value: T, subtree: NodeTree[T]): NodeTree[T] = new Unary(value, subtree)

  /** Creates a tree having two subtrees. */
  final def apply[T](value: T, left: NodeTree[T], right: NodeTree[T]): NodeTree[T] = new Binary(value, left, right)

  /** Creates a tree node from the value and multiple subtrees */
  final def apply[T](
    value: T,
    subtree1: NodeTree[T],
    subtree2: NodeTree[T],
    subtree3: NodeTree[T],
    others: NodeTree[T]*
  ): NodeTree[T] =
    new Bunch(value, subtree1 :: subtree2 :: subtree3 :: others.toList)

  /** Creates a tree node from the value and list of subtrees */
  final def apply[T](value: T, subtrees: List[NodeTree[T]]): NodeTree[T] = subtrees match {
    case Nil           => new Leaf(value)
    case x :: Nil      => new Unary(value, x)
    case x :: y :: Nil => new Binary(value, x, y)
    case _             => new Bunch(value, subtrees)
  }

  /** Deflates the tree, if inflated, otherwise returns as is. */
  final def deflate[T: ClassTag](tree: Tree[T]): Tree[T] = tree match {
    case `empty`                 => Tree.empty
    case arrayTree: ArrayTree[T] => arrayTree
    case _ =>
      val (structure, values) = tree.toArrays
      new ArrayTree[T](IntSlice.of(structure), Slice.of(values), tree.width, tree.height)
  }

  /** Inflates the tree, if deflated, otherwise returns as is. */
  final def inflate[T](tree: Tree[T]): Tree[T] = tree match {
    case `empty`                 => Tree.empty
    case arrayTree: ArrayTree[T] => arrayTree.inflated
    case _                       => tree
  }

  /**
    * A Tree represented internally by linked node objects,
    * each node consists of a value and a list of subtrees.
    *
    * Concrete, specialized node types are [[Leaf]], [[Unary]], [[Binary]], and [[Bunch]].
    */
  sealed trait NodeTree[+T] extends Tree[T] with NodeTreeOps[T] {

    override protected val node: NodeTree[T] = this

    val value: T
    def subtrees: List[NodeTree[T]]
  }

  /** Concrete node of the Tree, consisting of a value and no subtrees. */
  final class Leaf[+T] private[Tree] (val value: T) extends NodeTree[T] {

    override def size: Int = 1
    override def width: Int = 1
    override def height: Int = 1
    override def isLeaf: Boolean = true
    override def subtrees: List[NodeTree[T]] = Nil
    override def childrenCount: Int = 0
  }

  /** Concrete node of the Tree, consisting of a value and a single subtree. */
  final class Unary[+T] private[Tree] (val value: T, val subtree: NodeTree[T]) extends NodeTree[T] {

    override val size: Int = 1 + subtree.size
    override val width: Int = Math.max(1, subtree.width)
    override val height: Int = 1 + subtree.height
    override def isLeaf: Boolean = false
    override def subtrees: List[NodeTree[T]] = List(subtree)
    override def childrenCount: Int = 1
  }

  /** Concrete node of the Tree, consisting of a value and two subtrees. */
  final class Binary[+T] private[Tree] (val value: T, val left: NodeTree[T], val right: NodeTree[T])
      extends NodeTree[T] {

    override val size: Int = 1 + left.size + right.size
    override val width: Int = Math.max(1, left.width + right.width)
    override val height: Int = 1 + Math.max(left.height, right.height)
    override def isLeaf: Boolean = false
    override def subtrees: List[NodeTree[T]] = List(left, right)
    override def childrenCount: Int = 2
  }

  /** Concrete node of the Tree, consisting of a value and a list of subtrees (more than two). */
  final class Bunch[+T] private[Tree] (val value: T, val subtrees: List[NodeTree[T]]) extends NodeTree[T] {

    override val size: Int = 1 + subtrees.map(_.size).sum
    override val width: Int = Math.max(1, subtrees.map(_.width).sum)
    override val height: Int = 1 + subtrees.maxBy(_.height).height
    override def isLeaf: Boolean = subtrees.isEmpty
    override def childrenCount: Int = subtrees.length
  }

  /**
    * A Tree represented internally by two array slices,
    * one encoding the structure, the other holding node's values.
    */
  final class ArrayTree[T] private[tree] (
    val structure: IntSlice,
    val content: Slice[T],
    delayedWidth: => Int,
    delayedHeight: => Int
  )(implicit val classTag: ClassTag[T])
      extends Tree[T] with ArrayTreeOps[T] {

    assert(content.nonEmpty, "When creating an ArrayTree, `values` must not be empty.")
    assert(
      content.length == structure.length,
      "When creating an ArrayTree, `structure` and `values` must be of the same size."
    )

    override protected val tree: ArrayTree[T] = this

    override val size: Int = tree.structure.length
    override lazy val width: Int = delayedWidth
    override lazy val height: Int = delayedHeight
    override def isLeaf: Boolean = size == 1
    override def isEmpty: Boolean = size == 0
    override def childrenCount: Int = tree.structure.last
  }

  /**
    * An empty Tree singleton.
    */
  final case object empty extends Tree[Nothing] with EmptyTreeOps {

    override protected lazy val hashcode: Int = 0
    override val toString: String = "Tree.empty"
  }

  /** Checks equality of the two trees. */
  final def equals[T](tree1: Tree[T], tree2: Tree[T]): Boolean =
    tree1.eq(tree2) || (tree1.size == tree2.size &&
      tree1.width == tree2.width &&
      tree1.height == tree2.height &&
      tree1.valueOption == tree2.valueOption && Compare.sameTrees(tree1, tree2))

  /** Computes hashcode of the tree. */
  final def hashCodeOf[T](tree: Tree[T]): Int = {
    var hash = 17
    hash = hash * 31 + tree.valueOption.hashCode()
    hash = hash * 29 + tree.size.hashCode()
    hash = hash * 13 + tree.width.hashCode()
    hash = hash * 19 + tree.height.hashCode()
    hash
  }

}
