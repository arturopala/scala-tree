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

import scala.annotation.tailrec
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
  *   - [[Tree.Node]], nested hierarchy of immutable nodes (inflated tree),
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

  // NODES

  /** Returns value of the tree's node, if any.
    * @group values */
  def valueOption: Option[T]

  /** Returns direct children values, i.e. values of the subtree nodes, if any.
    * @group values */
  def childrenValues: List[T]

  /** Lists all the node's values in the tree.
    * @group values */
  def values(): List[T]

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
  def childrenTrees: List[Tree[T]]

  /** Lists all the possible subtrees in the tree inclusive.
    * @group sub-trees */
  def trees(): List[Tree[T]]

  /** Lists all the possible subtrees in the tree inclusive.
    *
    * @note Uses unsafe nested recursions, result same as [[treeIterator()]].
    * @group sub-trees */
  def treesUnsafe: List[Tree[T]]

  /** Iterates over filtered subtrees in the tree inclusive, top-down, depth-first.
    * @param pred return true to include the tree in the result, false otherwise.
    * @group sub-trees */
  def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]]

  /** Lazy stream of the possible subtrees of the tree inclusive.
    * @group sub-trees */
  def treeStream: Stream[Tree[T]]

  /** Filtered lazy stream of the possible subtrees in the tree inclusive.
    * @param pred return true to include the subtree in the result, false otherwise.
    * @group sub-trees */
  def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]]

  // BRANCHES

  /** Lists all the branches of the tree starting at the root.
    *
    * @note Uses unsafe nested recursions, result same as [[branchIterator()]].
    * @group branches */
  def branches(): List[List[T]]

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
  def insert[T1 >: T](value: T1): Tree[T1]

  /** Inserts a new sub-tree and returns updated tree.
    * @group modifications */
  def insert[T1 >: T](subtree: Tree[T1]): Tree[T1]

  /** Inserts a new branch of values and returns updated tree.
    * @param branch list of values forming a path from the root to the leaf.
    * @note New branch must start with the existing root element of the tree, otherwise the tree will stay intact.
    * @group modifications */
  def insert[T1 >: T](branch: List[T1]): Tree[T1]

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
    * @note It is possible to build a tree back using [[Tree.Builder.fromPairsIterator]] method.
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
    * @note It is possible to build a tree back using [[Tree.Builder.fromIterables]] method.
    *
    *       Properties of the generated arrays:
    *       - every children's node value/size comes before parent's node value
    *       - every subtree comes before the parent node
    *       - children node's values/sizes are listed in the reverse order (left to right)
    *       - sum of the structure array is the size of the tree minus 1
    *
    * @group serialization */
  def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1])

  /** Outputs the tree linearisation as a pair of slices. */
  def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1])

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

  /** Inflates the tree, if deflated, to be represented internally by hierarchy of immutable nodes.
    * @group optimization
    */
  def inflate: Tree[T]

  /** Deflates the tree, if inflated, to be represented internally by two linear arrays.
    * @group optimization
    */
  def deflate[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1]

  // EQUALITY, HASH CODE, AND TO_STRING

  final override def equals(obj: Any): Boolean = obj match {
    case otherTree: Tree[T] => Tree.equals(this, otherTree)
    case _                  => false
  }

  final override def hashCode(): Int = hashcode

  // Tree is immutable so should calculate hashcode once
  private lazy val hashcode: Int = Tree.hashCodeOf(this)

  final override def toString: String =
    if (size < 50)
      s"Tree(${valueOption.get}${if (size > 1) s", ${childrenTrees.map(_.toString).mkString(",")}" else ""})"
    else s"Tree(size=$size, width=$width, height=$height, hashCode=${hashCode()})"
}

/** Tree companion object.
  * Hosts factory methods, builders, flatMap strategies, helpers and visualisation templates.
  */
object Tree {

  /** Creates an empty Tree, same as [[Tree.empty]]. */
  final def apply[T](): Tree[T] = empty

  /** Creates a leaf tree. */
  final def apply[T](value: T): Node[T] = new Leaf(value)

  /** Creates a tree having a single subtree. */
  final def apply[T](value: T, subtree: Node[T]): Node[T] = new Unary(value, subtree)

  /** Creates a tree having two subtrees. */
  final def apply[T](value: T, left: Node[T], right: Node[T]): Node[T] = new Binary(value, left, right)

  /** Creates a tree node from the value and multiple subtrees */
  final def apply[T](value: T, subtree1: Node[T], subtree2: Node[T], subtree3: Node[T], others: Node[T]*): Node[T] =
    new Bunch(value, subtree1 :: subtree2 :: subtree3 :: others.toList)

  /** Creates a tree node from the value and list of subtrees */
  final def apply[T](value: T, subtrees: List[Node[T]]): Node[T] = subtrees match {
    case Nil           => new Leaf(value)
    case x :: Nil      => new Unary(value, x)
    case x :: y :: Nil => new Binary(value, x, y)
    case _             => new Bunch(value, subtrees)
  }

  final def deflate[T: ClassTag](tree: Tree[T]): Tree[T] = tree match {
    case `empty`                 => Tree.empty
    case arrayTree: ArrayTree[T] => arrayTree
    case _ =>
      val (structure, values) = tree.toArrays
      new ArrayTree[T](IntSlice.of(structure), Slice.of(values), tree.width, tree.height)
  }

  final def inflate[T](tree: Tree[T]): Tree[T] = tree match {
    case `empty`                 => Tree.empty
    case arrayTree: ArrayTree[T] => arrayTree.inflate
    case _                       => tree
  }

  @`inline` private def all[A]: A => Boolean = _ => true

  /** A Tree represented by the hierarchy of nodes, each one consisting of a value and a list of subtrees.
    * Concrete node types implemented by [[Leaf]], [[Unary]], [[Binary]], and [[Bunch]].*/
  sealed trait Node[+T] extends Tree[T] {

    val value: T
    def subtrees: List[Node[T]]

    override def valueOption: Option[T] = Some(value)

    override def values(): List[T] = NodeTree.values[T](all, this)
    override def valuesUnsafe: List[T] = value :: subtrees.flatMap(_.valuesUnsafe)
    override def valueIterator(pred: T => Boolean): Iterator[T] = NodeTree.valueIterator(pred, this)
    override def valueStream: Stream[T] = valueStream(all)
    override def valueStream(pred: T => Boolean): Stream[T] = NodeTree.valueStream(pred, this)
    override def childrenValues: List[T] = subtrees.map(_.value)

    override def childrenTrees: List[Tree[T]] = subtrees
    override def trees(): List[Tree[T]] = NodeTree.trees[T](all, this)
    override def treesUnsafe: List[Tree[T]] = this :: subtrees.flatMap(_.treesUnsafe)
    override def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]] = NodeTree.treeIterator(pred, this)
    override def treeStream: Stream[Tree[T]] = treeStream(all)
    override def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]] = NodeTree.treeStream(pred, this)

    override def branches(): List[List[T]] = NodeTree.branches[T](all, this)
    override def branchesUnsafe: List[List[T]] = subtrees match {
      case Nil => List(List(value))
      case _ =>
        subtrees.flatMap(_.branchesUnsafe).map(value :: _)
    }

    override def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]] =
      NodeTree.branchIterator(pred, this)

    override def branchStream: Stream[List[T]] = branchStream(all).map(_.toList)
    override def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]] = NodeTree.branchStream(pred, this)
    override def countBranches(pred: Iterable[T] => Boolean): Int = NodeTree.countBranches(pred, this)

    override def insert[T1 >: T](newValue: T1): Tree[T1] = Tree(value, Tree(newValue) :: subtrees)
    override def insert[T1 >: T](subtree: Tree[T1]): Tree[T1] = subtree match {
      case `empty`        => this
      case node: Node[T1] => Tree(value, node :: subtrees)
    }

    override def insert[T1 >: T](branch: List[T1]): Tree[T1] =
      branch match {
        case `value` :: xs => NodeTree.insert(this, xs)
        case _             => this
      }

    override def map[K: ClassTag](f: T => K): Tree[K] = {
      val (structure, values) = NodeTree.arrayMap(f, this)
      Builder.fromIterators(structure.iterator, values.iterator).headOption.getOrElse(empty)
    }

    override def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = {
      def mapNodeUnsafe(n: Node[T]): Node[K] = Tree(f(n.value), n.subtrees.map(mapNodeUnsafe))
      mapNodeUnsafe(this)
    }

    override def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] = {
      val list: List[(Int, Tree[K])] = NodeTree.listFlatMap(f, List((subtrees.size, f(value))), subtrees)
      Builder.fromTreeList(list, Nil, 0, FlatMapStrategy.JoinSubtrees).headOption.getOrElse(empty)
    }

    override def selectValue[T1 >: T](path: Iterable[T1]): Option[T] = NodeTree.selectTree(this, path).map(_.value)
    override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] = NodeTree.selectTree(this, path)
    override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean = NodeTree.containsBranch(this, branch)
    override def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(this, path)

    override def toPairsIterator: Iterator[(Int, T)] = NodeTree.toPairsList(this).iterator
    override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) = NodeTree.toArrays(this)
    override def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = NodeTree.toSlices(this)
    override def toStructureArray: Array[Int] = NodeTree.toStructureArray(this)
    override def mkStringUsingBranches(
      show: T => String,
      valueSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String,
      maxDepth: Int = Int.MaxValue
    ): String = {
      val string = show(value)
      subtrees match {
        case Nil => branchStart + string + branchEnd
        case _ =>
          NodeTree
            .mkStringUsingBranches(this, show, valueSeparator, branchSeparator, branchStart, branchEnd, maxDepth)
            .mkString
      }
    }

    override def inflate: Tree[T] = Tree.inflate(this)
    override def deflate[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1] = Tree.deflate[T1](this)
  }

  /** Concrete node of the Tree, consisting of a value and no subtrees. */
  final class Leaf[+T] private[Tree] (val value: T) extends Node[T] {
    @`inline` override def size: Int = 1
    @`inline` override def width: Int = 1
    @`inline` override def height: Int = 1
    @`inline` override def isLeaf: Boolean = true
    @`inline` override def subtrees: List[Node[T]] = Nil
  }

  /** Concrete node of the Tree, consisting of a value and a single subtree. */
  final class Unary[+T] private[Tree] (val value: T, val subtree: Node[T]) extends Node[T] {
    override val size: Int = 1 + subtree.size
    override val width: Int = Math.max(1, subtree.width)
    override val height: Int = 1 + subtree.height
    override def isLeaf: Boolean = false
    override def subtrees: List[Node[T]] = List(subtree)
  }

  /** Concrete node of the Tree, consisting of a value and two subtrees. */
  final class Binary[+T] private[Tree] (val value: T, val left: Node[T], val right: Node[T]) extends Node[T] {
    override val size: Int = 1 + left.size + right.size
    override val width: Int = Math.max(1, left.width + right.width)
    override val height: Int = 1 + Math.max(left.height, right.height)
    override def isLeaf: Boolean = false
    override def subtrees: List[Node[T]] = List(left, right)
  }

  /** Concrete node of the Tree, consisting of a value and a list of subtrees (more than two). */
  final class Bunch[+T] private[Tree] (val value: T, val subtrees: List[Node[T]]) extends Node[T] {
    override val size: Int = 1 + subtrees.map(_.size).sum
    override val width: Int = Math.max(1, subtrees.map(_.width).sum)
    override val height: Int = 1 + subtrees.maxBy(_.height).height
    override def isLeaf: Boolean = subtrees.isEmpty
  }

  /** A Tree represented by two arrays, one encoding the structure, the other holding node's values. */
  final class ArrayTree[T: ClassTag] private[tree] (
    structure: IntSlice,
    content: Slice[T],
    delayedWidth: => Int,
    delayedHeight: => Int
  ) extends Tree[T] {

    assert(!values.isEmpty, "When creating an ArrayTree, `values` must not be empty.")
    assert(
      values.length == structure.length,
      "When creating an ArrayTree, `structure` and `values` must be the same size."
    )

    def rootIndex: Int = structure.length - 1

    override val width: Int = delayedWidth
    override val height: Int = delayedHeight

    override def size: Int = structure.length
    override def isLeaf: Boolean = structure.length == 1

    override def valueOption: Option[T] = Some(content(rootIndex))
    override def values(): List[T] = content.reverseIterator.toList
    @`inline` override def valuesUnsafe: List[T] = values()
    override def valueIterator(pred: T => Boolean): Iterator[T] = content.reverseIterator(pred)
    @`inline` override def valueStream: Stream[T] = valueStream(all)
    override def valueStream(pred: T => Boolean): Stream[T] = streamFromIterator(valueIterator(pred))
    override def childrenValues: List[T] = ArrayTree.childrenIndexes(rootIndex, structure).map(content)

    override def childrenTrees: List[Tree[T]] =
      ArrayTree.childrenIndexes(rootIndex, structure).map(ArrayTree.treeAt(_, structure, content))

    override def trees(): List[Tree[T]] = treeIterator(all).toList
    @`inline` override def treesUnsafe: List[Tree[T]] = trees()
    override def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]] =
      ArrayTree.treeIterator(rootIndex, structure, content, pred)

    @`inline` override def treeStream: Stream[Tree[T]] = treeStream(all)
    override def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]] = streamFromIterator(treeIterator(pred))

    @`inline` override def branches(): List[List[T]] = branchIterator(all).map(_.toList).toList
    @`inline` override def branchesUnsafe: List[List[T]] = branches()
    override def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]] =
      ArrayTree.branchIterator(rootIndex, structure, content, pred)

    @`inline` override def branchStream: Stream[List[T]] = branchStream(all).map(_.toList)
    override def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]] =
      streamFromIterator(branchIterator(pred))

    override def countBranches(pred: Iterable[T] => Boolean): Int =
      ArrayTree.countBranches(rootIndex, structure, content, pred)

    override def insert[T1 >: T](value: T1): Tree[T1] = ???
    override def insert[T1 >: T](subtree: Tree[T1]): Tree[T1] = ???
    override def insert[T1 >: T](branch: List[T1]): Tree[T1] = ???

    override def map[K: ClassTag](f: T => K): Tree[K] =
      new ArrayTree[K](structure, content.map(f), width, height)

    override def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = map(f)

    override def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] =
      ArrayTree.flatMap(structure, content, f)

    override def selectValue[T1 >: T](path: Iterable[T1]): Option[T] =
      ArrayTree.selectValue(path, rootIndex, structure, content)

    override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
      ArrayTree.selectTree(path, rootIndex, structure, content)

    override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
      ArrayTree.containsBranch(branch, rootIndex, structure, content)

    override def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
      ArrayTree.containsPath(path, rootIndex, structure, content)

    override def toPairsIterator: Iterator[(Int, T)] = ???
    override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) =
      (structure.toArray, content.toArray.asInstanceOf[Array[T1]])

    override def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = (structure, content.asInstanceOf[Slice[T1]])

    override def toStructureArray: Array[Int] = structure.toArray

    override def mkStringUsingBranches(
      show: T => String,
      valueSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String,
      maxDepth: Int
    ): String =
      ArrayTree
        .mkStringUsingBranches(
          rootIndex,
          structure,
          content,
          show,
          valueSeparator,
          branchSeparator,
          branchStart,
          branchEnd,
          maxDepth
        )
        .toString()

    override def inflate: Tree[T] =
      Builder.fromIterators(structure.iterator, content.iterator).headOption.getOrElse(Tree.empty)

    override def deflate[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1] = Tree.deflate[T1](this)

    private def streamFromIterator[A](it: Iterator[A]): Stream[A] =
      if (it.hasNext) {
        new Stream.Cons(it.next(), streamFromIterator(it))
      } else Stream.Empty

  }

  /** An empty Tree. */
  final case object empty extends Tree[Nothing] {

    override val size: Int = 0
    override val width: Int = 0
    override val height: Int = 0
    override val isLeaf: Boolean = false
    override def valueOption: Option[Nothing] = None

    override def values(): List[Nothing] = Nil
    override def valuesUnsafe: List[Nothing] = Nil
    override def valueIterator(pred: Nothing => Boolean): Iterator[Nothing] = Iterator.empty
    override def valueStream: Stream[Nothing] = Stream.empty
    override def valueStream(pred: Nothing => Boolean): Stream[Nothing] = Stream.empty
    override def childrenValues: List[Nothing] = Nil

    override def childrenTrees: List[Tree[Nothing]] = Nil
    override def trees(): List[Tree[Nothing]] = List(empty)
    override def treesUnsafe: List[Tree[Nothing]] = List(empty)
    override def treeIterator(pred: Tree[Nothing] => Boolean): Iterator[Tree[Nothing]] = Iterator.empty
    override def treeStream: Stream[Nothing] = Stream.empty
    override def treeStream(pred: Tree[Nothing] => Boolean): Stream[Nothing] = Stream.empty

    override def branches(): List[List[Nothing]] = Nil
    override def branchesUnsafe: List[List[Nothing]] = Nil
    override def branchIterator(pred: Iterable[Nothing] => Boolean): Iterator[List[Nothing]] = Iterator.empty
    override def branchStream: Stream[List[Nothing]] = Stream.empty
    override def branchStream(pred: Iterable[Nothing] => Boolean): Stream[List[Nothing]] = Stream.empty
    override def countBranches(pred: Iterable[Nothing] => Boolean): Int = 0

    override def insert[T1](value: T1): Tree[T1] = Tree(value)
    override def insert[T1](subtree: Tree[T1]): Tree[T1] = subtree
    override def insert[T1](branch: List[T1]): Tree[T1] = branch match {
      case x :: xs => NodeTree.insert(Tree(x), xs)
      case _       => empty
    }

    override def selectValue[T1 >: Nothing](path: Iterable[T1]): Option[Nothing] = None
    override def selectTree[T1: ClassTag](path: Iterable[T1]): Option[Tree[Nothing]] =
      if (path.isEmpty) Some(empty) else None
    override def containsBranch[T1](branch: Iterable[T1]): Boolean = branch.isEmpty
    override def containsPath[T1 >: Nothing](path: Iterable[T1]): Boolean = path.isEmpty

    override def map[K: ClassTag](f: Nothing => K): Tree[K] = empty
    override def mapUnsafe[K: ClassTag](f: Nothing => K): Tree[K] = empty
    override def flatMap[K: ClassTag](f: Nothing => Tree[K]): Tree[K] = empty
    override def toPairsIterator: Iterator[(Int, Nothing)] = Iterator.empty
    override def toArrays[T1: ClassTag]: (Array[Int], Array[T1]) = (Array.empty[Int], Array.empty[T1])
    override def toSlices[T1 >: Nothing: ClassTag]: (IntSlice, Slice[T1]) = (IntSlice.empty, Slice.empty[T1])
    override def toStructureArray: Array[Int] = Array.empty[Int]

    override def mkStringUsingBranches(
      show: Nothing => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String,
      maxDepth: Int = Int.MaxValue
    ): String = ""

    override def inflate: Tree[Nothing] = Tree.empty
    override def deflate[T1](implicit tag: ClassTag[T1]): Tree[T1] = Tree.empty
  }

  /** There are multiple ways to flatten the tree after expanding a node.
    * As we don't want to be constrained by an arbitrary choice,
    * there is a possibility to create and/or use your own strategy when doing a flatMap.
    */
  trait FlatMapStrategy {

    /** When a value of a node expands into a new Node,
      * we need a way to deal with the existing subtrees. */
    def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T]

    /** When a value of a node expands into an Empty tree,
      * we need to decide either to keep or remove existing subtrees. */
    def keepOrphanedSubtrees: Boolean
  }

  final object FlatMapStrategy {

    /** Default strategy is to preserve all existing subtrees. */
    object JoinSubtrees extends FlatMapStrategy {

      /** Concatenates new and existing subtrees of an expanded node. */
      override def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T] =
        Tree(newNode.value, existingSubtrees ::: newNode.subtrees)

      /** Joins orphaned subtrees to the parent node. */
      override def keepOrphanedSubtrees: Boolean = true
    }

    /** A strategy to replace existing subtrees with the new ones. */
    object Replace extends FlatMapStrategy {

      /** Replaces old subtrees with the new ones. */
      override def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T] =
        newNode

      /** Removes orphaned subtrees completely. */
      override def keepOrphanedSubtrees: Boolean = false
    }

  }

  /** Useful methods to construct the tree. */
  final object Builder {

    /** Builds a tree from an iterator of pairs (numberOfChildren, value), where:
      *   - `value` is the value of a new node, and
      *   - `numberOfChildren` is a number of preceding elements in the list
      *                        to become direct subtrees of the node.
      * @note - Values of subtrees must always precede the value of a parent node, and appear in the reverse order.
      *       - The sum of all numberOfChildren values must be the size of the list minus one.
      */
    @`inline` def fromPairsIterator[T](iterator: Iterator[(Int, T)]): List[Tree[T]] = fromPairsIterator(iterator, Nil)

    /** Builds a tree from an iterable of pairs (numberOfChildren, value). */
    @`inline` def fromPairsIterable[T](iterable: Iterable[(Int, T)]): List[Tree[T]] =
      fromPairsIterator(iterable.iterator, Nil)

    @tailrec
    private def fromPairsIterator[T](iterator: Iterator[(Int, T)], result: List[Node[T]] = Nil): List[Tree[T]] =
      if (iterator.hasNext) {
        val (size, value) = iterator.next()
        fromPairsIterator(iterator, Tree(value, result.take(size)) :: result.drop(size))
      } else if (result.isEmpty) List(Tree.empty)
      else result

    /** Builds a tree from a pair of iterable collections:
      *   - `structure` is a collection representing serialized tree structure,
      *   - `values` is a collection of node's values.
      *
      * @note Both collections have to return data following rules set in [[Tree.toArrays]].
      */
    @`inline` def fromIterables[T](structure: Iterable[Int], values: Iterable[T]): List[Tree[T]] =
      fromIterators(structure.iterator, values.iterator)

    /** Builds a tree from a pair of iterators:
      *   - `structure` is an iterator over a serialized tree structure,
      *   - `values` is an iterator over node's values.
      *
      * @note Both iterators have to return data following rules set in [[Tree.toArrays]].
      */
    @`inline` def fromIterators[T](structure: Iterator[Int], values: Iterator[T]): List[Tree[T]] =
      fromIterators(structure, values, Nil)

    @tailrec
    private def fromIterators[T](structure: Iterator[Int], values: Iterator[T], result: List[Node[T]]): List[Tree[T]] =
      if (structure.hasNext && values.hasNext) {
        val value = values.next()
        val size = structure.next()
        fromIterators(structure, values, Tree(value.asInstanceOf[T], result.take(size)) :: result.drop(size))
      } else if (result.isEmpty) List(Tree.empty)
      else result

    /** Builds a tree from a pair of arrays:
      *   - `structure` is an arrays holding a serialized tree structure,
      *   - `values` is an arrays holding node's values.
      *
      * @note Both arrays have to return data following rules set in [[Tree.toArrays]].
      */
    @`inline` def fromArrays[T: ClassTag](structure: Array[Int], values: Array[T]): List[Tree[T]] = {
      assert(
        structure.length == values.length,
        "When constructing Tree from arrays, structure and values must be same size."
      )

      val tree =
        if (values.isEmpty) Tree.empty
        else {
          val width = structure.count(_ == 0)
          val height = ArrayTree.calculateHeight(structure.length - 1, structure)
          new ArrayTree[T](IntSlice.of(structure), Slice.of(values), width, height)
        }

      List(tree)
    }

    /** Shortcut for [[com.github.arturopala.tree.Tree.Builder.fromArrays]].
      * @return head element from the produced list or an empty tree */
    def fromArraysHead[T: ClassTag](structure: Array[Int], values: Array[T]): Tree[T] =
      fromArrays(structure, values).headOption.getOrElse(Tree.empty)

    /** Builds a tree from a list of pairs (numberOfChildren, node), where:
      *   - `node` is a new node, and
      *   - `numberOfChildren` is a number of preceding elements in the list
      *                      to become direct subtrees of the current node.
      *   - `strategy` defines how to merge nodes and what to do with orphaned subtrees.
      * @note - Nodes of subtrees must always precede the parent node, and appear in the reverse order.
      *       - The sum of all numberOfChildren values must be the size of the list minus one.
      */
    @tailrec
    def fromTreeList[T](
      list: List[(Int, Tree[T])],
      result: List[Node[T]] = Nil,
      offset: Int = 0,
      strategy: FlatMapStrategy = FlatMapStrategy.JoinSubtrees
    ): List[Tree[T]] =
      list match {
        case Nil => if (result.isEmpty) List(Tree.empty) else result
        case (size, tree) :: xs =>
          tree match {
            case `empty` =>
              val offset = if (strategy.keepOrphanedSubtrees) size else -1
              fromTreeList(xs, result.drop(size - offset), offset, strategy)
            case node: Node[T] =>
              val merged = strategy.merge(node, result.take(size))
              fromTreeList(xs, merged :: result.drop(size), 0, strategy)
          }
      }
  }

  /** Collection of common visualization templates. */
  final object Show {

    def showAsArrays[T <: Any](tree: Tree[T]): String =
      tree.mkStringUsingBranches(_.toString, ",", "\n", "[", "]")

    def showAsGraph[T <: Any](tree: Tree[T]): String =
      tree.mkStringUsingBranches(_.toString, " > ", "\n", "", "")

    def showAsPaths[T <: Any](tree: Tree[T]): String =
      tree.mkStringUsingBranches(_.toString, "/", "\n", "", "")

  }

  /** Checks equality of the two trees. */
  def equals[T](tree1: Tree[T], tree2: Tree[T]): Boolean =
    tree1.eq(tree2) || (tree1.size == tree2.size &&
      tree1.width == tree2.width &&
      tree1.height == tree2.height &&
      tree1.valueOption == tree2.valueOption && Compare.sameTrees(tree1, tree2))

  /** Computes hashcode of the tree. */
  def hashCodeOf[T](tree: Tree[T]): Int = {
    var hash = 17
    hash = hash * 31 + tree.valueOption.hashCode()
    hash = hash * 29 + tree.size.hashCode()
    hash = hash * 13 + tree.width.hashCode()
    hash = hash * 19 + tree.height.hashCode()
    hash
  }

}
