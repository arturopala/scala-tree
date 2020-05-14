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

import com.github.arturopala.bufferandslice.{IntSlice, Slice}
import com.github.arturopala.tree.util._

import scala.reflect.ClassTag

/**
  * A general-purpose, covariant, immutable, low overhead,
  * efficient, monadic tree-like data structure with comprehensive API.
  *
  * Conceptually, apart from an empty, each node of the tree has:
  *   - a value, and
  *   - a collection of subtrees.
  *
  * By the design choice, every node can possibly have duplicated children values,
  * although most modifications methods comes in two versions:
  * the default distinct (strict) and a lax variant.
  * Why to allow lax methods at all? Well, if the data is distinct by itself,
  * one doesn't have to pay a price of additional checks.
  *
  * Internally, there are three main implementations of the Tree:
  *   - [[Tree.empty]], an empty tree singleton,
  *   - [[Tree.NodeTree]], a deeply-nested hierarchy of immutable nodes (inflated tree),
  *   - [[Tree.ArrayTree]], encoded as a twin linear arrays of structure and values (deflated tree).
  *
  * The reason for having an inflated and deflated variants of the tree
  * is such that each one exhibits different performance and memory
  * consumption characteristics, making it possible to experiment and optimize
  * for individual targets while facing the same API.
  */
sealed trait Tree[+T] extends TreeLike[T] {

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

  override def toString: String = {
    def stringify(o: Any): String = if (o.isInstanceOf[String]) s""""$o"""" else o.toString

    if (size < 50)
      s"Tree(${stringify(valueOption.get)}${if (size > 1) s", ${children.map(_.toString).mkString(",")}" else ""})"
    else s"Tree(size=$size, width=$width, height=$height, hashCode=${hashCode()})"
  }
}

/**
  * Tree companion object.
  * Hosts factory methods and concrete tree implementations.
  */
object Tree {

  /** Creates an empty Tree, same as [[Tree.empty]].
    * @group Creation */
  final def apply[T](): Tree[T] = empty

  /** Creates a leaf tree.
    * @group Creation */
  final def apply[T](value: T): NodeTree[T] = new Leaf(value)

  /** Creates a tree having a single subtree.
    * @group Creation */
  final def apply[T](value: T, subtree: NodeTree[T]): NodeTree[T] = new Unary(value, subtree)

  /** Creates a tree having two subtrees.
    * @group Creation */
  final def apply[T](value: T, left: NodeTree[T], right: NodeTree[T]): NodeTree[T] = new Binary(value, left, right)

  /** Creates a tree node from the value and multiple subtrees.
    * @group Creation */
  final def apply[T](
    value: T,
    subtree1: NodeTree[T],
    subtree2: NodeTree[T],
    subtree3: NodeTree[T],
    others: NodeTree[T]*
  ): NodeTree[T] =
    new Bunch(value, subtree1 :: subtree2 :: subtree3 :: others.toList)

  /** Creates a tree node from the value and list of subtrees.
    * @group Creation */
  final def apply[T](value: T, subtrees: List[NodeTree[T]]): NodeTree[T] = subtrees match {
    case Nil           => new Leaf(value)
    case x :: Nil      => new Unary(value, x)
    case x :: y :: Nil => new Binary(value, x, y)
    case _             => new Bunch(value, subtrees)
  }

  /** Deflates the tree, if inflated, otherwise returns as is.
    * @group Utilities */
  final def deflate[T: ClassTag](tree: Tree[T]): Tree[T] = tree match {
    case `empty`                 => Tree.empty
    case arrayTree: ArrayTree[T] => arrayTree
    case _ =>
      val (structure, values) = tree.toArrays
      new ArrayTree[T](IntSlice.of(structure), Slice.of(values), tree.width, tree.height)
  }

  /** Inflates the tree, if deflated, otherwise returns as is.
    * @group Utilities */
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
  sealed trait NodeTree[+T] extends Tree[T] with NodeTreeLike[T] {

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
  final class ArrayTree[T: ClassTag] private[tree] (
    val structure: IntSlice,
    val content: Slice[T],
    delayedWidth:  => Int,
    delayedHeight: => Int
  ) extends ArrayTreeLike[T] with Tree[T] {

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
  final case object empty extends Tree[Nothing] with EmptyTreeLike {

    override protected lazy val hashcode: Int = 0
    override val toString: String = "Tree.empty"
  }

  /** Checks equality of the two trees.
    * @group Utilities */
  final def equals[T](tree1: Tree[T], tree2: Tree[T]): Boolean =
    tree1.eq(tree2) || (tree1.size == tree2.size &&
      tree1.width == tree2.width &&
      tree1.height == tree2.height &&
      tree1.valueOption == tree2.valueOption && Compare.sameTrees(tree1, tree2))

  /** Computes hashcode of the tree.
    * @group Utilities */
  final def hashCodeOf[T](tree: Tree[T]): Int = {
    var hash = 17
    hash = hash * 31 + tree.valueOption.hashCode()
    hash = hash * 29 + tree.size.hashCode()
    hash = hash * 13 + tree.width.hashCode()
    hash = hash * 19 + tree.height.hashCode()
    hash
  }

}
