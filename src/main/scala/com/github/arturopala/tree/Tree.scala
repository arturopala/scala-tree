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
import com.github.arturopala.tree.internal.{Compare, _}

import scala.reflect.ClassTag

/**
  * A general-purpose, covariant, immutable, low overhead,
  * efficient, monadic tree-like data structure with comprehensive API.
  *
  * Conceptually, apart from an empty, each node of the tree has:
  *
  *   - a head value,
  *   - a collection of children (sub-trees).
  *
  * By the design choice, every node may have duplicated children values,
  * although default set of modifications methods assumes and preserves its uniqueness.
  *
  * If the values in the tree are distinct by itself, or you don't care about uniqueness,
  * there is a supplementary set of lax extensions methods in [[LaxTreeOps]].
  *
  * Internally, there are three main implementations of the Tree:
  *   - [[Tree.empty]], an empty tree singleton, i.e. `Tree[Nothing]`,
  *   - [[Tree.NodeTree]], a classic, deeply-nested hierarchy of immutable nodes (inflated tree),
  *   - [[Tree.ArrayTree]], the tree encoded in an ultra-compact flat format of twin linear arrays of structure and values (deflated tree).
  *
  * The reason for having an inflated and deflated variants of the tree
  * is such that each one exhibits different performance and memory
  * consumption characteristics, making it possible to experiment and optimize
  * for individual targets while facing the same API.
  */
sealed trait Tree[+T] extends TreeLike[T] {

  // ---------------------------------------
  // Common Tree API is specified in TreeLike.
  // ---------------------------------------

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
    case otherTree: Tree[_] => Compare.sameTrees(this, otherTree)
    case _                  => false
  }

  final override def hashCode(): Int = hashcode

  // Tree is immutable so we shall calculate hashcode only once
  protected final lazy val hashcode: Int = {
    var hash = 17
    hash = hash * 31 + headOption.hashCode()
    hash = hash * 29 + size.hashCode()
    hash = hash * 13 + width.hashCode()
    hash = hash * 19 + height.hashCode()
    hash
  }

  override def toString: String = {
    def stringify(o: Any): String = if (o.isInstanceOf[String]) s""""$o"""" else o.toString

    if (size < 50)
      s"Tree(${stringify(headOption.get)}${if (size > 1) s", ${children.map(_.toString).mkString(",")}" else ""})"
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
  final def apply[T](head: T): Tree[T] = new Leaf(head)

  /** Creates a tree having a single child.
    * @group Creation */
  final def apply[T](head: T, child: Tree[T]): Tree[T] = new Unary(head, child)

  /** Creates a tree having two children.
    * @group Creation */
  final def apply[T](head: T, left: Tree[T], right: Tree[T]): Tree[T] = new Binary(head, left, right)

  /** Creates a tree node from the head and multiple children.
    * @group Creation */
  final def apply[T](
    head: T,
    child1: Tree[T],
    child2: Tree[T],
    child3: Tree[T],
    otherChildren: Tree[T]*
  ): Tree[T] =
    new Bunch(head, child1 +: child2 +: child3 +: otherChildren.toIndexedSeq)

  /** Creates a tree node from the value and list of subtrees.
    * @group Creation */
  final def apply[T](head: T, children: Iterable[Tree[T]]): Tree[T] =
    if (children.isEmpty) new Leaf(head)
    else if (children.size == 1) new Unary(head, children.head)
    else if (children.size == 2) new Binary(head, children.head, children.drop(1).head)
    else new Bunch(head, children)

  object Node {

    /** Universal Tree extractor of a tuple of (head, children). */
    def unapply[T](node: Tree[T]): Option[(T, Iterable[Tree[T]])] =
      Some((node.head, node.children))
  }

  /**
    * An empty Tree singleton.
    */
  final case object empty extends Tree[Nothing] with EmptyTreeLike {
    override val toString: String = "Tree.empty"
  }

  /**
    * A Tree represented internally by linked objects,
    * each node consists of a head value and children collection.
    *
    * Concrete, specialized node types are [[Leaf]], [[Unary]], [[Binary]], and [[Bunch]].
    */
  sealed trait NodeTree[+T] extends Tree[T] with NodeTreeLike[T] {
    final override protected val node: NodeTree[T] = this
  }

  /** Concrete node of the Tree, consisting of a value and no subtrees. */
  final class Leaf[+T] private[Tree] (val head: T) extends NodeTree[T] {

    override def size: Int = 1
    override def width: Int = 1
    override def height: Int = 1
    override def isLeaf: Boolean = true
    override def children: Iterable[Tree[T]] = Iterable.empty[Tree[T]]
    override def childrenCount: Int = 0
  }

  object Leaf {
    def unapply[T](node: Leaf[T]): Option[T] = Some(node.head)
  }

  /** Concrete node of the Tree, consisting of a value and a single subtree. */
  final class Unary[+T] private[Tree] (val head: T, val child: Tree[T]) extends NodeTree[T] {

    override val size: Int = 1 + child.size
    override val width: Int = Math.max(1, child.width)
    override val height: Int = 1 + child.height
    override def isLeaf: Boolean = false
    override def children: Iterable[Tree[T]] = List(child)
    override def childrenCount: Int = 1
  }

  object Unary {
    def unapply[T](node: Unary[T]): Option[(T, Tree[T])] = Some((node.head, node.child))
  }

  /** Concrete node of the Tree, consisting of a value and two subtrees. */
  final class Binary[+T] private[Tree] (val head: T, val left: Tree[T], val right: Tree[T]) extends NodeTree[T] {

    override val size: Int = 1 + left.size + right.size
    override val width: Int = Math.max(1, left.width + right.width)
    override val height: Int = 1 + Math.max(left.height, right.height)
    override def isLeaf: Boolean = false
    override def children: Iterable[Tree[T]] = List(left, right)
    override def childrenCount: Int = 2
  }

  object Binary {
    def unapply[T](node: Binary[T]): Option[(T, Tree[T], Tree[T])] = Some((node.head, node.left, node.right))
  }

  /** Concrete node of the Tree, consisting of a value and a list of subtrees (more than two). */
  final class Bunch[+T] private[Tree] (val head: T, val children: Iterable[Tree[T]]) extends NodeTree[T] {

    override val size: Int = 1 + children.map(_.size).sum
    override val width: Int = Math.max(1, children.map(_.width).sum)
    override val height: Int = 1 + children.maxBy(_.height).height
    override def isLeaf: Boolean = children.isEmpty
    override def childrenCount: Int = children.size
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

    /** Top index of the buffers. */
    def top: Int = tree.structure.top

    override val size: Int = tree.structure.length
    override lazy val width: Int = delayedWidth
    override lazy val height: Int = delayedHeight
    override def isLeaf: Boolean = size == 1
    override def isEmpty: Boolean = size == 0
    override def childrenCount: Int = tree.structure.last
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

  /** Arbitrary number for inflate-deflate heuristics. */
  @`inline` final val DEFLATE_SIZE_THRESHOLD: Int = 1000

  @`inline` final def preferInflated[T, T1 >: T](node: Tree.NodeTree[T], tree: Tree.ArrayTree[T1]): Boolean =
    tree.size < Tree.DEFLATE_SIZE_THRESHOLD || tree.size <= node.size

}
