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
import com.github.arturopala.tree.internal.{Compare, _}

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
  def deflated: Tree[T]

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

  /** Creates new leaf tree.
    * @group Creation */
  final def apply[T](head: T): Tree[T] = new Leaf(head)

  /** Creates new tree having a single child.
    * @group Creation */
  final def apply[T](head: T, child: Tree[T]): Tree[T] = new Unary(head, child)

  /** Creates new tree having two children.
    * @group Creation */
  final def apply[T](head: T, left: Tree[T], right: Tree[T]): Tree[T] = new Binary(head, left, right)

  /** Creates new tree from the head value and list of children.
    * @group Creation */
  final def apply[T](head: T, children: Iterable[Tree[T]]): Tree[T] =
    if (children.isEmpty) new Leaf(head)
    else if (children.size == 1) new Unary(head, children.head)
    else if (children.size == 2) new Binary(head, children.head, children.drop(1).head)
    else new Bunch(head, children.toSeq)

  /** Creates new tree from the head value and initial children, and last child.
    * @group Creation */
  final def apply[T](head: T, children: Iterable[Tree[T]], child: Tree[T]): Tree[T] =
    if (children.isEmpty) new Unary(head, child)
    else if (children.size == 1) new Binary(head, children.head, child)
    else new Bunch(head, children.toSeq :+ child)

  /** Creates new tree from the head value and first child, and following children.
    * @group Creation */
  final def apply[T](head: T, child: Tree[T], children: Iterable[Tree[T]]): Tree[T] =
    if (children.isEmpty) new Unary(head, child)
    else if (children.size == 1) new Binary(head, child, children.head)
    else new Bunch(head, child +: children.toSeq)

  /** Creates new tree from the head and multiple children.
    * @group Creation */
  final def apply[T](
    head: T,
    child1: Tree[T],
    child2: Tree[T],
    child3: Tree[T],
    otherChildren: Tree[T]*
  ): Tree[T] =
    new Bunch(head, child1 +: child2 +: child3 +: otherChildren.toIndexedSeq)

  /** Creates a tree node from the head value and a tuple of (left children, right children).
    * @group Creation */
  final def apply[T](head: T, children: (Iterable[Tree[T]], Iterable[Tree[T]])): Tree[T] =
    children match {
      case (s1, s2) if s1.isEmpty                   => Tree(head, s2)
      case (s1, s2) if s2.isEmpty                   => Tree(head, s1)
      case (s1, s2) if s1.size == 1 && s2.size == 1 => Tree.Binary(head, s1.head, s2.head)
      case (s1, s2)                                 => Tree(head, s1 ++ s2)
    }

  /** Extracts non-empty tree as a tuple of (head, children). */
  final def unapply[T](node: Tree[T]): Option[(T, Iterable[Tree[T]])] = node match {
    case Tree.empty => None
    case _          => Some((node.head, node.children))
  }

  /**
    * An empty Tree singleton.
    */
  final case object empty extends Tree[Nothing] with EmptyTreeLike {
    override val toString: String = "Tree.empty"
    override val inflated: Tree[T] = this
    override val deflated: Tree[T] = this
  }

  /**
    * A Tree represented internally by linked objects,
    * each node consists of a head value and children collection.
    *
    * Concrete, specialized node types are [[Leaf]], [[Unary]], [[Binary]], and [[Bunch]].
    */
  sealed trait NodeTree[+T] extends Tree[T] with NodeTreeLike[T] {

    final override protected val node: NodeTree[T] = this
    final override val inflated: Tree[T] = this

    final override def deflated: Tree[T] = {
      val (structure, values) = node.toBuffers
      new ArrayTree[T](structure.asSlice, values.asSlice, node.width, node.height)
    }
  }

  /** Concrete node of the Tree, consisting of a value and no subtrees. */
  final class Leaf[+T] private[Tree] (val head: T) extends NodeTree[T] {

    override def size: Int = 1
    override def width: Int = 1
    override def height: Int = 1
    override def isLeaf: Boolean = true
    override def children: Iterable[Tree[T]] = Iterable.empty[Tree[T]]
    override def childrenCount: Int = 0
    override def firstChildValue: Option[T] = None
    override def lastChildValue: Option[T] = None
    override def firstChild: Option[Tree[T]] = None
    override def lastChild: Option[Tree[T]] = None
  }

  object Leaf {
    def apply[T](head: T): Tree[T] = new Leaf(head)
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
    override def firstChildValue: Option[T] = Some(child.head)
    override def lastChildValue: Option[T] = Some(child.head)
    override def firstChild: Option[Tree[T]] = Some(child)
    override def lastChild: Option[Tree[T]] = Some(child)
  }

  object Unary {
    def apply[T](head: T, child: Tree[T]): Tree[T] = new Unary(head, child)
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
    override def firstChildValue: Option[T] = Some(left.head)
    override def lastChildValue: Option[T] = Some(right.head)
    override def firstChild: Option[Tree[T]] = Some(left)
    override def lastChild: Option[Tree[T]] = Some(right)
  }

  object Binary {
    def apply[T](head: T, left: Tree[T], right: Tree[T]): Tree[T] = new Binary(head, left, right)
    def unapply[T](node: Binary[T]): Option[(T, Tree[T], Tree[T])] = Some((node.head, node.left, node.right))
  }

  /** Concrete node of the Tree, consisting of a value and a list of subtrees (more than two). */
  final class Bunch[+T] private[Tree] (val head: T, val children: Seq[Tree[T]]) extends NodeTree[T] {

    override val size: Int = 1 + children.map(_.size).sum
    override val width: Int = Math.max(1, children.map(_.width).sum)
    override val height: Int = 1 + children.maxBy(_.height).height
    override def isLeaf: Boolean = children.isEmpty
    override def childrenCount: Int = children.size
    override def firstChildValue: Option[T] = Some(children.head.head)
    override def lastChildValue: Option[T] = Some(children.last.head)
    override def firstChild: Option[Tree[T]] = Some(children.head)
    override def lastChild: Option[Tree[T]] = Some(children.last)
  }

  object Bunch {
    def apply[T](head: T, children: Seq[Tree[T]]): Tree[T] = new Bunch(head, children)
    def unapply[T](node: Bunch[T]): Option[(T, Seq[Tree[T]])] = Some((node.head, node.children))
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
  ) extends ArrayTreeLike[T] with Tree[T] {

    override protected val tree: ArrayTree[T] = this

    assert(structure.nonEmpty, "When creating an ArrayTree, structure slice must not be empty.")
    assert(content.nonEmpty, "When creating an ArrayTree, content slice must not be empty.")
    assert(
      content.length == structure.length || content.length == Int.MaxValue,
      "When creating an ArrayTree, structure and content slices must be of the same size."
    )

    override val size: Int = structure.length
    override lazy val width: Int = delayedWidth
    override lazy val height: Int = delayedHeight
    override def isLeaf: Boolean = size == 1
    override def isEmpty: Boolean = size == 0
    override def childrenCount: Int = structure.last

    override def inflated: Tree[T] =
      TreeBuilder.fromIterators(structure.iterator, content.iterator).headOption.getOrElse(Tree.empty)

    override val deflated: Tree[T] = this
  }

  /** Arbitrary number for inflate-deflate heuristics. */
  @`inline` final val DEFLATE_SIZE_THRESHOLD: Int = 1000

  @`inline` final def preferInflated[T, T1 >: T](node: Tree.NodeTree[T], tree: Tree.ArrayTree[T1]): Boolean =
    tree.size < Tree.DEFLATE_SIZE_THRESHOLD || tree.size <= node.size

  /** Transformer instance for the Tree. */
  implicit object TreeTransformer extends Transformer[Tree] {

    final override def toSlices[T](target: Tree[T]): (IntSlice, Slice[T]) =
      target.toSlices

    /** Creates a tree from a pair of slices. */
    override def fromSlices[T](structure: IntSlice, values: Slice[T]): Tree[T] =
      if (structure.length == 0) Tree.empty
      else
        new ArrayTree[T](
          structure,
          values,
          ArrayTreeFunctions.calculateWidth(structure),
          ArrayTreeFunctions.calculateHeight(structure)
        )

    /** Outputs tree linearisation as a pair of buffers. */
    override def toBuffers[T, T1 >: T](target: Tree[T]): (IntBuffer, Buffer[T1]) =
      target.toBuffers

    /** Creates a tree from a pair of buffers. */
    override def fromBuffers[T](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Tree[T] =
      fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)

    /** Returns an empty instance. */
    override def empty[T]: Tree[T] = Tree.empty
  }

}
