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

package com.github.arturopala.tree.internal

import java.util.NoSuchElementException

import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.Tree.{ArrayTree, Binary, Leaf, Node, NodeTree, Unary}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.internal.VectorOps._
import com.github.arturopala.tree.internal.IteratorOps._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/** Collection of operations on the hierarchical, node-based, representation of the tree. */
object NodeTree {

  /** Type alias of a tree split, i.e. a tuple of (leftChildren, head, rightChildren). */
  type TreeSplit[T] = (Seq[NodeTree[T]], T, Seq[NodeTree[T]])

  object NonEmptyNode {

    /** Extracts head, first child and remaining children if non empty. */
    def unapply[T](node: NodeTree[T]): Option[(T, NodeTree[T], Seq[NodeTree[T]])] = node match {
      case _: Leaf[T]      => None
      case node: Unary[T]  => Some((node.head, node.child, Nil))
      case node: Binary[T] => Some((node.head, node.left, node.right :: Nil))
      case _               => Some((node.head, node.children.head, node.children.tail))
    }
  }

  object Node2 {

    /** Universal NodeTree extractor as a tuple of (head, childrenIterator). */
    def unapply[T](node: NodeTree[T]): Option[(T, Iterator[Tree[T]])] =
      Some((node.head, node.children.iterator))
  }

  final def leavesIterator[T](node: NodeTree[T]): Iterator[T] = new Iterator[T] {

    type Queue = Iterator[Tree[T]]
    private var queue: Queue = Iterator(node)

    var hasNext: Boolean = false
    private var nextItem: T = _

    seekNext()

    override final def next(): T =
      if (hasNext) {
        val result = nextItem
        seekNext()
        result
      } else throw new NoSuchElementException

    @tailrec
    private final def seekNext(): Unit = {
      hasNext = false
      if (queue.hasNext) {
        queue.next match {
          case Leaf(head) =>
            nextItem = head
            hasNext = true
            queue = queue.trim

          case Node2(_, children: Iterator[Tree[T]]) =>
            queue = children ++: queue.trim
            seekNext()
        }
      }
    }
  }

  /** Returns an iterator over all the nodes of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesIterator[T](node: NodeTree[T], depthFirst: Boolean): Iterator[T] = new Iterator[T] {

    type Queue = Iterator[Tree[T]]
    private var queue: Queue = Iterator(node)

    override final def hasNext: Boolean = queue.nonEmpty

    override final def next(): T =
      if (queue.isEmpty) throw new NoSuchElementException()
      else {
        val Node2(head: T, children: Iterator[Tree[T]]) = queue.next
        queue =
          if (depthFirst) children ++: queue.trim
          else queue.trim :++ children
        head
      }
  }

  /** Returns an iterator over the filtered nodes of the tree, goes depth-first with maxDepth limit.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesIteratorWithFilter[T](pred: T => Boolean, node: NodeTree[T], depthFirst: Boolean): Iterator[T] =
    new FilterIterator(valuesIterator(node, depthFirst), pred)

  /** Returns an iterator over all the values and levels of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesAndLevelsIterator[T](
    node: NodeTree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, T, Boolean)] =
    new Iterator[(Int, T, Boolean)] {

      type Queue = Iterator[(Int, Tree[T])]
      private var queue: Queue =
        if (maxDepth > 0) Iterator((1, node))
        else Iterator.empty

      override final def hasNext: Boolean = queue.nonEmpty

      override final def next(): (Int, T, Boolean) =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (level, node @ Node2(head: T, children: Iterator[Tree[T]])) = queue.next
          if (level < maxDepth)
            queue =
              if (depthFirst) children.map((level + 1, _)) ++: queue.trim
              else queue.trim :++ children.map((level + 1, _))
          else
            queue = queue.trim

          (level, head, node.isLeaf)
        }
    }

  /** Returns an iterator over the filtered values and levels of the tree with maxDepth limit.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesAndLevelsIteratorWithFilter[T](
    pred: T => Boolean,
    node: NodeTree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, T, Boolean)] =
    new FilterIterator(valuesAndLevelsIterator(node, maxDepth, depthFirst), (t: (Int, T, Boolean)) => pred(t._2))

  /** Returns an iterator over the filtered nodes of the tree with maxDepth limit.
    * @note uses Iterator internally */
  final def valuesIteratorWithLimit[T](
    pred: T => Boolean,
    node: NodeTree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[T] =
    new Iterator[T] {

      type Queue = Iterator[(Int, Tree[T])]
      private var queue: Queue =
        if (maxDepth > 0) Iterator((1, node))
        else Iterator.empty

      var hasNext: Boolean = false
      private var nextItem: T = _

      seekNext()

      override final def next(): T =
        if (hasNext) {
          val result = nextItem
          seekNext()
          result
        } else throw new NoSuchElementException

      @tailrec
      private final def seekNext(): Unit = {
        hasNext = false
        if (queue.hasNext) {
          val (level, Node2(head: T, children: Iterator[Tree[T]])) = queue.next
          if (level < maxDepth) {
            queue =
              if (depthFirst) children.map((level + 1, _)) ++: queue.trim
              else queue.trim :++ children.map((level + 1, _))
          } else {
            queue = queue.trim
          }
          if (pred(head)) {
            nextItem = head
            hasNext = true
          } else {
            seekNext()
          }
        }
      }
    }

  /** Sequence of all values of the tree fulfilling the predicate. */
  final def values[T](pred: T => Boolean, node: NodeTree[T]): Vector[T] = values(pred, Vector.empty[T], Vector(node))

  @tailrec
  private def values[T](pred: T => Boolean, result: Vector[T], queue: Vector[NodeTree[T]]): Vector[T] =
    if (queue.isEmpty) result
    else {
      val Node(head, children) = queue.head
      if (pred(head)) values(pred, result :+ head, children ++: queue.safeTail)
      else values(pred, result, children ++: queue.safeTail)
    }

  /** Returns an iterator over all (sub)trees of the tree inclusive. */
  final def treesIterator[T](node: NodeTree[T], depthFirst: Boolean): Iterator[Tree[T]] = new Iterator[Tree[T]] {

    type Queue = Iterator[Tree[T]]
    private var queue: Queue = Iterator(node)

    override final def hasNext: Boolean = queue.nonEmpty

    override final def next(): Tree[T] =
      if (queue.isEmpty) throw new NoSuchElementException()
      else {
        val node = queue.next
        queue =
          if (depthFirst) node.children.iterator ++: queue.trim
          else queue.trim :++ node.children.iterator

        node
      }
  }

  /** Returns an iterator over filtered trees of the tree. */
  final def treesIteratorWithFilter[T](
    pred: Tree[T] => Boolean,
    node: NodeTree[T],
    depthFirst: Boolean
  ): Iterator[Tree[T]] =
    new FilterIterator(treesIterator(node, depthFirst), pred)

  /** Returns an iterator over pairs of (level, tree) of the tree inclusive. */
  final def treesAndLevelsIterator[T](node: NodeTree[T], maxDepth: Int, depthFirst: Boolean): Iterator[(Int, Tree[T])] =
    new Iterator[(Int, Tree[T])] {

      type Queue = Iterator[(Int, Tree[T])]
      private var queue: Queue =
        if (maxDepth > 0) Iterator((1, node))
        else Iterator.empty

      override final def hasNext: Boolean = queue.nonEmpty

      override final def next(): (Int, Tree[T]) =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val pair = queue.next
          if (pair._1 < maxDepth)
            queue =
              if (depthFirst) pair._2.children.iterator.map((pair._1 + 1, _)) ++: queue.trim
              else queue.trim :++ pair._2.children.iterator.map((pair._1 + 1, _))
          else
            queue = queue.trim

          pair
        }
    }

  /** Returns an iterator over filtered pairs of (level, tree) of the tree. */
  final def treesAndLevelsIteratorWithFilter[T](
    pred: Tree[T] => Boolean,
    node: NodeTree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, Tree[T])] =
    new FilterIterator(treesAndLevelsIterator(node, maxDepth, depthFirst), (t: (Int, Tree[T])) => pred(t._2))

  /** Returns an iterator over filtered (sub)trees of the tree with depth limit.
    * @note uses Iterator internally */
  final def treesIteratorWithLimit[T](
    pred: Tree[T] => Boolean,
    node: NodeTree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[Tree[T]] =
    new Iterator[Tree[T]] {

      type Queue = Iterator[(Int, Tree[T])]
      private var queue: Queue = if (maxDepth > 0) Iterator((1, node)) else Iterator.empty

      var hasNext: Boolean = false
      private var nextItem: Tree[T] = _

      seekNext()

      override final def next(): Tree[T] =
        if (hasNext) {
          val result = nextItem
          seekNext()
          result
        } else throw new NoSuchElementException

      @tailrec
      private final def seekNext(): Unit = {
        hasNext = false
        if (queue.hasNext) {
          val (level, node) = queue.next
          if (level < maxDepth) {
            queue =
              if (depthFirst) node.children.iterator.map((level + 1, _)) ++: queue.trim
              else queue.trim ++ node.children.iterator.map((level + 1, _))
          } else {
            queue = queue.trim
          }
          if (pred(node)) {
            nextItem = node
            hasNext = true
          } else {
            seekNext()
          }
        }
      }
    }

  /** Lists all the trees fulfilling the predicate. */
  final def trees[T](pred: Tree[T] => Boolean, node: NodeTree[T]): Vector[NodeTree[T]] =
    trees(pred, Vector.empty, Vector(node))

  @tailrec
  private def trees[T](
    pred: Tree[T] => Boolean,
    result: Vector[NodeTree[T]],
    queue: Vector[NodeTree[T]]
  ): Vector[NodeTree[T]] =
    if (queue.isEmpty) result
    else {
      val node @ Node(_, children) = queue.head
      if (pred(node)) trees(pred, result :+ node, children ++: queue.safeTail)
      else trees(pred, result, children ++: queue.safeTail)
    }

  /** Returns an iterator over all paths of the tree. */
  final def pathsIterator[T](node: NodeTree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Iterator[(Vector[T], Tree[T])]
      private var queue: Queue = Iterator((Vector.empty, node))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Node2(head: T, children: Iterator[Tree[T]])) = queue.next
          val branch = acc :+ head
          queue = children.map((branch, _)) ++: queue.trim
          branch
        }
    }

  /** Returns an iterator over all branches of the tree. */
  final def branchesIterator[T](node: NodeTree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], NodeTree[T])]
      private var queue: Queue = seekNext(Vector((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Node(head, children)) = queue.head
          val branch = acc :+ head
          queue = seekNext(children.map((branch, _)) ++: queue.safeTail)
          branch
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Node(head, children)) = q.head
          val branch = acc :+ head
          children match {
            case Nil => q
            case _   => seekNext(children.map((branch, _)) ++: q.safeTail)
          }
        }
    }

  /** Returns an iterator over filtered branches of the tree. */
  final def branchesIteratorWithFilter[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], NodeTree[T])]
      private var queue: Queue = seekNext(Vector((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Node(head, children)) = queue.head
          val branch = acc :+ head
          queue = seekNext(children.map((branch, _)) ++: queue.safeTail)
          branch
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Node(head, children)) = q.head
          val branch = acc :+ head
          children match {
            case Nil if pred(branch) => q
            case _                   => seekNext(children.map((branch, _)) ++: q.safeTail)
          }
        }
    }

  /** Returns an iterator over filtered branches of the tree with depth limit. */
  final def branchesIteratorWithLimit[T](
    pred: Iterable[T] => Boolean,
    node: NodeTree[T],
    maxDepth: Int
  ): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], NodeTree[T])]
      private var queue: Queue =
        if (maxDepth > 0) seekNext(Vector((Vector.empty, node)))
        else Vector.empty

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Node(head, children)) = queue.head
          val branch = acc :+ head
          queue =
            if (branch.length < maxDepth) seekNext(children.map((branch, _)) ++: queue.safeTail)
            else seekNext(queue.safeTail)
          branch
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Node(head, children)) = q.head
          val branch = acc :+ head
          children match {
            case s if (s.isEmpty || branch.length >= maxDepth) && pred(branch) => q
            case _ =>
              if (branch.length < maxDepth) seekNext(children.map((branch, _)) ++: q.safeTail)
              else seekNext(q.safeTail)
          }
        }
    }

  final def branches[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Iterable[Iterable[T]] =
    branches(pred, Vector.empty, Vector((Vector.empty, node)))

  @tailrec
  private def branches[T](
    pred: Iterable[T] => Boolean,
    result: Vector[Vector[T]],
    queue: Vector[(Vector[T], NodeTree[T])]
  ): Vector[Vector[T]] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case (acc, Node(head, children)) =>
          val branch = acc :+ head
          children match {
            case Nil if pred(branch) => branches(pred, result :+ branch, queue.safeTail)
            case _                   => branches(pred, result, children.map((branch, _)) ++: queue.safeTail)
          }
      }

  final def countBranches[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Int =
    countBranches(pred, 0, Iterator((Vector.empty, node)))

  @tailrec
  private def countBranches[T](
    pred: Iterable[T] => Boolean,
    result: Int,
    queue: Iterator[(Vector[T], Tree[T])]
  ): Int =
    if (queue.isEmpty) result
    else {
      val (acc, Node2(head: T, children: Iterator[Tree[T]])) = queue.next
      val branch = acc :+ head
      children match {
        case i if i.isEmpty && pred(branch) => countBranches(pred, 1 + result, queue.trim)
        case _                              => countBranches(pred, result, children.map((branch, _)) ++: queue.trim)
      }
    }

  @tailrec
  final def select[T, T1 >: T, R](
    node: NodeTree[T],
    path: Iterable[T1],
    result: NodeTree[T] => R
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != node.head)) None
    else if (path.tail.isEmpty) {
      if (path.head == node.head) Some(result(node)) else None
    } else {
      val nextOpt = node.children.collect {
        case nextNode if path.tail.head == nextNode.head => nextNode
      }.lastOption
      if (nextOpt.isEmpty) None
      else select(nextOpt.get, path.tail, result)
    }

  @tailrec
  final def select[T, K, R](
    node: NodeTree[T],
    path: Iterable[K],
    toResult: NodeTree[T] => R,
    toPathItem: T => K
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != toPathItem(node.head))) None
    else if (path.tail.isEmpty) {
      if (path.head == toPathItem(node.head)) Some(toResult(node)) else None
    } else {
      val nextOpt = node.children.collect {
        case nextNode if path.tail.head == toPathItem(nextNode.head) => nextNode
      }.lastOption
      if (nextOpt.isEmpty) None
      else select(nextOpt.get, path.tail, toResult, toPathItem)
    }

  @`inline` final def containsBranch[T, T1 >: T](node: NodeTree[T], branch: Iterable[T1]): Boolean =
    contains(node, branch, requiresFullMatch = true)

  @`inline` final def containsBranch[T, K](node: NodeTree[T], branch: Iterable[K], toPathItem: T => K): Boolean =
    contains(node, branch, requiresFullMatch = true, toPathItem)

  @`inline` final def containsPath[T, T1 >: T](node: NodeTree[T], path: Iterable[T1]): Boolean =
    contains(node, path, requiresFullMatch = false)

  @`inline` final def containsPath[T, K](node: NodeTree[T], path: Iterable[K], toPathItem: T => K): Boolean =
    contains(node, path, requiresFullMatch = false, toPathItem)

  @tailrec
  final def contains[T, T1 >: T](node: NodeTree[T], path: Iterable[T1], requiresFullMatch: Boolean): Boolean =
    if (path.isEmpty || (path.nonEmpty && path.head != node.head)) false
    else if (path.tail.isEmpty) (!requiresFullMatch || node.isLeaf) && path.head == node.head
    else {
      val nextOpt = node.children.collect {
        case nextNode if nextNode.head == path.tail.head => nextNode
      }.lastOption
      if (nextOpt.isEmpty) false
      else contains(nextOpt.get, path.tail, requiresFullMatch)
    }

  @tailrec
  final def contains[T, K](
    node: NodeTree[T],
    path: Iterable[K],
    requiresFullMatch: Boolean,
    toPathItem: T => K
  ): Boolean =
    if (path.isEmpty || (path.nonEmpty && path.head != toPathItem(node.head))) false
    else if (path.tail.isEmpty) (!requiresFullMatch || node.isLeaf) && path.head == toPathItem(node.head)
    else {
      val nextOpt = node.children.collect {
        case nextNode if toPathItem(nextNode.head) == path.tail.head => nextNode
      }.lastOption
      if (nextOpt.isEmpty) false
      else contains(nextOpt.get, path.tail, requiresFullMatch, toPathItem)
    }

  final def insertBranchUnsafe[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): NodeTree[T1] =
    if (branchIterator.hasNext) {
      Tree(tree.head, insertBranchInSubtrees(branchIterator.next, Nil, tree.children, branchIterator))
    } else tree

  @tailrec
  private def insertBranchInSubtrees[T, T1 >: T: ClassTag](
    branchHead: T1,
    subtreesLeft: Seq[NodeTree[T]],
    subtreesRight: Seq[NodeTree[T]],
    branchTailIterator: Iterator[T1]
  ): Seq[NodeTree[T1]] =
    subtreesRight match {
      case Nil =>
        val branchTree: NodeTree[T1] =
          TreeBuilder.linearTreeFromSequence(branchHead +: branchTailIterator.toSeq).asInstanceOf[NodeTree[T1]]
        branchTree +: subtreesLeft

      case head :: tail if head.head == branchHead =>
        val modified = insertBranchUnsafe(head, branchTailIterator)
        subtreesLeft.reverse ++: (modified :: tail)

      case head :: tail =>
        insertBranchInSubtrees(branchHead, head +: subtreesLeft, tail, branchTailIterator)
    }

  final def insertBranch[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, branchIterator).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: NodeTree[T1] =
          TreeBuilder.linearTreeFromSequence(value +: remainingBranchIterator.toSeq).asInstanceOf[NodeTree[T1]]

        val newNode = Tree(remainingTree.head, branchTree +: remainingTree.children)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case _ => None
    }

  final def toPairsList[T](node: NodeTree[T]): Seq[(Int, T)] = toPairsList(Vector.empty, Vector(node))

  @tailrec
  private def toPairsList[T](result: Vector[(Int, T)], queue: Vector[NodeTree[T]]): Seq[(Int, T)] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Leaf(head)           => toPairsList((0, head) +: result, queue.safeTail)
        case Node(head, children) => toPairsList((children.size, head) +: result, children ++: queue.safeTail)
      }

  final def toSlices[T: ClassTag](node: NodeTree[T]): (IntSlice, Slice[T]) = {
    val (structure, values) = toArrays(node)
    (IntSlice.of(structure), Slice.of(values))
  }

  final def toBuffers[T: ClassTag](node: NodeTree[T]): (IntBuffer, Buffer[T]) = {
    val (structure, values) = toArrays(node)
    (IntBuffer(structure), Buffer(values))
  }

  final def toArrays[T: ClassTag](node: NodeTree[T]): (Array[Int], Array[T]) = {
    val queue = new Array[NodeTree[T]](Math.max(node.width, node.height))
    queue(0) = node
    toArrays(new Array[Int](node.size), new Array[T](node.size), queue, node.size - 1, 0)
  }

  @tailrec
  private final def toArrays[T: ClassTag](
    structure: Array[Int],
    values: Array[T],
    queue: Array[NodeTree[T]],
    position: Int,
    queuePosition: Int
  ): (Array[Int], Array[T]) =
    if (position < 0) (structure, values)
    else
      queue(queuePosition) match {
        case Leaf(head) =>
          structure.update(position, 0)
          values.update(position, head)
          toArrays(structure, values, queue, position - 1, queuePosition - 1)

        case Node(head, children) =>
          structure.update(position, children.size)
          values.update(position, head)
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          toArrays(structure, values, queue, position - 1, queuePosition + children.size - 1)
      }

  @`inline` final def toStructureArray[T](node: NodeTree[T]): Array[Int] = {
    val queue = new Array[NodeTree[T]](Math.max(node.width, node.height))
    queue(0) = node
    toStructureArray(new Array[Int](node.size), queue, node.size - 1, 0)
  }

  @tailrec
  private final def toStructureArray[T](
    structure: Array[Int],
    queue: Array[NodeTree[T]],
    position: Int,
    queuePosition: Int
  ): Array[Int] =
    if (position < 0) structure
    else
      queue(queuePosition) match {
        case Leaf(_) =>
          structure.update(position, 0)
          toStructureArray(structure, queue, position - 1, queuePosition - 1)

        case Node(_, children) =>
          structure.update(position, children.size)
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          toStructureArray(structure, queue, position - 1, queuePosition + children.size - 1)
      }

  @tailrec
  final def toTreeList[T](
    result: Vector[(Int, Tree[T])],
    queue: Vector[NodeTree[T]]
  ): Vector[(Int, Tree[T])] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Leaf(head)           => toTreeList((0, Tree(head)) +: result, queue.safeTail)
        case Node(head, children) => toTreeList((children.size, Tree(head)) +: result, children ++: queue.safeTail)
      }

  @`inline` final def listMap[T, K](f: T => K, node: NodeTree[T]): Vector[(Int, K)] =
    listMap(f, Vector.empty, Vector(node))

  @tailrec
  private final def listMap[T, K](f: T => K, result: Vector[(Int, K)], queue: Vector[NodeTree[T]]): Vector[(Int, K)] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Leaf(head)                => listMap(f, (0, f(head)) +: result, queue.safeTail)
        case Unary(head, child)        => listMap(f, (1, f(head)) +: result, child +: queue.safeTail)
        case Binary(head, left, right) => listMap(f, (2, f(head)) +: result, left +: right +: queue.safeTail)
        case Node(head, children)      => listMap(f, (children.size, f(head)) +: result, children ++: queue.safeTail)
      }

  @`inline` final def arrayMap[T, K](f: T => K, node: NodeTree[T])(
    implicit tag: ClassTag[K]
  ): (Array[Int], Array[K]) = {
    val queue = new Array[NodeTree[T]](Math.max(node.width, node.height))
    queue(0) = node
    arrayMap(f, new Array[Int](node.size), new Array[K](node.size), queue, node.size - 1, 0)
  }

  @tailrec
  private final def arrayMap[T, K](
    f: T => K,
    structure: Array[Int],
    values: Array[K],
    queue: Array[NodeTree[T]],
    position: Int,
    queuePosition: Int
  ): (Array[Int], Array[K]) =
    if (position < 0) (structure, values)
    else
      queue(queuePosition) match {
        case Leaf(head) =>
          structure.update(position, 0)
          values.update(position, f(head))
          arrayMap(f, structure, values, queue, position - 1, queuePosition - 1)

        case Unary(head, child) =>
          structure.update(position, 1)
          values.update(position, f(head))
          queue(queuePosition) = child
          arrayMap(f, structure, values, queue, position - 1, queuePosition)

        case Binary(head, left, right) =>
          structure.update(position, 2)
          values.update(position, f(head))
          queue(queuePosition) = right
          queue(queuePosition + 1) = left
          arrayMap(f, structure, values, queue, position - 1, queuePosition + 1)

        case Node(head, children) =>
          structure.update(position, children.size)
          values.update(position, f(head))
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          arrayMap(f, structure, values, queue, position - 1, queuePosition + children.size - 1)
      }

  @tailrec
  final def listFlatMap[T, K](
    f: T => Tree[K],
    result: Vector[(Int, Tree[K])],
    queue: Vector[NodeTree[T]]
  ): Vector[(Int, Tree[K])] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Leaf(head)           => listFlatMap(f, (0, f(head)) +: result, queue.safeTail)
        case Node(head, children) => listFlatMap(f, (children.size, f(head)) +: result, children ++: queue.safeTail)
      }

  final def mkStringUsingBranches[T](
    node: NodeTree[T],
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int
  ): StringBuilder =
    if (maxDepth <= 0) new StringBuilder
    else
      mkStringUsingBranches(
        show,
        valueSeparator,
        branchSeparator,
        branchEnd,
        maxDepth,
        new StringBuilder(branchStart),
        Vector((1, branchStart, node)),
        newBranch = false
      )

  @tailrec
  private def mkStringUsingBranches[T](
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchEnd: String,
    maxDepth: Int,
    builder: StringBuilder,
    queue: Vector[(Int, String, NodeTree[T])],
    newBranch: Boolean
  ): StringBuilder =
    if (queue.isEmpty) builder
    else
      queue.head match {
        case (level, prefix, Node(head, children)) =>
          val string = show(head)
          if (level <= maxDepth) {
            if (newBranch) builder.append(branchSeparator).append(prefix)
            if (level > 1) builder.append(valueSeparator)
            builder.append(string)
          }
          val subtrees2 = if (level >= maxDepth) Nil else children
          subtrees2 match {
            case Nil =>
              mkStringUsingBranches(
                show,
                valueSeparator,
                branchSeparator,
                branchEnd,
                maxDepth,
                builder.append(branchEnd),
                queue.safeTail,
                newBranch = true
              )
            case _ =>
              mkStringUsingBranches(
                show,
                valueSeparator,
                branchSeparator,
                branchEnd,
                maxDepth,
                builder,
                children
                  .map((level + 1, prefix + (if (level > 1) valueSeparator else "") + string, _)) ++: queue.safeTail,
                newBranch = false
              )
          }
      }

  /** Inserts new child into a tree keeping children distinct.
    * @note distinct child is prepended on the left side of existing children list,
    *       otherwise merged down with existing duplicate.
    */
  final def insertChildDistinct[T, T1 >: T](tree: NodeTree[T], newChild: NodeTree[T1]): Tree[T1] =
    insertChildDistinct(tree.head, Nil, newChild, tree.children, preserveExisting = true)

  /** Ensures that a child at an index position is distinct,
    * if not, then merges it with the nearest duplicate on the right side or left side.
    * The global order of children is preserved.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.
    */
  final def ensureChildDistinct[T](tree: NodeTree[T], index: Int, preserveExisting: Boolean): NodeTree[T] =
    tree.children.drop(index) match {
      case s if s.isEmpty => tree
      case s =>
        insertChildDistinct(tree.head, tree.children.take(index), s.head, s.tail, preserveExisting)
    }

  /** Inserts new children distinct between left and right siblings.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.*/
  final def insertChildrenDistinct[T](
    head: T,
    leftSiblings: Seq[NodeTree[T]],
    newChildren: Iterable[NodeTree[T]],
    rightSiblings: Seq[NodeTree[T]],
    preserveExisting: Boolean
  ): NodeTree[T] = {

    @tailrec
    def insert(
      queue: Iterator[NodeTree[T]],
      left: Seq[NodeTree[T]],
      right: Seq[NodeTree[T]]
    ): (Seq[NodeTree[T]], Seq[NodeTree[T]]) =
      if (queue.hasNext) {
        val newChild = queue.next
        val (newLeft, newRight) = insertDistinctBetweenSiblings(left, newChild, right, preserveExisting)
        insert(queue, newLeft, newRight)
      } else (left, right)

    val (left, right) = insert(newChildren.iterator, leftSiblings, rightSiblings)
    Tree(head, left ++ right)
  }

  /** Inserts new child distinct between left and right siblings.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.*/
  final def insertChildDistinct[T](
    head: T,
    leftSiblings: Seq[NodeTree[T]],
    newChild: NodeTree[T],
    rightSiblings: Seq[NodeTree[T]],
    preserveExisting: Boolean
  ): NodeTree[T] = {
    val (left, right) = insertDistinctBetweenSiblings(leftSiblings, newChild, rightSiblings, preserveExisting)
    Tree(head, left ++ right)
  }

  /** Inserts new child distinct between left and right siblings.
    * If distinct then appends to the left side,
    * otherwise merges with the nearest duplicate on the left (preferred) or right.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.
    */
  final def insertDistinctBetweenSiblings[T](
    leftSiblings: Seq[NodeTree[T]],
    newChild: NodeTree[T],
    rightSiblings: Seq[NodeTree[T]],
    preserveExisting: Boolean
  ): (Seq[NodeTree[T]], Seq[NodeTree[T]]) =
    splitSequenceWhen[NodeTree[T]](_.head == newChild.head, leftSiblings.reverse) match {
      case Some((right, duplicateOnLeft, left)) =>
        val mergedNode =
          insertChildrenDistinct(newChild.head, duplicateOnLeft.children, newChild.children, Nil, preserveExisting)
        (left.reverse ++: mergedNode +: right.reverse, rightSiblings)

      case None =>
        splitSequenceWhen[NodeTree[T]](_.head == newChild.head, rightSiblings) match {
          case None =>
            (leftSiblings :+ newChild, rightSiblings)

          case Some((left, duplicateOnRight, right)) =>
            val mergedNode =
              insertChildrenDistinct(
                newChild.head,
                newChild.children,
                duplicateOnRight.children,
                Nil,
                preserveExisting
              )
            if (preserveExisting) (leftSiblings, left ++: mergedNode +: right)
            else (leftSiblings :+ mergedNode, left ++: right)
        }
    }

  /** Makes tree's children deeply distinct.
    * @param tree the node under investigation
    * @param maxLookupLevel the max number of nesting levels to investigate children,
    *                       does not constraint merging algorithm as it will always proceed as deep as necessary.
    */
  @`inline` final def makeTreeDistinct[T](tree: NodeTree[T], maxLookupLevel: Int = Int.MaxValue): NodeTree[T] =
    makeTreeDistinct(Vector((tree, true, 1)), Vector.empty, maxLookupLevel)

  @tailrec
  private final def makeTreeDistinct[T](
    queue: Vector[(NodeTree[T], Boolean, Int)],
    result: Vector[(Int, NodeTree[T])],
    maxLookupLevel: Int
  ): NodeTree[T] =
    if (queue.isEmpty) {
      TreeBuilder.fromSizeAndTreePairsIterable(result).head.asInstanceOf[NodeTree[T]]
    } else {
      val (tree, inspectChildren, level) = queue.head
      if (inspectChildren || level <= maxLookupLevel) {
        val groups = groupChildrenByValue(tree.children)
        val newQueuePrefix = groups.map { group =>
          if (group.size == 1) (group.head, false, level + 1)
          else {
            val concatChildren = group.flatMap(_.children).toList
            (Tree(group.head.head, concatChildren), concatChildren.size > 1, level + 1)
          }
        }
        makeTreeDistinct(
          newQueuePrefix ++: queue.tail,
          (newQueuePrefix.size, Tree(tree.head)) +: result,
          maxLookupLevel
        )
      } else {
        makeTreeDistinct(queue.tail, (0, tree) +: result, maxLookupLevel)
      }
    }

  /** Groups children by its head. */
  private final def groupChildrenByValue[T](seq: Seq[NodeTree[T]]): Vector[Vector[NodeTree[T]]] = {
    val map: mutable.Map[T, Int] = mutable.Map.empty
    val buffer = Buffer.empty[Vector[NodeTree[T]]]
    seq.foreach { tree =>
      val pos = map.getOrElseUpdate(tree.head, buffer.length)
      buffer(pos) = buffer.get(pos).getOrElse(Vector.empty).:+(tree)
    }
    buffer.iterator.toVector
  }

  /** Builds a tree from a list of (numberOfChildrenToCollect, nodeValue, partialChildren).
    * Each tree node must come in the list after its `numberOfPrecedingChildren` children.
    *
    * @param prepend if true, partialChildren are eventually prepended to the collected ones,
    *                if false, they are appended.*/
  @tailrec
  final def buildTreeFromPartials[T, T1 >: T](
    queue: Vector[(Int, T, Seq[NodeTree[T1]])],
    result: Seq[NodeTree[T1]],
    prepend: Boolean = true
  ): Seq[NodeTree[T1]] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case (numberOfChildrenToCollect, nodeValue, partialChildren) =>
          val node = Tree(
            nodeValue,
            if (prepend) partialChildren ++: result.take(numberOfChildrenToCollect)
            else result.take(numberOfChildrenToCollect) ++: partialChildren
          )
          buildTreeFromPartials(queue.safeTail, node +: result.drop(numberOfChildrenToCollect), prepend)
      }

  final def insertTreeAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    nodeToInsert: NodeTree[T1],
    keepDistinct: Boolean
  ): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, pathIterator).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: NodeTree[T1] =
          TreeBuilder
            .fromTreeSequence((Tree(value) +: remainingBranchIterator.map(Tree.apply[T1]).toVector) :+ nodeToInsert)
            .asInstanceOf[NodeTree[T1]]
        val newNode = Tree(remainingTree.head, branchTree +: remainingTree.children)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case (treeSplit, None, _, remainingTree) =>
        val newNode =
          if (keepDistinct && !remainingTree.isLeaf)
            remainingTree.insertChild(nodeToInsert).asInstanceOf[NodeTree[T1]]
          else Tree(remainingTree.head, nodeToInsert +: remainingTree.children)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
    }

  final def insertTreeAt[T, T1 >: T: ClassTag, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    nodeToInsert: NodeTree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          val newNode =
            if (keepDistinct && !recipientTree.isLeaf)
              recipientTree.insertChild(nodeToInsert).asInstanceOf[NodeTree[T1]]
            else Tree(recipientTree.head, nodeToInsert +: recipientTree.children)
          Right(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
      }
      .getOrElse(Left(tree))

  /** Splits the tree following the path and succeeds only if all path items exists.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some pair of (the tree split, recipient tree holding the last path item)
    *         or none if:
    *         1) the path doesn't exist in full,
    *         2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingEntirePath[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1]
  ): Option[(Vector[TreeSplit[T]], NodeTree[T])] =
    splitTreeFollowingPath[T, T1](tree, pathIterator).flatMap {
      case (_, Some(_), _, _)                  => None
      case (treeSplit, None, _, remainingTree) => Some((treeSplit, remainingTree))
    }

  /** Splits the tree following the path.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some quadruple of (
    *         - the tree split,
    *         - optionally last unmatched path item,
    *         - remaining path iterator,
    *         - a remaining tree holding the last matched path value
    *         ) or none if the path root doesn't match tree root at all.
    */
  final def splitTreeFollowingPath[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1]
  ): Option[(Vector[TreeSplit[T]], Option[T1], Iterator[T1], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (tree.head == head) Some(splitTreeFollowingPath(tree, pathIterator, Vector.empty)) else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    queue: Vector[TreeSplit[T]]
  ): (Vector[TreeSplit[T]], Option[T1], Iterator[T1], NodeTree[T]) =
    if (pathIterator.hasNext) {
      val value: T1 = pathIterator.next()
      splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
        case None =>
          (queue, Some(value), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, (left, tree.head, right) +: queue)
      }
    } else {
      (queue, None, pathIterator, tree)
    }

  /** Splits the tree following the path, using toPathItem extractor function,
    * and succeeds only if all path items exists.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some pair of (the tree split, recipient tree matching the last path item)
    *         or none if:
    *         1) the path doesn't exist in full,
    *         2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingEntirePath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K
  ): Option[(Vector[TreeSplit[T]], NodeTree[T])] =
    splitTreeFollowingPath(tree, pathIterator, toPathItem).flatMap {
      case (_, Some(_), _, _)                  => None
      case (treeSplit, None, _, remainingTree) => Some((treeSplit, remainingTree))
    }

  /** Splits the tree following the path using toPathItem extractor function.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some quadruple of (
    *         - the tree split,
    *         - optionally last unmatched path item,
    *         - remaining path iterator,
    *         - a remaining tree holding the last matched path item
    *         ) or none if:
    *               1) the path doesn't exist in full,
    *               2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingPath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K
  ): Option[(Vector[TreeSplit[T]], Option[K], Iterator[K], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (toPathItem(tree.head) == head) Some(splitTreeFollowingPath(tree, pathIterator, toPathItem, Vector.empty))
      else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    queue: Vector[TreeSplit[T]]
  ): (Vector[TreeSplit[T]], Option[K], Iterator[K], NodeTree[T]) =
    if (pathIterator.hasNext) {
      val pathItem: K = pathIterator.next()
      splitSequenceWhen[NodeTree[T]](node => toPathItem(node.head) == pathItem, tree.children) match {
        case None =>
          (queue, Some(pathItem), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, toPathItem, (left, tree.head, right) +: queue)
      }
    } else {
      (queue, None, pathIterator, tree)
    }

  /** Optionally splits sequence into left and right part around matching element. */
  final def splitSequenceWhen[T](f: T => Boolean, list: Seq[T]): Option[(Seq[T], T, Seq[T])] = {
    @tailrec
    def split(left: Seq[T], right: Seq[T]): Option[(Seq[T], T, Seq[T])] =
      if (right.isEmpty) None
      else {
        if (f(right.head)) Some((left.reverse, right.head, right.tail))
        else split(right.head +: left, right.tail)
      }
    if (list.isEmpty) None else split(Nil, list)
  }

  /** Joins single treeSplit back into a tree node. */
  @`inline` final def join[T](split: TreeSplit[T]): NodeTree[T] = Tree(split._2, split._1 ++ split._3)

  /** Updates a value of a child, and builds a tree back from the treeSplit. */
  final def updateChildValueInSplit[T, T1 >: T: ClassTag](
    treeSplit: Vector[TreeSplit[T]],
    child: NodeTree[T],
    replacement: T1,
    keepDistinct: Boolean
  ): Tree[T1] = {
    val modifiedChild = Tree(replacement, child.children)
    if (keepDistinct && treeSplit.nonEmpty) {
      val (left, head, right) = treeSplit.head
      val newHead = insertChildDistinct(head, left, modifiedChild, right, preserveExisting = false)
      TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
    } else {
      TreeBuilder.fromChildAndTreeSplit(modifiedChild, treeSplit)
    }
  }

  /** Updates a child, and builds a tree back from the treeSplit. */
  final def updateChildInSplit[T, T1 >: T: ClassTag](
    treeSplit: Vector[TreeSplit[T]],
    child: NodeTree[T],
    replacement: Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    replacement match {
      case Tree.empty =>
        TreeBuilder.fromTreeSplit[T1](treeSplit)

      case tree: NodeTree[T1] =>
        if (keepDistinct && treeSplit.nonEmpty && tree.head != child.head) {
          val (left, head, right) = treeSplit.head
          val newHead = insertChildDistinct(head, left, tree, right, preserveExisting = false)
          TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
        } else {
          TreeBuilder.fromChildAndTreeSplit(tree, treeSplit)
        }

      case tree: ArrayTree[T1] =>
        ArrayTree.buildFromChildAndTreeSplit(tree, treeSplit.tail)
    }

  /** Updates value of the child holding the value. */
  final def updateChildValue[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    replacement: T1,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case Some((left, node, right)) if node.head != replacement =>
        val updatedNode = Tree(replacement, node.children)
        if (updatedNode == node) tree
        else if (keepDistinct) {
          insertChildDistinct(tree.head, left, updatedNode, right, preserveExisting = false)
        } else {
          Tree(tree.head, left ++: updatedNode +: right)
        }
      case _ => tree
    }

  /** Updates value of the node selected by the path. */
  final def updateValueAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    replacement: T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates value of the node selected by the path using a path item extractor. */
  final def updateValueAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    replacement: T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates the child tree holding the value at head. */
  final def updateChild[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    replacement: Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case Some((left, node, right)) if replacement != node =>
        replacement match {
          case Tree.empty =>
            Tree(tree.head, left ++ right)

          case t: NodeTree[T1] =>
            if (keepDistinct && t.head != node.head) {
              insertChildDistinct(tree.head, left, t, right, preserveExisting = false)
            } else {
              Tree(tree.head, left ++: t +: right)
            }

          case t: ArrayTree[T1] =>
            ArrayTree
              .insertChildren(ArrayTree.prepend(tree.head, t), left, right, keepDistinct && t.head != node.head)
        }

      case _ => tree
    }

  /** Updates a subtree selected by the path. */
  final def updateTreeAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    replacement: Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates a subtree selected by the path using path item extractor. */
  final def updateTreeAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    replacement: Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node holding the value. */
  final def modifyChildValue[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    modify: T => T1,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case None => tree
      case Some((left, node, right)) =>
        val replacement = Tree(modify(node.head), node.children)
        if (replacement == node) tree
        else if (keepDistinct) {
          insertChildDistinct(tree.head, left, replacement, right, preserveExisting = false)
        } else {
          Tree(tree.head, left ++: replacement +: right)
        }
    }

  /** Modifies value of the node selected by the path. */
  final def modifyValueAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    modify: T => T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          val replacement = modify(recipientTree.head)
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node selected by the path using path item extractor. */
  final def modifyValueAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: T => T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          val replacement = modify(recipientTree.head)
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies the child tree holding the value at head. */
  final def modifyChild[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case None => tree
      case Some((left, node, right)) =>
        val replacement = modify(node)
        if (replacement == node) tree
        else
          replacement match {
            case Tree.empty =>
              Tree(tree.head, left ++ right)

            case t: NodeTree[T1] =>
              if (keepDistinct && t.head != node.head) {
                insertChildDistinct(tree.head, left, t, right, preserveExisting = false)
              } else {
                Tree(tree.head, left ++: t +: right)
              }

            case t: ArrayTree[T1] =>
              ArrayTree
                .insertChildren(ArrayTree.prepend(tree.head, t), left, right, keepDistinct && t.head != node.head)
          }
    }

  /** Modifies a subtree selected by the path. */
  final def modifyTreeAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          val replacement = modify(recipientTree)
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies a subtree selected by the path using path item extractor. */
  final def modifyTreeAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          val replacement = modify(recipientTree)
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Removes the child node and inserts its children into the treeSplit. */
  final def removeChildValueFromSplit[T](
    tree: NodeTree[T],
    treeSplit: Vector[TreeSplit[T]],
    child: NodeTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    if (treeSplit.isEmpty) {
      if (child.isLeaf) Tree.empty
      else if (child.childrenCount == 1) child.children.head
      else tree
    } else if (child.size <= 1) TreeBuilder.fromTreeSplit(treeSplit)
    else {
      val (left, head, right) = treeSplit.head
      val newChild =
        if (keepDistinct)
          makeTreeDistinct(Tree(head, left ++ child.children ++ right), maxLookupLevel = 1)
        else
          Tree(head, left ++ child.children ++ right)

      TreeBuilder.fromChildAndTreeSplit(newChild, treeSplit.tail)
    }

  /** Removes the child holding the value and inserts its children into the tree.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * */
  final def removeChildValue[T, T1 >: T](
    tree: NodeTree[T],
    value: T1,
    keepDistinct: Boolean
  ): Tree[T] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case None => tree
      case Some((left, node, right)) =>
        if (keepDistinct && !node.isLeaf) {
          insertChildrenDistinct(tree.head, left, node.children, right, preserveExisting = false)
        } else {
          Tree(tree.head, left ++ node.children ++ right)
        }
    }

  /** Removes the node selected by the path and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * */
  final def removeValueAt[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    keepDistinct: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          removeChildValueFromSplit(tree, treeSplit, recipientTree, keepDistinct)
      }
      .getOrElse(tree)

  /** Removes the node selected by the path and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * */
  final def removeValueAt[K, T](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          removeChildValueFromSplit(tree, treeSplit, recipientTree, keepDistinct)
      }
      .getOrElse(tree)

  /** Removes the direct child holding the value. */
  final def removeChild[T, T1 >: T](
    tree: NodeTree[T],
    value: T1
  ): Tree[T] =
    splitSequenceWhen[NodeTree[T]](_.head == value, tree.children) match {
      case None                   => tree
      case Some((left, _, right)) => Tree(tree.head, left ++ right)
    }

  /** Removes the tree selected by the path. */
  final def removeTreeAt[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1]
  ): Tree[T] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator)
      .map { case (treeSplit, _) => TreeBuilder.fromTreeSplit(treeSplit) }
      .getOrElse(tree)

  /** Removes the tree selected by the path using an extractor function. */
  final def removeTreeAt[K, T](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K
  ): Tree[T] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem)
      .map { case (treeSplit, _) => TreeBuilder.fromTreeSplit(treeSplit) }
      .getOrElse(tree)

}
