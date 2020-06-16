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
import com.github.arturopala.bufferandslice.{ArrayOps, Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.internal.VectorOps._
import com.github.arturopala.tree.internal.IteratorOps._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/** Collection of operations on the hierarchical, node-based, representation of the tree. */
object NodeTree {

  /** Type alias of a tree split, i.e. a tuple of (leftChildren, head, rightChildren). */
  type TreeSplit[T] = (Seq[Tree[T]], T, Seq[Tree[T]])

  final def leavesIterator[T](node: Tree[T]): Iterator[T] = new Iterator[T] {

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
          case Tree.Leaf(head) =>
            nextItem = head
            hasNext = true
            queue = queue.trim

          case Tree(_, children: Iterable[Tree[T]]) =>
            queue = children ++: queue.trim
            seekNext()

          case _ =>
            seekNext()
        }
      }
    }
  }

  /** Returns an iterator over all the nodes of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesIterator[T](node: Tree[T], depthFirst: Boolean): Iterator[T] = new Iterator[T] {

    type Queue = Iterator[Tree[T]]
    private var queue: Queue = Iterator(node)

    override final def hasNext: Boolean = queue.nonEmpty

    override final def next(): T =
      if (queue.isEmpty) throw new NoSuchElementException()
      else {
        val Tree(head: T, children: Iterable[Tree[T]]) = queue.next
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
  final def valuesIteratorWithFilter[T](pred: T => Boolean, node: Tree[T], depthFirst: Boolean): Iterator[T] =
    new FilterIterator(valuesIterator(node, depthFirst), pred)

  /** Returns an iterator over all the values and levels of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first.
    */
  final def valuesAndLevelsIterator[T](
    node: Tree[T],
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
          val (level, node @ Tree(head: T, children: Iterable[Tree[T]])) = queue.next
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
    node: Tree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, T, Boolean)] =
    new FilterIterator(valuesAndLevelsIterator(node, maxDepth, depthFirst), (t: (Int, T, Boolean)) => pred(t._2))

  /** Returns an iterator over the filtered nodes of the tree with maxDepth limit.
    * @note uses Iterator internally */
  final def valuesIteratorWithLimit[T](
    pred: T => Boolean,
    node: Tree[T],
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
          val (level, Tree(head: T, children: Iterable[Tree[T]])) = queue.next
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
  final def values[T](pred: T => Boolean, node: Tree[T]): Vector[T] = values(pred, Vector.empty[T], Vector(node))

  @tailrec
  private def values[T](pred: T => Boolean, result: Vector[T], queue: Vector[Tree[T]]): Vector[T] =
    if (queue.isEmpty) result
    else {
      val Tree(head: T, children: Iterable[Tree[T]]) = queue.head
      if (pred(head)) values(pred, result :+ head, children ++: queue.safeTail)
      else values(pred, result, children ++: queue.safeTail)
    }

  /** Returns an iterator over all (sub)trees of the tree inclusive. */
  final def treesIterator[T](node: Tree[T], depthFirst: Boolean): Iterator[Tree[T]] = new Iterator[Tree[T]] {

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
    node: Tree[T],
    depthFirst: Boolean
  ): Iterator[Tree[T]] =
    new FilterIterator(treesIterator(node, depthFirst), pred)

  /** Returns an iterator over pairs of (level, tree) of the tree inclusive. */
  final def treesAndLevelsIterator[T](node: Tree[T], maxDepth: Int, depthFirst: Boolean): Iterator[(Int, Tree[T])] =
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
    node: Tree[T],
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, Tree[T])] =
    new FilterIterator(treesAndLevelsIterator(node, maxDepth, depthFirst), (t: (Int, Tree[T])) => pred(t._2))

  /** Returns an iterator over filtered (sub)trees of the tree with depth limit.
    * @note uses Iterator internally */
  final def treesIteratorWithLimit[T](
    pred: Tree[T] => Boolean,
    node: Tree[T],
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
  final def trees[T](pred: Tree[T] => Boolean, node: Tree[T]): Vector[Tree[T]] =
    trees(pred, Vector.empty, Vector(node))

  @tailrec
  private def trees[T](
    pred: Tree[T] => Boolean,
    result: Vector[Tree[T]],
    queue: Vector[Tree[T]]
  ): Vector[Tree[T]] =
    if (queue.isEmpty) result
    else {
      val node @ Tree(_, children: Iterable[Tree[T]]) = queue.head
      if (pred(node)) trees(pred, result :+ node, children ++: queue.safeTail)
      else trees(pred, result, children ++: queue.safeTail)
    }

  /** Returns an iterator over all paths of the tree. */
  final def pathsIterator[T](node: Tree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Iterator[(Vector[T], Tree[T])]
      private var queue: Queue = Iterator((Vector.empty, node))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = queue.next
          val path = acc :+ head
          queue = children.map((path, _)) ++: queue.trim
          path
        }
    }

  /** Returns an iterator over all branches of the tree. */
  final def branchesIterator[T](node: Tree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], Tree[T])]
      private var queue: Queue = seekNext(Vector((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = queue.head
          val branch = acc :+ head
          queue = seekNext(children.map((branch, _)) ++: queue.safeTail)
          branch
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = q.head
          val branch = acc :+ head
          children match {
            case Nil => q
            case _   => seekNext(children.map((branch, _)) ++: q.safeTail)
          }
        }
    }

  /** Returns an iterator over filtered branches of the tree. */
  final def branchesIteratorWithFilter[T](
    pred: Iterable[T] => Boolean,
    node: Tree[T],
    partialPaths: Boolean
  ): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], Tree[T])]
      private var queue: Queue = seekNext(Vector((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = queue.head
          val path = acc :+ head
          queue = seekNext(children.map((path, _)) ++: queue.safeTail)
          path
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = q.head
          val path = acc :+ head
          children match {
            case ch if (partialPaths || ch.isEmpty) && pred(path) => q
            case _                                                => seekNext(children.map((path, _)) ++: q.safeTail)
          }
        }
    }

  /** Returns an iterator over filtered branches of the tree with depth limit. */
  final def branchesIteratorWithLimit[T](
    pred: Iterable[T] => Boolean,
    node: Tree[T],
    maxDepth: Int,
    partialPaths: Boolean
  ): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = Vector[(Vector[T], Tree[T])]
      private var queue: Queue =
        if (maxDepth > 0) seekNext(Vector((Vector.empty, node)))
        else Vector.empty

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] =
        if (queue.isEmpty) throw new NoSuchElementException()
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = queue.head
          val path = acc :+ head
          queue =
            if (path.length < maxDepth) seekNext(children.map((path, _)) ++: queue.safeTail)
            else seekNext(queue.safeTail)
          path
        }

      @tailrec
      private def seekNext(q: Queue): Queue =
        if (q.isEmpty) q
        else {
          val (acc, Tree(head: T, children: Iterable[Tree[T]])) = q.head
          val path = acc :+ head
          children match {
            case s if (partialPaths || s.isEmpty || path.length >= maxDepth) && pred(path) => q
            case _ =>
              if (path.length < maxDepth) seekNext(children.map((path, _)) ++: q.safeTail)
              else seekNext(q.safeTail)
          }
        }
    }

  final def branches[T](pred: Iterable[T] => Boolean, node: Tree[T]): Iterable[Iterable[T]] =
    branches(pred, Vector.empty, Vector((Vector.empty, node)))

  @tailrec
  private def branches[T](
    pred: Iterable[T] => Boolean,
    result: Vector[Vector[T]],
    queue: Vector[(Vector[T], Tree[T])]
  ): Vector[Vector[T]] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case (acc, Tree(head: T, children: Iterable[Tree[T]])) =>
          val branch = acc :+ head
          children match {
            case Nil if pred(branch) => branches(pred, result :+ branch, queue.safeTail)
            case _                   => branches(pred, result, children.map((branch, _)) ++: queue.safeTail)
          }

        case _ => branches(pred, result, queue.safeTail)
      }

  final def countBranches[T](pred: Iterable[T] => Boolean, node: Tree[T]): Int =
    countBranches(pred, 0, Iterator((Vector.empty, node)))

  @tailrec
  private def countBranches[T](
    pred: Iterable[T] => Boolean,
    result: Int,
    queue: Iterator[(Vector[T], Tree[T])]
  ): Int =
    if (queue.isEmpty) result
    else {
      val (acc, Tree(head: T, children: Iterable[Tree[T]])) = queue.next
      val branch = acc :+ head
      children match {
        case i if i.isEmpty && pred(branch) => countBranches(pred, 1 + result, queue.trim)
        case _                              => countBranches(pred, result, children.map((branch, _)) ++: queue.trim)
      }
    }

  @tailrec
  final def select[T, T1 >: T, R](
    node: Tree[T],
    path: Iterable[T1],
    result: Tree[T] => R,
    rightmost: Boolean
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != node.head)) None
    else if (path.tail.isEmpty) {
      if (path.head == node.head) Some(result(node)) else None
    } else {
      val item = path.tail.head
      val nextOpt =
        if (rightmost) node.children.filter(_.head == item).lastOption
        else node.children.iterator.find(_.head == item)
      if (nextOpt.isEmpty) None
      else select(nextOpt.get, path.tail, result, rightmost)
    }

  @tailrec
  final def select[T, K, R](
    node: Tree[T],
    path: Iterable[K],
    toResult: Tree[T] => R,
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != toPathItem(node.head))) None
    else if (path.tail.isEmpty) {
      if (path.head == toPathItem(node.head)) Some(toResult(node)) else None
    } else {
      val item = path.tail.head
      val nextOpt =
        if (rightmost) node.children.filter(n => toPathItem(n.head) == item).lastOption
        else node.children.iterator.find(n => toPathItem(n.head) == item)
      if (nextOpt.isEmpty) None
      else select(nextOpt.get, path.tail, toResult, toPathItem, rightmost)
    }

  @`inline` final def containsBranch[T, T1 >: T](node: Tree[T], branch: Iterable[T1]): Boolean =
    contains(node, branch, requiresFullMatch = true)

  @`inline` final def containsBranch[T, K](node: Tree[T], branch: Iterable[K], toPathItem: T => K): Boolean =
    contains(node, branch, requiresFullMatch = true, toPathItem)

  @`inline` final def containsPath[T, T1 >: T](node: Tree[T], path: Iterable[T1]): Boolean =
    contains(node, path, requiresFullMatch = false)

  @`inline` final def containsPath[T, K](node: Tree[T], path: Iterable[K], toPathItem: T => K): Boolean =
    contains(node, path, requiresFullMatch = false, toPathItem)

  @tailrec
  final def contains[T, T1 >: T](node: Tree[T], path: Iterable[T1], requiresFullMatch: Boolean): Boolean =
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
    node: Tree[T],
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

  /** Returns true if branch or path fulfilling the predicate exists in the tree. */
  final def existsBranch[T](
    pred: Iterable[T] => Boolean,
    node: Tree[T],
    partialPaths: Boolean
  ): Boolean = {

    type Queue = Vector[(Vector[T], Tree[T])]

    @tailrec
    def seekNext(q: Queue): Queue =
      if (q.isEmpty) q
      else {
        val (acc, Tree(head: T, children: Iterable[Tree[T]])) = q.head
        val path = acc :+ head
        children match {
          case ch if (partialPaths || ch.isEmpty) && pred(path) => q
          case _                                                => seekNext(children.map((path, _)) ++: q.safeTail)
        }
      }

    seekNext(Vector((Vector.empty, node))).nonEmpty
  }

  /** Returns true if branch or path fulfilling the predicate exists in the tree. */
  final def existsBranch[K, T](
    pred: Iterable[K] => Boolean,
    node: Tree[T],
    partialPaths: Boolean,
    toPathItem: T => K
  ): Boolean = {

    type Queue = Vector[(Vector[K], Tree[T])]

    @tailrec
    def seekNext(q: Queue): Queue =
      if (q.isEmpty) q
      else {
        val (acc, Tree(head: T, children: Iterable[Tree[T]])) = q.head
        val path = acc :+ toPathItem(head)
        children match {
          case ch if (partialPaths || ch.isEmpty) && pred(path) => q
          case _                                                => seekNext(children.map((path, _)) ++: q.safeTail)
        }
      }

    seekNext(Vector((Vector.empty, node))).nonEmpty
  }

  /** Prepends children of the tree with the new child. */
  final def prependChild[T, T1 >: T: ClassTag](tree: Tree[T], child: Tree[T1]): Tree[T1] = tree match {
    case Tree.Leaf(head)                    => Tree.Unary(head, child)
    case Tree.Unary(head, existingChild)    => Tree.Binary(head, child, existingChild)
    case Tree.Binary(head, left, right)     => Tree.Bunch(head, List(child, left, right))
    case Tree.Bunch(head, existingChildren) => Tree.Bunch(head, child +: existingChildren)
    case tree: Tree.ArrayTree[T]            => ArrayTree.prependChild(tree, child)
    case _                                  => Tree.empty
  }

  /** Appends new child to the children of the tree. */
  final def appendChild[T, T1 >: T: ClassTag](tree: Tree[T], child: Tree[T1]): Tree[T1] = tree match {
    case Tree.Leaf(head)                    => Tree.Unary(head, child)
    case Tree.Unary(head, existingChild)    => Tree.Binary(head, existingChild, child)
    case Tree.Binary(head, left, right)     => Tree.Bunch(head, List(left, right, child))
    case Tree.Bunch(head, existingChildren) => Tree.Bunch(head, existingChildren :+ child)
    case tree: Tree.ArrayTree[T]            => ArrayTree.appendChild(tree, child)
    case _                                  => Tree.empty
  }

  /** Inserts branch to the tree using unsafe recursion. */
  final def insertBranchUnsafe[T, T1 >: T: ClassTag](tree: Tree[T], branchIterator: Iterator[T1]): Option[Tree[T1]] =
    if (tree.nonEmpty && branchIterator.hasNext && branchIterator.next == tree.head) {
      Some(Tree(tree.head, insertBranchUnsafe(branchIterator.next, Vector.empty, tree.children, branchIterator)))
    } else None

  private def insertBranchUnsafe[T, T1 >: T: ClassTag](
    branchHead: T1,
    subtreesLeft: Vector[Tree[T]],
    subtreesRight: Iterable[Tree[T]],
    branchTailIterator: Iterator[T1]
  ): Vector[Tree[T1]] =
    if (subtreesRight.isEmpty) {
      val branchTree: Tree[T1] = TreeBuilder.linearTreeFromSequence(branchHead +: branchTailIterator.toVector)
      subtreesLeft :+ branchTree
    } else {
      val node = subtreesRight.head
      if (node.head == branchHead) {
        val modified = Tree(
          node.head,
          insertBranchUnsafe(branchTailIterator.next, Vector.empty, node.children, branchTailIterator)
        )
        (subtreesLeft :+ modified) ++ subtreesRight.tail
      } else {
        insertBranchUnsafe(branchHead, subtreesLeft :+ node, subtreesRight.tail, branchTailIterator)
      }
    }

  final def insertBranch[T, T1 >: T: ClassTag](
    tree: Tree[T],
    branchIterator: Iterator[T1],
    append: Boolean
  ): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, branchIterator, rightmost = append).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: Tree[T1] =
          TreeBuilder.linearTreeFromSequence(value +: remainingBranchIterator.toVector)
        val newNode =
          if (append) appendChild(remainingTree, branchTree)
          else prependChild(remainingTree, branchTree)

        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case _ => None
    }

  final def toPairsList[T](node: Tree[T]): Seq[(Int, T)] = toPairsList(Vector.empty, Vector(node))

  @tailrec
  private def toPairsList[T](result: Vector[(Int, T)], queue: Vector[Tree[T]]): Seq[(Int, T)] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Tree.Leaf(head) => toPairsList((0, head) +: result, queue.safeTail)
        case Tree(head: T, children: Iterable[Tree[T]]) =>
          toPairsList((children.size, head) +: result, children ++: queue.safeTail)
        case _ =>
          toPairsList(result, queue.safeTail)
      }

  final def toSlices[T](node: Tree[T]): (IntSlice, Slice[T]) = {
    val (structure, values) = toBuffers(node)
    (structure.asSlice, values.asSlice)
  }

  final def toBuffers[T](node: Tree[T]): (IntBuffer, Buffer[T]) =
    if (node.isEmpty) (IntBuffer.empty, Buffer.empty[T])
    else {
      val queue = Buffer(node)
      toBuffers(new IntBuffer(node.size), Buffer.ofSize[T](node.size), queue, node.size - 1, 0)
    }

  @tailrec
  private final def toBuffers[T](
    structure: IntBuffer,
    values: Buffer[T],
    queue: Buffer[Tree[T]],
    position: Int,
    queuePosition: Int
  ): (IntBuffer, Buffer[T]) =
    if (position < 0) (structure, values)
    else
      queue(queuePosition) match {
        case Tree.Leaf(head) =>
          structure.update(position, 0)
          values.update(position, head)
          toBuffers(structure, values, queue, position - 1, queuePosition - 1)

        case Tree(head: T, children: Iterable[Tree[T]]) =>
          structure.update(position, children.size)
          values.update(position, head)
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          toBuffers(structure, values, queue, position - 1, queuePosition + children.size - 1)

        case _ =>
          toBuffers(structure, values, queue, position, queuePosition - 1)
      }

  final def toArrays[T: ClassTag](node: Tree[T]): (Array[Int], Array[T]) = {
    val (structure, values) = toBuffers(node)
    (structure.toArray, values.toArray)
  }

  @`inline` final def toStructureArray[T](node: Tree[T]): Array[Int] =
    toStructureBuffer(new IntBuffer(node.size), Buffer(node), node.size - 1, 0).asArray

  @tailrec
  private final def toStructureBuffer[T](
    structure: IntBuffer,
    queue: Buffer[Tree[T]],
    position: Int,
    queuePosition: Int
  ): IntBuffer =
    if (position < 0) structure
    else
      queue(queuePosition) match {
        case Tree.Leaf(_) =>
          structure.update(position, 0)
          toStructureBuffer(structure, queue, position - 1, queuePosition - 1)

        case Tree(_, children: Iterable[Tree[T]]) =>
          structure.update(position, children.size)
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          toStructureBuffer(structure, queue, position - 1, queuePosition + children.size - 1)

        case _ =>
          toStructureBuffer(structure, queue, position, queuePosition - 1)
      }

  @tailrec
  final def toTreeList[T](
    result: Vector[(Int, Tree[T])],
    queue: Vector[Tree[T]]
  ): Vector[(Int, Tree[T])] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Tree.Leaf(head) =>
          toTreeList((0, Tree(head)) +: result, queue.safeTail)
        case Tree(head: T, children: Iterable[Tree[T]]) =>
          toTreeList((children.size, Tree(head)) +: result, children ++: queue.safeTail)
        case _ =>
          toTreeList(result, queue.safeTail)
      }

  @`inline` final def listMap[T, K](f: T => K, node: Tree[T]): Vector[(Int, K)] =
    listMap(f, Vector.empty, Vector(node))

  @tailrec
  private final def listMap[T, K](f: T => K, result: Vector[(Int, K)], queue: Vector[Tree[T]]): Vector[(Int, K)] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Tree.Leaf(head)                => listMap(f, (0, f(head)) +: result, queue.safeTail)
        case Tree.Unary(head, child)        => listMap(f, (1, f(head)) +: result, child +: queue.safeTail)
        case Tree.Binary(head, left, right) => listMap(f, (2, f(head)) +: result, left +: right +: queue.safeTail)
        case Tree(head: T, children: Iterable[Tree[T]]) =>
          listMap(f, (children.size, f(head)) +: result, children ++: queue.safeTail)
        case _ =>
          listMap(f, result, queue.safeTail)
      }

  @`inline` final def arrayMap[T, K](f: T => K, node: Tree[T]): (Array[Int], Array[K]) = {
    val queue = new Array[Tree[T]](Math.max(node.width, node.height))
    queue(0) = node
    val valuesArray = ArrayOps.newArray(f(node.head), node.size)
    arrayMap(f, new Array[Int](node.size), valuesArray, queue, node.size - 1, 0)
  }

  @tailrec
  private final def arrayMap[T, K](
    f: T => K,
    structure: Array[Int],
    values: Array[K],
    queue: Array[Tree[T]],
    position: Int,
    queuePosition: Int
  ): (Array[Int], Array[K]) =
    if (position < 0) (structure, values)
    else
      queue(queuePosition) match {
        case Tree.Leaf(head) =>
          structure.update(position, 0)
          values.update(position, f(head))
          arrayMap(f, structure, values, queue, position - 1, queuePosition - 1)

        case Tree.Unary(head, child) =>
          structure.update(position, 1)
          values.update(position, f(head))
          queue(queuePosition) = child
          arrayMap(f, structure, values, queue, position - 1, queuePosition)

        case Tree.Binary(head, left, right) =>
          structure.update(position, 2)
          values.update(position, f(head))
          queue(queuePosition) = right
          queue(queuePosition + 1) = left
          arrayMap(f, structure, values, queue, position - 1, queuePosition + 1)

        case Tree(head: T, children: Iterable[Tree[T]]) =>
          structure.update(position, children.size)
          values.update(position, f(head))
          var rp: Int = queuePosition + children.size - 1
          children.foreach { child =>
            queue(rp) = child
            rp = rp - 1
          }
          arrayMap(f, structure, values, queue, position - 1, queuePosition + children.size - 1)

        case _ =>
          arrayMap(f, structure, values, queue, position, queuePosition - 1)
      }

  @tailrec
  final def listFlatMap[T, K](
    f: T => Tree[K],
    result: Vector[(Int, Tree[K])],
    queue: Vector[Tree[T]]
  ): Vector[(Int, Tree[K])] =
    if (queue.isEmpty) result
    else
      queue.head match {
        case Tree.Leaf(head) =>
          listFlatMap(f, (0, f(head)) +: result, queue.safeTail)
        case Tree(head: T, children: Iterable[Tree[T]]) =>
          listFlatMap(f, (children.size, f(head)) +: result, children ++: queue.safeTail)
        case _ =>
          listFlatMap(f, result, queue.safeTail)
      }

  final def mkStringUsingBranches[T](
    node: Tree[T],
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
    queue: Vector[(Int, String, Tree[T])],
    newBranch: Boolean
  ): StringBuilder =
    if (queue.isEmpty) builder
    else
      queue.head match {
        case (level, prefix, Tree(head: T, children: Iterable[Tree[T]])) =>
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

        case _ =>
          mkStringUsingBranches(
            show,
            valueSeparator,
            branchSeparator,
            branchEnd,
            maxDepth,
            builder,
            queue.safeTail,
            newBranch
          )

      }

  /** Inserts new child into a tree keeping children distinct.
    * @note distinct child is prepended on the left side of existing children list,
    *       otherwise merged down with existing duplicate.
    */
  final def insertChildDistinct[T, T1 >: T](tree: Tree[T], newChild: Tree[T1], append: Boolean): Tree[T1] =
    Tree(
      tree.head,
      if (append) insertChildrenAfterDistinct(tree.children, List(newChild))
      else insertChildrenBeforeDistinct(List(newChild), tree.children, preserveExisting = true)
    )

  /** Ensures that a child at an index position is distinct,
    * if not, then merges it with the nearest duplicate on the right side or left side.
    * The global order of children is preserved.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.
    */
  final def ensureChildDistinct[T](tree: Tree[T], index: Int, preserveExisting: Boolean): Tree[T] =
    tree.children.drop(index) match {
      case s if s.isEmpty => tree
      case s =>
        Tree(
          tree.head,
          insertDistinctBetweenSiblings(tree.children.take(index), s.head, s.tail, preserveExisting)
        )
    }

  /** Inserts new children distinct between left and right siblings.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.*/
  final def insertChildrenDistinct[T](
    head: T,
    leftSiblings: Iterable[Tree[T]],
    newChildren: Iterable[Tree[T]],
    rightSiblings: Iterable[Tree[T]],
    preserveExisting: Boolean
  ): Tree[T] = {

    @tailrec
    def insert(
      queue: Iterator[Tree[T]],
      left: Iterable[Tree[T]],
      right: Iterable[Tree[T]]
    ): (Iterable[Tree[T]], Iterable[Tree[T]]) =
      if (queue.hasNext) {
        val newChild = queue.next
        val (newLeft, newRight) = insertDistinctBetweenSiblings(left, newChild, right, preserveExisting)
        insert(queue, newLeft, newRight)
      } else (left, right)

    val consolidatedNewChildren = insertChildrenAfterDistinct(Vector.empty[Tree[T]], newChildren)
    Tree(head, insert(consolidatedNewChildren.iterator, leftSiblings, rightSiblings))
  }

  /** Inserts new child distinct between left and right siblings.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.*/
  final def insertChildDistinct[T](
    head: T,
    leftSiblings: Seq[Tree[T]],
    child: Tree[T],
    rightSiblings: Seq[Tree[T]],
    preserveExisting: Boolean
  ): Tree[T] = Tree(head, insertDistinctBetweenSiblings(leftSiblings, child, rightSiblings, preserveExisting))

  /** Inserts new child distinct between left and right siblings.
    * If distinct then appends to the left side,
    * otherwise merges with the nearest duplicate on the left (preferred) or right.
    * @param preserveExisting when having duplicate on the right, whether to keep its current position
    *                         or to prefer new child position (to the left) after merging.
    */
  final def insertDistinctBetweenSiblings[T](
    leftSiblings: Iterable[Tree[T]],
    child: Tree[T],
    rightSiblings: Iterable[Tree[T]],
    preserveExisting: Boolean
  ): (Iterable[Tree[T]], Iterable[Tree[T]]) =
    splitSequenceWhen[Tree[T]](_.head == child.head, leftSiblings, rightmost = true) match {
      case Some((left, duplicateOnLeft, right)) =>
        val mergedNode =
          Tree(child.head, insertChildrenAfterDistinct(duplicateOnLeft.children, child.children))
        (left ++: mergedNode +: right, rightSiblings)

      case None =>
        splitSequenceWhen[Tree[T]](_.head == child.head, rightSiblings, rightmost = false) match {
          case None =>
            (leftSiblings.toVector :+ child, rightSiblings)

          case Some((left, duplicateOnRight, right)) =>
            val mergedNode =
              Tree(
                child.head,
                insertChildrenBeforeDistinct(
                  child.children,
                  duplicateOnRight.children,
                  preserveExisting = false
                )
              )
            if (preserveExisting) (leftSiblings, left ++: mergedNode +: right)
            else (leftSiblings.toVector :+ mergedNode, left ++: right)
        }
    }

  /** Inserts new children before existing while preserving uniqueness. */
  final def insertChildrenBeforeDistinct[T](
    newChildren: Iterable[Tree[T]],
    children: Iterable[Tree[T]],
    preserveExisting: Boolean
  ): Iterable[Tree[T]] = {
    val consolidatedNewChildren = insertChildrenAfterDistinct(Vector.empty[Tree[T]], newChildren)
    consolidatedNewChildren.foldRight(children)((newChild, acc) =>
      insertChildBeforeDistinct(newChild, acc, preserveExisting)
    )
  }

  /** Inserts new child before existing while preserving uniqueness. */
  final def insertChildBeforeDistinct[T](
    newChild: Tree[T],
    children: Iterable[Tree[T]],
    preserveExisting: Boolean
  ): Iterable[Tree[T]] =
    splitSequenceWhen[Tree[T]](_.head == newChild.head, children, rightmost = false) match {
      case Some((left, duplicateOnRight, right)) =>
        val mergedNode = Tree(
          duplicateOnRight.head,
          insertChildrenBeforeDistinct(
            newChild.children,
            duplicateOnRight.children,
            false
          )
        )
        if (preserveExisting) left ++: mergedNode +: right
        else mergedNode +: left ++: right

      case None =>
        newChild +: children.toVector
    }

  /** Inserts new children after existing while preserving uniqueness. */
  final def insertChildrenAfterDistinct[T](
    children: Iterable[Tree[T]],
    newChildren: Iterable[Tree[T]]
  ): Iterable[Tree[T]] =
    newChildren.foldLeft(children)((acc, newChild) => insertChildAfterDistinct(acc, newChild))

  /** Inserts new child after existing while preserving uniqueness. */
  final def insertChildAfterDistinct[T](
    children: Iterable[Tree[T]],
    newChild: Tree[T]
  ): Iterable[Tree[T]] =
    splitSequenceWhen[Tree[T]](_.head == newChild.head, children, rightmost = true) match {
      case Some((left, duplicateOnLeft, right)) =>
        val mergedNode =
          Tree(duplicateOnLeft.head, insertChildrenAfterDistinct(duplicateOnLeft.children, newChild.children))
        left ++: mergedNode +: right

      case None =>
        children.toVector :+ newChild
    }

  /** Makes tree's children deeply distinct.
    * @param tree the node under investigation
    * @param maxLookupLevel the max number of nesting levels to investigate children,
    *                       does not constraint merging algorithm as it will always proceed as deep as necessary.
    */
  @`inline` final def makeTreeDistinct[T](tree: Tree[T], maxLookupLevel: Int = Int.MaxValue): Tree[T] =
    makeTreeDistinct(Vector((tree, true, 1)), Vector.empty, maxLookupLevel)

  @tailrec
  private final def makeTreeDistinct[T](
    queue: Vector[(Tree[T], Boolean, Int)],
    result: Vector[(Int, Tree[T])],
    maxLookupLevel: Int
  ): Tree[T] =
    if (queue.isEmpty) {
      TreeBuilder.fromSizeAndTreePairsIterable(result).head
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
  private final def groupChildrenByValue[T](iterable: Iterable[Tree[T]]): Vector[Vector[Tree[T]]] = {
    val map: mutable.Map[T, Int] = mutable.Map.empty
    val buffer = Buffer.empty[Vector[Tree[T]]]
    iterable.foreach { tree =>
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
    queue: Vector[(Int, T, Seq[Tree[T1]])],
    result: Seq[Tree[T1]],
    prepend: Boolean = true
  ): Seq[Tree[T1]] =
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

  final def insertChildAt[T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    nodeToInsert: Tree[T1],
    append: Boolean,
    keepDistinct: Boolean
  ): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, pathIterator, rightmost = append).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: Tree[T1] =
          TreeBuilder
            .fromTreeSequence(value, remainingBranchIterator.map(Tree.Leaf.apply[T1]).toVector, nodeToInsert)
        val newNode =
          if (append) Tree(remainingTree.head, remainingTree.children, branchTree)
          else Tree(remainingTree.head, branchTree, remainingTree.children)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case (treeSplit, None, _, recipientTree) =>
        val newNode =
          if (keepDistinct && !recipientTree.isLeaf)
            recipientTree.insertChild(nodeToInsert, append)
          else if (append) Tree(recipientTree.head, recipientTree.children, nodeToInsert)
          else Tree(recipientTree.head, nodeToInsert, recipientTree.children)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
    }

  final def insertChildAt[T, T1 >: T: ClassTag, K](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    nodeToInsert: Tree[T1],
    append: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost = append)
      .map {
        case (treeSplit, recipientTree) =>
          val newNode =
            if (keepDistinct && !recipientTree.isLeaf)
              recipientTree.insertChild(nodeToInsert, append)
            else if (append) appendChild(recipientTree, nodeToInsert)
            else prependChild(recipientTree, nodeToInsert)

          Right(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
      }
      .getOrElse(Left(tree))

  /** Inserts new children at the specified path. */
  final def insertChildrenAt[T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    children: Iterable[Tree[T1]],
    append: Boolean,
    keepDistinct: Boolean
  ): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, pathIterator, rightmost = append).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branch = value +: remainingBranchIterator.toVector
        val branchTree: Tree[T1] =
          if (keepDistinct)
            TreeBuilder
              .linearTreeFromSequence(branch)
              .insertChildrenAt(branch, children, append)
          else {
            TreeBuilder
              .fromTreeSequence(branch.init.map(Tree.Leaf.apply[T1]), Tree(branch.last, children))

          }
        val newNode =
          if (append) appendChild(remainingTree, branchTree)
          else prependChild(remainingTree, branchTree)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case (treeSplit, None, _, recipientTree) =>
        val newNode =
          if (keepDistinct) recipientTree.insertChildren(children, append)
          else if (append) Tree(recipientTree.head, (recipientTree.children, children))
          else Tree(recipientTree.head, (children, recipientTree.children))

        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
    }

  /** Inserts new children at the specified path. */
  final def insertChildrenAt[T, T1 >: T: ClassTag, K](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    children: Iterable[Tree[T1]],
    append: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost = append)
      .map {
        case (treeSplit, recipientTree) =>
          val newNode =
            if (keepDistinct) recipientTree.insertChildren(children, append)
            else if (append) Tree(recipientTree.head, (recipientTree.children, children))
            else Tree(recipientTree.head, (children, recipientTree.children))
          Right(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))
      }
      .getOrElse(Left(tree))

  /** Splits the tree following the path and succeeds only if all path items exists.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some pair of (the tree split, recipient tree holding the rightmost path item)
    *         or none if:
    *         1) the path doesn't exist in full,
    *         2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingEntirePath[T, T1 >: T](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    rightmost: Boolean
  ): Option[(Vector[TreeSplit[T]], Tree[T])] =
    splitTreeFollowingPath[T, T1](tree, pathIterator, rightmost).flatMap {
      case (_, Some(_), _, _)                  => None
      case (treeSplit, None, _, remainingTree) => Some((treeSplit, remainingTree))
    }

  /** Splits the tree following the path.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some quadruple of (
    *         - the tree split,
    *         - optionally rightmost unmatched path item,
    *         - remaining path iterator,
    *         - a remaining tree holding the rightmost matched path value
    *         ) or none if the path root doesn't match tree root at all.
    */
  final def splitTreeFollowingPath[T, T1 >: T](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    rightmost: Boolean
  ): Option[(Vector[TreeSplit[T]], Option[T1], Iterator[T1], Tree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (tree.head == head) Some(splitTreeFollowingPath(tree, pathIterator, Vector.empty, rightmost)) else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, T1 >: T](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    queue: Vector[TreeSplit[T]],
    rightmost: Boolean
  ): (Vector[TreeSplit[T]], Option[T1], Iterator[T1], Tree[T]) =
    if (pathIterator.hasNext) {
      val value: T1 = pathIterator.next()
      splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
        case None =>
          (queue, Some(value), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, (left, tree.head, right) +: queue, rightmost)
      }
    } else {
      (queue, None, pathIterator, tree)
    }

  /** Splits the tree following the path, using toPathItem extractor function,
    * and succeeds only if all path items exists.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some pair of (the tree split, recipient tree matching the rightmost path item)
    *         or none if:
    *         1) the path doesn't exist in full,
    *         2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingEntirePath[T, K](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[(Vector[TreeSplit[T]], Tree[T])] =
    splitTreeFollowingPath(tree, pathIterator, toPathItem, rightmost).flatMap {
      case (_, Some(_), _, _)                  => None
      case (treeSplit, None, _, remainingTree) => Some((treeSplit, remainingTree))
    }

  /** Splits the tree following the path using toPathItem extractor function.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some quadruple of (
    *         - the tree split,
    *         - optionally rightmost unmatched path item,
    *         - remaining path iterator,
    *         - a remaining tree holding the rightmost matched path item
    *         ) or none if:
    *               1) the path doesn't exist in full,
    *               2) the path root doesn't match the tree root.
    */
  final def splitTreeFollowingPath[T, K](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[(Vector[TreeSplit[T]], Option[K], Iterator[K], Tree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (toPathItem(tree.head) == head)
        Some(splitTreeFollowingPath(tree, pathIterator, toPathItem, Vector.empty, rightmost))
      else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, K](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    queue: Vector[TreeSplit[T]],
    rightmost: Boolean
  ): (Vector[TreeSplit[T]], Option[K], Iterator[K], Tree[T]) =
    if (pathIterator.hasNext) {
      val pathItem: K = pathIterator.next()
      splitSequenceWhen[Tree[T]](node => toPathItem(node.head) == pathItem, tree.children, rightmost) match {
        case None =>
          (queue, Some(pathItem), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, toPathItem, (left, tree.head, right) +: queue, rightmost)
      }
    } else {
      (queue, None, pathIterator, tree)
    }

  /** Optionally splits sequence into left and right part around matching element. */
  final def splitSequenceWhen[T](
    f: T => Boolean,
    iterable: Iterable[T],
    rightmost: Boolean
  ): Option[(Vector[T], T, Vector[T])] = {
    @tailrec
    def split(left: Vector[T], right: Iterator[T]): Option[(Vector[T], T, Vector[T])] =
      if (right.isEmpty) None
      else {
        val head = right.next()
        if (f(head)) Some((left, head, right.toVector))
        else split(left :+ head, right)
      }

    if (iterable.isEmpty) None
    else if (rightmost) splitSequenceWhenLast(f, iterable)
    else split(Vector.empty, iterable.iterator)
  }

  /** Optionally splits sequence into left and right part around rightmost matching element. */
  final private def splitSequenceWhenLast[T](
    f: T => Boolean,
    iterable: Iterable[T]
  ): Option[(Vector[T], T, Vector[T])] = {
    @tailrec
    def split(
      left: Vector[T],
      iterator: Iterator[T],
      right: Vector[T],
      result: Option[(Vector[T], T)]
    ): Option[(Vector[T], T, Vector[T])] =
      if (iterator.isEmpty) result.map { case (left, head) => (left, head, right) }
      else {
        val head = iterator.next()
        if (f(head)) split(left :+ head, iterator, Vector.empty, Some((left, head)))
        else split(left :+ head, iterator, if (result.isDefined) right :+ head else right, result)
      }
    split(Vector.empty, iterable.iterator, Vector.empty, None)
  }

  /** Joins single treeSplit back into a tree node. */
  @`inline` final def join[T](split: TreeSplit[T]): Tree[T] = Tree(split._2, split._1 ++ split._3)

  /** Updates a value of a child, and builds a tree back from the treeSplit. */
  final def updateChildValueInSplit[T, T1 >: T: ClassTag](
    treeSplit: Vector[TreeSplit[T]],
    existingChild: Tree[T],
    replacementValue: T1,
    keepDistinct: Boolean
  ): Tree[T1] = {
    val modifiedChild = Tree(replacementValue, existingChild.children)
    if (keepDistinct && treeSplit.nonEmpty) {
      val (left, head, right) = treeSplit.head
      val newHead = insertChildDistinct(head, left, modifiedChild, right, preserveExisting = false)
      TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
    } else {
      TreeBuilder.fromChildAndTreeSplit(modifiedChild, treeSplit)
    }
  }

  /** Updates a child, and builds a tree back from the treeSplit.
    * @param existingChild existing child
    * @param replacementChild replacement child
    */
  final def updateChildInSplit[T, T1 >: T: ClassTag](
    treeSplit: Vector[TreeSplit[T]],
    existingChild: Tree[T],
    replacementChild: Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    replacementChild match {
      case Tree.empty =>
        TreeBuilder.fromTreeSplit[T1](treeSplit)

      case tree: Tree[T1] =>
        if (keepDistinct && treeSplit.nonEmpty && tree.head != existingChild.head) {
          val (left, head, right) = treeSplit.head
          val newHead = insertChildDistinct(head, left, tree, right, preserveExisting = false)
          TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
        } else {
          TreeBuilder.fromChildAndTreeSplit(tree, treeSplit)
        }
    }

  /** Updates value of the child holding the value. */
  final def updateChildValue[T, T1 >: T: ClassTag](
    tree: Tree[T],
    value: T1,
    replacement: T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
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
    tree: Tree[T],
    pathIterator: Iterator[T1],
    replacement: T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates value of the node selected by the path using a path item extractor. */
  final def updateValueAt[K, T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    replacement: T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree.head)
            Right(updateChildValueInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates the child tree holding the value at head. */
  final def updateChild[T, T1 >: T: ClassTag](
    tree: Tree[T],
    value: T1,
    replacement: Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
      case Some((left, node, right)) if replacement != node =>
        replacement match {
          case Tree.empty =>
            Tree(tree.head, left ++ right)

          case t: Tree.NodeTree[T1] =>
            if (keepDistinct && t.head != node.head) {
              insertChildDistinct(tree.head, left, t, right, preserveExisting = false)
            } else {
              Tree(tree.head, left ++: t +: right)
            }

          case t: Tree.ArrayTree[T1] =>
            ArrayTree
              .insertChildren(ArrayTree.prepend(tree.head, t), left, right, keepDistinct && t.head != node.head)
        }

      case _ => tree
    }

  /** Updates a subtree selected by the path. */
  final def updateTreeAt[T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    replacement: Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Updates a subtree selected by the path using path item extractor. */
  final def updateTreeAt[K, T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    replacement: Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node holding the value. */
  final def modifyChildValue[T, T1 >: T: ClassTag](
    tree: Tree[T],
    value: T1,
    modify: T => T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
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
    tree: Tree[T],
    pathIterator: Iterator[T1],
    modify: T => T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
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
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: T => T1,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
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
    tree: Tree[T],
    value: T1,
    modify: Tree[T] => Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
      case None => tree
      case Some((left, node, right)) =>
        val replacement = modify(node)
        if (replacement == node) tree
        else
          replacement match {
            case Tree.empty =>
              Tree(tree.head, left ++ right)

            case t: Tree.NodeTree[T1] =>
              if (keepDistinct && t.head != node.head) {
                insertChildDistinct(tree.head, left, t, right, preserveExisting = false)
              } else {
                Tree(tree.head, left ++: t +: right)
              }

            case t: Tree.ArrayTree[T1] =>
              ArrayTree
                .insertChildren(ArrayTree.prepend(tree.head, t), left, right, keepDistinct && t.head != node.head)
          }
    }

  /** Modifies a subtree selected by the path. */
  final def modifyTreeAt[T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    modify: Tree[T] => Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
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
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: Tree[T] => Tree[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          val replacement = modify(recipientTree)
          if (replacement != recipientTree)
            Right(updateChildInSplit(treeSplit, recipientTree, replacement, keepDistinct))
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies children of the node selected by the path. */
  final def modifyChildrenAt[T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          val replacementChildren = modify(recipientTree.children)
          if (replacementChildren != recipientTree.children)
            Right(
              updateChildInSplit(treeSplit, recipientTree, Tree(recipientTree.head, replacementChildren), keepDistinct)
            )
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Modifies children of the node selected by the path using path item extractor. */
  final def modifyChildrenAt[K, T, T1 >: T: ClassTag](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          val replacementChildren = modify(recipientTree.children)
          if (replacementChildren != recipientTree.children)
            Right(
              updateChildInSplit(treeSplit, recipientTree, Tree(recipientTree.head, replacementChildren), keepDistinct)
            )
          else Right(tree)
      }
      .getOrElse(Left(tree))

  /** Removes the child node and inserts its children into the treeSplit. */
  final def removeChildValueFromSplit[T](
    tree: Tree[T],
    treeSplit: Vector[TreeSplit[T]],
    child: Tree[T],
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
    tree: Tree[T],
    value: T1,
    keepDistinct: Boolean,
    rightmost: Boolean
  ): Tree[T] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
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
    tree: Tree[T],
    pathIterator: Iterator[T1],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
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
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, recipientTree) =>
          removeChildValueFromSplit(tree, treeSplit, recipientTree, keepDistinct)
      }
      .getOrElse(tree)

  /** Removes the direct child holding the value. */
  final def removeChild[T, T1 >: T](
    tree: Tree[T],
    value: T1,
    rightmost: Boolean
  ): Tree[T] =
    splitSequenceWhen[Tree[T]](_.head == value, tree.children, rightmost) match {
      case None                   => tree
      case Some((left, _, right)) => Tree(tree.head, left ++ right)
    }

  /** Removes the tree selected by the path. */
  final def removeTreeAt[T, T1 >: T](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    rightmost: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
      .map { case (treeSplit, _) => TreeBuilder.fromTreeSplit(treeSplit) }
      .getOrElse(tree)

  /** Removes the tree selected by the path using an extractor function. */
  final def removeTreeAt[K, T](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    rightmost: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map { case (treeSplit, _) => TreeBuilder.fromTreeSplit(treeSplit) }
      .getOrElse(tree)

  /** Removes children of the tree selected by the path. */
  final def removeChildrenAt[T, T1 >: T](
    tree: Tree[T],
    pathIterator: Iterator[T1],
    rightmost: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath[T, T1](tree, pathIterator, rightmost)
      .map {
        case (treeSplit, selectedTree) =>
          TreeBuilder.fromChildAndTreeSplit(Tree.Leaf(selectedTree.head), treeSplit)
      }
      .getOrElse(tree)

  /** Removes children of the tree selected by the path using an extractor function. */
  final def removeChildrenAt[K, T](
    tree: Tree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    rightmost: Boolean
  ): Tree[T] =
    splitTreeFollowingEntirePath(tree, pathIterator, toPathItem, rightmost)
      .map {
        case (treeSplit, selectedTree) =>
          TreeBuilder.fromChildAndTreeSplit(Tree.Leaf(selectedTree.head), treeSplit)
      }
      .getOrElse(tree)

}
