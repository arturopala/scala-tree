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

import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.Tree.{ArrayTree, Binary, Leaf, NodeTree, Unary}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

/** Collection of operations on the hierarchical, node-based, representation of the tree. */
object NodeTree {

  object Node {
    def unapply[T](node: NodeTree[T]): Option[(T, List[NodeTree[T]])] =
      Some((node.value, node.subtrees))
  }

  object NonEmptySubtree {
    def unapply[T](node: NodeTree[T]): Option[(T, NodeTree[T], List[NodeTree[T]])] = node match {
      case _: Leaf[T]      => None
      case node: Unary[T]  => Some((node.value, node.subtree, Nil))
      case node: Binary[T] => Some((node.value, node.left, node.right :: Nil))
      case _               => Some((node.value, node.subtrees.head, node.subtrees.tail))
    }
  }

  object Leaf {
    def unapply[T](node: Leaf[T]): Option[T] = Some(node.value)
  }

  object Unary {
    def unapply[T](node: Unary[T]): Option[(T, NodeTree[T])] = Some((node.value, node.subtree))
  }

  object Binary {
    def unapply[T](node: Binary[T]): Option[(T, NodeTree[T], NodeTree[T])] = Some((node.value, node.left, node.right))
  }

  type TreeSplit[T] = (List[NodeTree[T]], T, List[NodeTree[T]])

  /** Returns an iterator over the filtered nodes of the tree, goes depth-first with maxDepth limit. */
  final def valueIterator[T](pred: T => Boolean, node: NodeTree[T]): Iterator[T] = new Iterator[T] {

    type Queue = List[(Int, NodeTree[T])]
    var queue: Queue = seekNext(List((1, node)))

    override def hasNext: Boolean = queue.nonEmpty

    @tailrec
    override def next(): T = queue match {
      case Nil => throw new NoSuchElementException()
      case (level, Node(value, subtrees)) :: xs =>
        queue = seekNext(subtrees.map((level + 1, _)) ::: xs)
        if (pred(value)) value else next()
    }

    @tailrec
    private def seekNext(q: Queue): Queue = q match {
      case Nil => q
      case (level, Node(value, subtrees)) :: xs =>
        if (pred(value)) q
        else seekNext(subtrees.map((level + 1, _)) ::: xs)
    }
  }

  /** Returns an iterator over the filtered nodes of the tree, goes depth-first with maxDepth limit. */
  final def valueIteratorWithLimit[T](pred: T => Boolean, node: NodeTree[T], maxDepth: Int): Iterator[T] =
    new Iterator[T] {

      type Queue = List[(Int, NodeTree[T])]
      var queue: Queue =
        if (maxDepth > 0) seekNext(List((1, node)))
        else Nil

      override def hasNext: Boolean = queue.nonEmpty

      @tailrec
      override def next(): T = queue match {
        case Nil => throw new NoSuchElementException()
        case (level, Node(value, subtrees)) :: xs =>
          queue =
            if (level < maxDepth) seekNext(subtrees.map((level + 1, _)) ::: xs)
            else seekNext(xs)

          if (pred(value)) value else next()
      }

      @tailrec
      private def seekNext(q: Queue): Queue = q match {
        case Nil => q
        case (level, Node(value, subtrees)) :: xs =>
          if (pred(value)) q
          else if (level < maxDepth) seekNext(subtrees.map((level + 1, _)) ::: xs)
          else seekNext(xs)
      }
    }

  final def values[T](pred: T => Boolean, node: NodeTree[T]): List[T] = values(pred, Nil, List(node))

  @tailrec
  private def values[T](pred: T => Boolean, result: List[T], queue: List[NodeTree[T]]): List[T] =
    queue match {
      case Nil => result.reverse
      case Node(value, subtrees) :: xs =>
        if (pred(value)) values(pred, value :: result, subtrees ::: xs)
        else values(pred, result, subtrees ::: xs)
    }

  @tailrec
  final def select[T, T1 >: T, R](
    node: NodeTree[T],
    path: Iterable[T1],
    result: NodeTree[T] => R
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != node.value)) None
    else if (path.tail.isEmpty) {
      if (path.head == node.value) Some(result(node)) else None
    } else {
      val nextOpt = node.subtrees.collect {
        case nextNode if path.tail.head == nextNode.value => nextNode
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
    if (path.isEmpty || (path.nonEmpty && path.head != toPathItem(node.value))) None
    else if (path.tail.isEmpty) {
      if (path.head == toPathItem(node.value)) Some(toResult(node)) else None
    } else {
      val nextOpt = node.subtrees.collect {
        case nextNode if path.tail.head == toPathItem(nextNode.value) => nextNode
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
    if (path.isEmpty || (path.nonEmpty && path.head != node.value)) false
    else if (path.tail.isEmpty) (!requiresFullMatch || node.isLeaf) && path.head == node.value
    else {
      val nextOpt = node.subtrees.collect {
        case nextNode if nextNode.value == path.tail.head => nextNode
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
    if (path.isEmpty || (path.nonEmpty && path.head != toPathItem(node.value))) false
    else if (path.tail.isEmpty) (!requiresFullMatch || node.isLeaf) && path.head == toPathItem(node.value)
    else {
      val nextOpt = node.subtrees.collect {
        case nextNode if toPathItem(nextNode.value) == path.tail.head => nextNode
      }.lastOption
      if (nextOpt.isEmpty) false
      else contains(nextOpt.get, path.tail, requiresFullMatch, toPathItem)
    }

  /** Returns an iterator over filtered (sub)trees of the tree, goes depth-first. */
  final def treeIterator[T](pred: Tree[T] => Boolean, node: NodeTree[T]): Iterator[Tree[T]] = new Iterator[Tree[T]] {

    type Queue = List[NodeTree[T]]
    var queue: Queue = seekNext(List(node))

    override def hasNext: Boolean = queue.nonEmpty

    @tailrec
    override def next(): Tree[T] = queue match {
      case Nil => throw new NoSuchElementException()
      case (node @ Node(_, subtrees)) :: xs =>
        queue = seekNext(subtrees ::: xs)
        if (pred(node)) node else next()
    }

    @tailrec
    private def seekNext(q: Queue): Queue = q match {
      case Nil => q
      case (node @ Node(_, subtrees)) :: xs =>
        if (pred(node)) q
        else seekNext(subtrees ::: xs)
    }
  }

  /** Returns an iterator over filtered (sub)trees of the tree with depth limit, goes depth-first. */
  final def treeIteratorWithLimit[T](pred: Tree[T] => Boolean, node: NodeTree[T], maxDepth: Int): Iterator[Tree[T]] =
    new Iterator[Tree[T]] {

      type Queue = List[(Int, NodeTree[T])]
      var queue: Queue = if (maxDepth > 0) seekNext(List((1, node))) else Nil

      override def hasNext: Boolean = queue.nonEmpty

      @tailrec
      override def next(): Tree[T] = queue match {
        case Nil => throw new NoSuchElementException()
        case (level, node @ Node(_, subtrees)) :: xs =>
          queue =
            if (level < maxDepth) seekNext(subtrees.map((level + 1, _)) ::: xs)
            else seekNext(xs)
          if (pred(node)) node else next()
      }

      @tailrec
      private def seekNext(q: Queue): Queue = q match {
        case Nil => q
        case (level, node @ Node(_, subtrees)) :: xs =>
          if (pred(node)) q
          else if (level < maxDepth) seekNext(subtrees.map((level + 1, _)) ::: xs)
          else seekNext(xs)
      }
    }

  final def trees[T](pred: Tree[T] => Boolean, node: NodeTree[T]): List[NodeTree[T]] = trees(pred, Nil, List(node))

  @tailrec
  private def trees[T](
    pred: Tree[T] => Boolean,
    result: List[NodeTree[T]],
    queue: List[NodeTree[T]]
  ): List[NodeTree[T]] =
    queue match {
      case Nil => result.reverse
      case (node @ Node(_, subtrees)) :: xs =>
        if (pred(node)) trees(pred, node :: result, subtrees ::: xs)
        else trees(pred, result, subtrees ::: xs)
    }

  /** Returns an iterator over filtered branches of the tree. */
  final def branchIterator[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = List[(Vector[T], NodeTree[T])]
      var queue: Queue = seekNext(List((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] = queue match {
        case Nil => throw new NoSuchElementException()
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = acc :+ value
          queue = seekNext(subtrees.map((branch, _)) ::: xs)
          branch
      }

      @tailrec
      private def seekNext(q: Queue): Queue = q match {
        case Nil => q
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = acc :+ value
          subtrees match {
            case Nil if pred(branch) => q
            case _                   => seekNext(subtrees.map((branch, _)) ::: xs)
          }
      }
    }

  /** Returns an iterator over filtered branches of the tree with depth limit. */
  final def branchIteratorWithLimit[T](
    pred: Iterable[T] => Boolean,
    node: NodeTree[T],
    maxDepth: Int
  ): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = List[(Vector[T], NodeTree[T])]
      var queue: Queue =
        if (maxDepth > 0) seekNext(List((Vector.empty, node)))
        else Nil

      override def hasNext: Boolean = queue.nonEmpty

      override def next(): Iterable[T] = queue match {
        case Nil => throw new NoSuchElementException()
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = acc :+ value
          queue =
            if (branch.length < maxDepth) seekNext(subtrees.map((branch, _)) ::: xs)
            else seekNext(xs)
          branch
      }

      @tailrec
      private def seekNext(q: Queue): Queue = q match {
        case Nil => q
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = acc :+ value
          subtrees match {
            case s if (s.isEmpty || branch.length >= maxDepth) && pred(branch) => q
            case _ =>
              if (branch.length < maxDepth) seekNext(subtrees.map((branch, _)) ::: xs)
              else seekNext(xs)
          }
      }
    }

  final def branches[T](pred: List[T] => Boolean, node: NodeTree[T]): List[List[T]] =
    branches(pred, Nil, List((Nil, node)))

  @tailrec
  private def branches[T](
    pred: List[T] => Boolean,
    result: List[List[T]],
    queue: List[(List[T], NodeTree[T])]
  ): List[List[T]] =
    queue match {
      case Nil => result.reverse
      case (acc, Node(value, subtrees)) :: xs =>
        val branch = value :: acc
        subtrees match {
          case Nil if pred(branch) => branches(pred, branch.reverse :: result, xs)
          case _                   => branches(pred, result, subtrees.map((branch, _)) ::: xs)
        }
    }

  final def countBranches[T](pred: List[T] => Boolean, node: NodeTree[T]): Int =
    countBranches(pred, 0, List((Nil, node)))

  @tailrec
  private def countBranches[T](pred: List[T] => Boolean, result: Int, queue: List[(List[T], NodeTree[T])]): Int =
    queue match {
      case Nil => result
      case (acc, Node(value, subtrees)) :: xs =>
        val branch = value :: acc
        subtrees match {
          case Nil if pred(branch) => countBranches(pred, 1 + result, xs)
          case _                   => countBranches(pred, result, subtrees.map((branch, _)) ::: xs)
        }
    }

  final def insertBranchUnsafe[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): NodeTree[T1] =
    if (branchIterator.hasNext) {
      Tree(tree.value, insertBranchInSubtrees(branchIterator.next, Nil, tree.subtrees, branchIterator))
    } else tree

  @tailrec
  private def insertBranchInSubtrees[T, T1 >: T: ClassTag](
    branchHead: T1,
    subtreesLeft: List[NodeTree[T]],
    subtreesRight: List[NodeTree[T]],
    branchTailIterator: Iterator[T1]
  ): List[NodeTree[T1]] =
    subtreesRight match {
      case Nil =>
        val branchTree: NodeTree[T1] =
          TreeBuilder.linearTreeFromSequence(branchHead :: branchTailIterator.toList).asInstanceOf[NodeTree[T1]]
        branchTree :: subtreesLeft

      case head :: tail if head.value == branchHead =>
        val modified = insertBranchUnsafe(head, branchTailIterator)
        subtreesLeft.reverse ::: (modified :: tail)

      case head :: tail =>
        insertBranchInSubtrees(branchHead, head :: subtreesLeft, tail, branchTailIterator)
    }

  final def insertBranch[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): Option[Tree[T1]] =
    splitTreeFollowingPath(tree, branchIterator).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: NodeTree[T1] =
          TreeBuilder.linearTreeFromSequence(value :: remainingBranchIterator.toList).asInstanceOf[NodeTree[T1]]

        val newNode = Tree(remainingTree.value, branchTree :: remainingTree.subtrees)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case _ => None
    }

  final def toPairsList[T](node: NodeTree[T]): List[(Int, T)] = toPairsList(Nil, List(node))

  @tailrec
  private def toPairsList[T](result: List[(Int, T)], queue: List[NodeTree[T]]): List[(Int, T)] =
    queue match {
      case Nil                         => result
      case Leaf(value) :: xs           => toPairsList((0, value) :: result, xs)
      case Node(value, subtrees) :: xs => toPairsList((subtrees.size, value) :: result, subtrees ::: xs)
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
        case Leaf(value) =>
          structure.update(position, 0)
          values.update(position, value)
          toArrays(structure, values, queue, position - 1, queuePosition - 1)

        case Node(value, subtrees) =>
          structure.update(position, subtrees.size)
          values.update(position, value)
          var rp: Int = queuePosition + subtrees.size - 1
          subtrees.foreach { subtree =>
            queue(rp) = subtree
            rp = rp - 1
          }
          toArrays(structure, values, queue, position - 1, queuePosition + subtrees.size - 1)
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

        case Node(_, subtrees) =>
          structure.update(position, subtrees.size)
          var rp: Int = queuePosition + subtrees.size - 1
          subtrees.foreach { subtree =>
            queue(rp) = subtree
            rp = rp - 1
          }
          toStructureArray(structure, queue, position - 1, queuePosition + subtrees.size - 1)
      }

  @tailrec
  final def toTreeList[T](
    result: List[(Int, Tree[T])],
    queue: List[NodeTree[T]]
  ): List[(Int, Tree[T])] =
    queue match {
      case Nil                         => result
      case Leaf(value) :: xs           => toTreeList((0, Tree(value)) :: result, xs)
      case Node(value, subtrees) :: xs => toTreeList((subtrees.size, Tree(value)) :: result, subtrees ::: xs)
    }

  @`inline` final def listMap[T, K](f: T => K, node: NodeTree[T]): List[(Int, K)] = listMap(f, Nil, List(node))

  @tailrec
  private final def listMap[T, K](f: T => K, result: List[(Int, K)], queue: List[NodeTree[T]]): List[(Int, K)] =
    queue match {
      case Nil                              => result
      case Leaf(value) :: xs                => listMap(f, (0, f(value)) :: result, xs)
      case Unary(value, subtree) :: xs      => listMap(f, (1, f(value)) :: result, subtree :: xs)
      case Binary(value, left, right) :: xs => listMap(f, (2, f(value)) :: result, left :: right :: xs)
      case Node(value, subtrees) :: xs      => listMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
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
        case Leaf(value) =>
          structure.update(position, 0)
          values.update(position, f(value))
          arrayMap(f, structure, values, queue, position - 1, queuePosition - 1)

        case Unary(value, subtree) =>
          structure.update(position, 1)
          values.update(position, f(value))
          queue(queuePosition) = subtree
          arrayMap(f, structure, values, queue, position - 1, queuePosition)

        case Binary(value, left, right) =>
          structure.update(position, 2)
          values.update(position, f(value))
          queue(queuePosition) = right
          queue(queuePosition + 1) = left
          arrayMap(f, structure, values, queue, position - 1, queuePosition + 1)

        case Node(value, subtrees) =>
          structure.update(position, subtrees.size)
          values.update(position, f(value))
          var rp: Int = queuePosition + subtrees.size - 1
          subtrees.foreach { subtree =>
            queue(rp) = subtree
            rp = rp - 1
          }
          arrayMap(f, structure, values, queue, position - 1, queuePosition + subtrees.size - 1)
      }

  @tailrec
  final def listFlatMap[T, K](
    f: T => Tree[K],
    result: List[(Int, Tree[K])],
    queue: List[NodeTree[T]]
  ): List[(Int, Tree[K])] =
    queue match {
      case Nil                         => result
      case Leaf(value) :: xs           => listFlatMap(f, (0, f(value)) :: result, xs)
      case Node(value, subtrees) :: xs => listFlatMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
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
        List((1, branchStart, node)),
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
    queue: List[(Int, String, NodeTree[T])],
    newBranch: Boolean
  ): StringBuilder =
    queue match {
      case Nil => builder
      case (level, prefix, Node(value, subtrees)) :: xs =>
        val string = show(value)
        if (level <= maxDepth) {
          if (newBranch) builder.append(branchSeparator).append(prefix)
          if (level > 1) builder.append(valueSeparator)
          builder.append(string)
        }
        val subtrees2 = if (level >= maxDepth) Nil else subtrees
        subtrees2 match {
          case Nil =>
            mkStringUsingBranches(
              show,
              valueSeparator,
              branchSeparator,
              branchEnd,
              maxDepth,
              builder.append(branchEnd),
              xs,
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
              subtrees.map((level + 1, prefix + (if (level > 1) valueSeparator else "") + string, _)) ::: xs,
              newBranch = false
            )
        }
    }

  /** Inserts new child into a tree keeping children distinct. */
  final def insertTreeDistinct[T, T1 >: T](tree: NodeTree[T], newChild: NodeTree[T1]): Tree[T1] = {
    val (children, hasDuplicate) = insertNewChild(newChild, tree.subtrees, prepend = true)
    if (hasDuplicate) makeTreeDistinct(Tree(tree.value, children), maxLookupLevel = 1)
    else Tree(tree.value, children)
  }

  /** Inserts new child into existing children.
    * If child value exists already, inserts new child next to the duplicate,
    * otherwise, prepends or appends depending on the flag.
    * @param prepend if true, child is eventually prepended to the existing children,
    *                if false, child is eventually appended.
    * @return a tuple of (children, hasDuplicate)
    */
  private final def insertNewChild[T, T1 >: T](
    newChild: Tree.NodeTree[T1],
    children: List[Tree.NodeTree[T]],
    prepend: Boolean
  ): (List[Tree.NodeTree[T1]], Boolean) =
    splitListWhen[NodeTree[T]](_.value == newChild.value, children) match {
      case None =>
        (if (prepend) newChild +: children else children :+ newChild, false)

      case Some((left, node, right)) =>
        (left ::: (newChild :: node :: right), true)
    }

  /** Ensures that a child at an index position is distinct,
    * if not, then merges it with the nearest duplicate on the right side or left side.
    * The global order of children is preserved.
    */
  final def ensureChildDistinct[T](tree: NodeTree[T], index: Int): NodeTree[T] =
    tree.subtrees.drop(index) match {
      case Nil => tree
      case child :: tail =>
        insertChildDistinct(tree.value, tree.subtrees.take(index), child, tail)
    }

  /** Inserts new children distinct between left and right siblings. */
  final def insertChildrenDistinct[T](
    head: T,
    leftSiblings: List[NodeTree[T]],
    newChildren: List[NodeTree[T]],
    rightSiblings: List[NodeTree[T]]
  ): NodeTree[T] = {

    @tailrec
    def insert(
      queue: Iterator[NodeTree[T]],
      left: List[NodeTree[T]],
      right: List[NodeTree[T]]
    ): (List[NodeTree[T]], List[NodeTree[T]]) =
      if (queue.hasNext) {
        val newChild = queue.next
        val (newLeft, newRight) = insertDistinctBetweenSiblings(left, newChild, right)
        insert(queue, newLeft, newRight)
      } else (left, right)

    val (left, right) = insert(newChildren.iterator, leftSiblings, rightSiblings)
    Tree(head, left ::: right)
  }

  /** Inserts new child distinct between left and right siblings. */
  final def insertChildDistinct[T](
    head: T,
    leftSiblings: List[NodeTree[T]],
    newChild: NodeTree[T],
    rightSiblings: List[NodeTree[T]]
  ): NodeTree[T] = {
    val (left, right) = insertDistinctBetweenSiblings(leftSiblings, newChild, rightSiblings)
    Tree(head, left ::: right)
  }

  /** Inserts new child distinct between left and right siblings.
    * If distinct then appends to the left side,
    * otherwise merges with the nearest duplicate on the left (preferred) or right.
    */
  final def insertDistinctBetweenSiblings[T](
    leftSiblings: List[NodeTree[T]],
    newChild: NodeTree[T],
    rightSiblings: List[NodeTree[T]]
  ): (List[NodeTree[T]], List[NodeTree[T]]) =
    splitListWhen[NodeTree[T]](_.value == newChild.value, leftSiblings.reverse) match {
      case Some((right, duplicateOnLeft, left)) =>
        val newNode =
          insertChildrenDistinct(newChild.value, duplicateOnLeft.subtrees, newChild.subtrees, Nil)
        (left.reverse ::: newNode :: right.reverse, rightSiblings)

      case None =>
        splitListWhen[NodeTree[T]](_.value == newChild.value, rightSiblings) match {
          case None =>
            (leftSiblings :+ newChild, rightSiblings)

          case Some((left, duplicateOnRight, right)) =>
            val newNode =
              insertChildrenDistinct(newChild.value, newChild.subtrees, duplicateOnRight.subtrees, Nil)
            (leftSiblings :+ newNode, left ::: right)
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
        val groups = groupChildrenByValue(tree.subtrees)
        val newQueuePrefix = groups.map { group =>
          if (group.size == 1) (group.head, false, level + 1)
          else {
            val concatChildren = group.flatMap(_.subtrees).toList
            (Tree(group.head.value, concatChildren), concatChildren.size > 1, level + 1)
          }
        }
        makeTreeDistinct(
          newQueuePrefix ++: queue.tail,
          (newQueuePrefix.size, Tree(tree.value)) +: result,
          maxLookupLevel
        )
      } else {
        makeTreeDistinct(queue.tail, (0, tree) +: result, maxLookupLevel)
      }
    }

  /** Groups children by its value. */
  private final def groupChildrenByValue[T](seq: Seq[NodeTree[T]]): Vector[Vector[NodeTree[T]]] = {
    val map: mutable.Map[T, Int] = mutable.Map.empty
    val buffer = Buffer.empty[Vector[NodeTree[T]]]
    seq.foreach { tree =>
      val pos = map.getOrElseUpdate(tree.value, buffer.length)
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
    list: List[(Int, T, List[NodeTree[T1]])],
    result: List[NodeTree[T1]],
    prepend: Boolean = true
  ): List[NodeTree[T1]] =
    list match {
      case Nil => result
      case (numberOfChildrenToCollect, nodeValue, partialChildren) :: xs =>
        val node = Tree(
          nodeValue,
          if (prepend) partialChildren ::: result.take(numberOfChildrenToCollect)
          else result.take(numberOfChildrenToCollect) ::: partialChildren
        )
        buildTreeFromPartials(xs, node :: result.drop(numberOfChildrenToCollect), prepend)
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
        val newNode = Tree(remainingTree.value, branchTree :: remainingTree.subtrees)
        Some(TreeBuilder.fromChildAndTreeSplit(newNode, treeSplit))

      case (treeSplit, None, _, remainingTree) =>
        val newNode =
          if (keepDistinct && !remainingTree.isLeaf)
            remainingTree.insertTree(nodeToInsert).asInstanceOf[NodeTree[T1]]
          else Tree(remainingTree.value, nodeToInsert :: remainingTree.subtrees)
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
              recipientTree.insertTree(nodeToInsert).asInstanceOf[NodeTree[T1]]
            else Tree(recipientTree.value, nodeToInsert :: recipientTree.subtrees)
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
  ): Option[(List[TreeSplit[T]], NodeTree[T])] =
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
  ): Option[(List[TreeSplit[T]], Option[T1], Iterator[T1], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (tree.value == head) Some(splitTreeFollowingPath(tree, pathIterator, Nil)) else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, T1 >: T](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    queue: List[TreeSplit[T]]
  ): (List[TreeSplit[T]], Option[T1], Iterator[T1], NodeTree[T]) =
    if (pathIterator.hasNext) {
      val value: T1 = pathIterator.next()
      splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
        case None =>
          (queue, Some(value), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, (left, tree.value, right) :: queue)
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
  ): Option[(List[TreeSplit[T]], NodeTree[T])] =
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
  ): Option[(List[TreeSplit[T]], Option[K], Iterator[K], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (toPathItem(tree.value) == head) Some(splitTreeFollowingPath(tree, pathIterator, toPathItem, Nil))
      else None
    }

  @tailrec
  private def splitTreeFollowingPath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    queue: List[TreeSplit[T]]
  ): (List[TreeSplit[T]], Option[K], Iterator[K], NodeTree[T]) =
    if (pathIterator.hasNext) {
      val pathItem: K = pathIterator.next()
      splitListWhen[NodeTree[T]](node => toPathItem(node.value) == pathItem, tree.subtrees) match {
        case None =>
          (queue, Some(pathItem), pathIterator, tree)

        case Some((left, node, right)) =>
          splitTreeFollowingPath(node, pathIterator, toPathItem, (left, tree.value, right) :: queue)
      }
    } else {
      (queue, None, pathIterator, tree)
    }

  /** Optionally splits list into left and right part around matching element. */
  final def splitListWhen[T](f: T => Boolean, list: List[T]): Option[(List[T], T, List[T])] = {
    @tailrec
    def split(left: List[T], right: List[T]): Option[(List[T], T, List[T])] = right match {
      case Nil => None
      case head :: tail =>
        if (f(head)) Some((left.reverse, head, tail))
        else split(head :: left, tail)
    }
    split(Nil, list)
  }

  /** Joins single treeSplit back into a tree node. */
  @`inline` final def join[T](split: TreeSplit[T]): NodeTree[T] = Tree(split._2, split._1 ++ split._3)

  /** Modifies a value of a child, and builds a tree back from the treeSplit. */
  final def modifyChildValueInSplit[T, T1 >: T: ClassTag](
    treeSplit: List[TreeSplit[T]],
    child: NodeTree[T],
    modify: T => T1,
    keepDistinct: Boolean
  ): Tree[T1] = {
    val modifiedChild = Tree(modify(child.value), child.subtrees)
    if (keepDistinct && treeSplit.nonEmpty) {
      val (left, value, right) = treeSplit.head
      val newHead = insertChildDistinct(value, left, modifiedChild, right)
      TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
    } else {
      TreeBuilder.fromChildAndTreeSplit(modifiedChild, treeSplit)
    }
  }

  /** Modifies a child, and builds a tree back from the treeSplit. */
  final def modifyChildInSplit[T, T1 >: T: ClassTag](
    treeSplit: List[TreeSplit[T]],
    child: NodeTree[T],
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    modify(child) match {
      case Tree.empty =>
        TreeBuilder.fromTreeSplit[T1](treeSplit)

      case tree: NodeTree[T1] =>
        if (keepDistinct && treeSplit.nonEmpty) {
          val (left, value, right) = treeSplit.head
          val newHead = insertChildDistinct(value, left, tree, right)
          TreeBuilder.fromChildAndTreeSplit(newHead, treeSplit.tail)
        } else {
          TreeBuilder.fromChildAndTreeSplit(tree, treeSplit)
        }

      case tree: ArrayTree[T1] =>
        ArrayTree.buildFromChildAndTreeSplit(tree, treeSplit.tail)
    }

  /** Modifies value of the node holding the value. */
  final def modifyValue[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    modify: T => T1,
    keepDistinct: Boolean
  ): Tree[T1] =
    splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
      case None => tree
      case Some((left, node, right)) =>
        val modifiedNode = Tree(modify(node.value), node.subtrees)
        if (keepDistinct) {
          insertChildDistinct(tree.value, left, modifiedNode, right)
        } else {
          Tree(tree.value, left ::: modifiedNode :: right)
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
          Right(modifyChildValueInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node selected by the path using a path item extractor. */
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
          Right(modifyChildValueInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies the child tree holding the value at head. */
  final def modifyTree[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    value: T1,
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Tree[T1] =
    splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
      case None => tree
      case Some((left, node, right)) =>
        modify(node) match {
          case Tree.empty =>
            Tree(tree.value, left ::: right)

          case t: NodeTree[T1] =>
            if (keepDistinct) {
              insertChildDistinct(tree.value, left, t, right)
            } else {
              Tree(tree.value, left ::: t :: right)
            }

          case t: ArrayTree[T1] =>
            ArrayTree.insertChildren(ArrayTree.prepend(tree.value, t), left, right, keepDistinct)
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
          Right(modifyChildInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies a subtree selected by the path. */
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
          Right(modifyChildInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Removes the child node and inserts its children into the treeSplit. */
  final def removeChildValueFromSplit[T](
    tree: NodeTree[T],
    treeSplit: List[TreeSplit[T]],
    child: NodeTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    if (treeSplit.isEmpty) {
      if (child.isLeaf) Tree.empty
      else if (child.childrenCount == 1) child.children.head
      else tree
    } else if (child.size <= 1) TreeBuilder.fromTreeSplit(treeSplit)
    else {
      val (left, value, right) = treeSplit.head
      val newChild =
        if (keepDistinct)
          makeTreeDistinct(Tree(value, left ::: child.subtrees ::: right), maxLookupLevel = 1)
        else
          Tree(value, left ::: child.subtrees ::: right)

      TreeBuilder.fromChildAndTreeSplit(newChild, treeSplit.tail)
    }

  /** Removes the child holding the value and inserts its children into the tree.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * */
  final def removeValue[T, T1 >: T](
    tree: NodeTree[T],
    value: T1,
    keepDistinct: Boolean
  ): Tree[T] =
    splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
      case None => tree
      case Some((left, node, right)) =>
        if (keepDistinct && !node.isLeaf) {
          insertChildrenDistinct(tree.value, left, node.subtrees, right)
        } else {
          Tree(tree.value, left ::: node.subtrees ::: right)
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
  final def removeTree[T, T1 >: T](
    tree: NodeTree[T],
    value: T1
  ): Tree[T] =
    splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
      case None                   => tree
      case Some((left, _, right)) => Tree(tree.value, left ::: right)
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
