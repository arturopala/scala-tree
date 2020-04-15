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

package com.github.arturopala.tree.util

import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.Tree.{Binary, Leaf, NodeTree, Unary}

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Collection of operations on the hierarchical, node-based, representation of the tree. */
object NodeTree {

  final object Node {
    def unapply[T](node: NodeTree[T]): Option[(T, List[NodeTree[T]])] =
      Some((node.value, node.subtrees))
  }

  final object NonEmptySubtree {
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

  /** Returns lazy node stream, goes depth-first */
  final def valueStream[T](pred: T => Boolean, node: NodeTree[T]): Stream[T] = valueStream(pred, node, Nil)

  private def valueStream[T](pred: T => Boolean, node: NodeTree[T], queue: List[NodeTree[T]]): Stream[T] = {
    def continue: Stream[T] = node match {
      case NonEmptySubtree(_, x, xs) =>
        valueStream(pred, x, xs ::: queue)

      case _ =>
        queue match {
          case y :: ys => valueStream(pred, y, ys)
          case Nil     => Stream.empty
        }
    }
    if (pred(node.value)) Stream.cons(node.value, continue) else continue
  }

  @tailrec
  final def select[T, K, R](
    node: NodeTree[T],
    path: Iterable[K],
    extract: T => K,
    result: NodeTree[T] => R
  ): Option[R] =
    if (path.isEmpty || (path.nonEmpty && path.head != extract(node.value))) None
    else if (path.tail.isEmpty) {
      if (path.head == extract(node.value)) Some(result(node)) else None
    } else {
      val nextOpt = node.subtrees.collect {
        case nextNode if path.tail.head == extract(nextNode.value) => nextNode
      }.lastOption
      if (nextOpt.isEmpty) None
      else select(nextOpt.get, path.tail, extract, result)
    }

  @`inline` final def containsBranch[T, T1 >: T](node: NodeTree[T], branch: Iterable[T1]): Boolean =
    contains(node, branch, fullMatch = true)

  @`inline` final def containsPath[T, T1 >: T](node: NodeTree[T], path: Iterable[T1]): Boolean =
    contains(node, path, fullMatch = false)

  @tailrec
  final def contains[T, T1 >: T](node: NodeTree[T], branch: Iterable[T1], fullMatch: Boolean): Boolean =
    if (branch.isEmpty || (branch.nonEmpty && branch.head != node.value)) false
    else if (branch.tail.isEmpty) (!fullMatch || node.isLeaf) && branch.head == node.value
    else {
      val nextOpt = node.subtrees.collect {
        case nextNode if nextNode.value == branch.tail.head => nextNode
      }.lastOption
      if (nextOpt.isEmpty) false
      else contains(nextOpt.get, branch.tail, fullMatch)
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

  /** Returns lazy subtree stream, goes depth-first */
  final def treeStream[T](pred: Tree[T] => Boolean, node: NodeTree[T]): Stream[Tree[T]] = treeStream(pred, node, Nil)

  private def treeStream[T](pred: Tree[T] => Boolean, node: NodeTree[T], queue: List[NodeTree[T]]): Stream[Tree[T]] = {
    def continue: Stream[Tree[T]] = node match {
      case NonEmptySubtree(_, x, xs) =>
        treeStream(pred, x, xs ::: queue)

      case _ =>
        queue match {
          case y :: ys => treeStream(pred, y, ys)
          case Nil     => Stream.empty
        }
    }
    if (pred(node)) Stream.cons(node, continue) else continue
  }

  /** Returns an iterator over branches of the tree */
  final def branchIterator[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Iterator[Iterable[T]] =
    new Iterator[Iterable[T]] {

      type Queue = List[(Vector[T], NodeTree[T])]
      var queue: Queue = seekNext(List((Vector.empty, node)))

      override def hasNext: Boolean = queue.nonEmpty

      @tailrec
      override def next(): Iterable[T] = queue match {
        case Nil => throw new NoSuchElementException()
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = acc :+ value
          queue = seekNext(subtrees.map((branch, _)) ::: xs)
          subtrees match {
            case Nil if pred(branch) => branch
            case _                   => next()
          }
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

  final def branchStream[T](pred: Iterable[T] => Boolean, node: NodeTree[T]): Stream[Iterable[T]] =
    branchStream(pred, node, Vector.empty, Nil)

  private def branchStream[T](
    pred: Iterable[T] => Boolean,
    node: NodeTree[T],
    acc: Vector[T],
    queue: List[(Vector[T], NodeTree[T])]
  ): Stream[Vector[T]] =
    node match {
      case NonEmptySubtree(value, x, Nil) => branchStream(pred, x, acc :+ value, queue)
      case NonEmptySubtree(value, x, xs)  => branchStream(pred, x, acc :+ value, (acc, Tree(value, xs)) :: queue)
      case Node(value, Nil) =>
        val branch = acc :+ value
        def continue: Stream[Vector[T]] = queue match {
          case (acc2, y) :: ys => branchStream(pred, y, acc2, ys)
          case Nil             => Stream.empty
        }
        if (pred(branch)) Stream.cons(branch, continue)
        else continue

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
          TreeBuilder.fromList(branchHead :: branchTailIterator.toList).asInstanceOf[NodeTree[T1]]
        branchTree :: subtreesLeft

      case head :: tail if head.value == branchHead =>
        val modified = insertBranchUnsafe(head, branchTailIterator)
        subtreesLeft.reverse ::: (modified :: tail)

      case head :: tail =>
        insertBranchInSubtrees(branchHead, head :: subtreesLeft, tail, branchTailIterator)
    }

  final def insertBranch[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): Option[NodeTree[T1]] =
    insertBranch(tree, branchIterator, Nil)

  @tailrec
  private def insertBranch[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    branchIterator: Iterator[T1],
    queue: List[(T, List[NodeTree[T]], List[NodeTree[T]])]
  ): Option[NodeTree[T1]] =
    if (branchIterator.hasNext) {
      val value = branchIterator.next()
      splitListWhen[NodeTree[T]](_.value == value, tree.subtrees) match {
        case None =>
          val branchTree: NodeTree[T1] =
            TreeBuilder.fromList(value :: branchIterator.toList).asInstanceOf[NodeTree[T1]]
          val newNode: NodeTree[T] = Tree(tree.value, branchTree :: tree.subtrees).asInstanceOf[NodeTree[T]]
          Some(queue.foldLeft(newNode) { case (n, (v, l, r)) => Tree(v, l ::: (n :: r)) })

        case Some((left, node, right)) =>
          insertBranch(node, branchIterator, (tree.value, left, right) :: queue)
      }
    } else None

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

}
