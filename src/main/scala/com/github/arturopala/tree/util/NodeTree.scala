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

import com.github.arturopala.tree.Tree
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

  /** Returns an iterator over the nodes of the tree, goes depth-first. */
  final def valueIterator[T](pred: T => Boolean, node: NodeTree[T]): Iterator[T] = new Iterator[T] {

    type Queue = List[NodeTree[T]]
    var queue: Queue = seekNext(List(node))

    override def hasNext: Boolean = queue.nonEmpty

    @tailrec
    override def next(): T = queue match {
      case Nil => throw new NoSuchElementException()
      case Node(value, subtrees) :: xs =>
        queue = seekNext(subtrees ::: xs)
        if (pred(value)) value else next()
    }

    @tailrec
    private def seekNext(q: Queue): Queue = q match {
      case Nil => q
      case Node(value, subtrees) :: xs =>
        if (pred(value)) q
        else seekNext(subtrees ::: xs)
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
  final def selectTree[T, T1 >: T](node: NodeTree[T], path: Iterable[T1]): Option[NodeTree[T]] =
    if (path.isEmpty || (path.nonEmpty && path.head != node.value)) None
    else if (path.tail.isEmpty) {
      if (path.head == node.value) Some(node) else None
    } else {
      val nextOpt = node.subtrees
        .collectFirst {
          case nextNode if nextNode.value == path.tail.head => nextNode
        }
      if (nextOpt.isEmpty) None
      else selectTree(nextOpt.get, path.tail)
    }

  final def containsBranch[T, T1 >: T](node: NodeTree[T], branch: Iterable[T1]): Boolean =
    contains(node, branch, fullMatch = true)

  final def containsPath[T, T1 >: T](node: NodeTree[T], path: Iterable[T1]): Boolean =
    contains(node, path, fullMatch = false)

  @tailrec
  private def contains[T, T1 >: T](node: NodeTree[T], branch: Iterable[T1], fullMatch: Boolean): Boolean =
    if (branch.isEmpty || (branch.nonEmpty && branch.head != node.value)) false
    else if (branch.tail.isEmpty) (!fullMatch || node.isLeaf) && branch.head == node.value
    else {
      val nextOpt = node.subtrees
        .collectFirst {
          case nextNode if nextNode.value == branch.tail.head => nextNode
        }
      if (nextOpt.isEmpty) false
      else contains(nextOpt.get, branch.tail, fullMatch)
    }

  /** Returns an iterator over (sub)trees of the tree, goes depth-first. */
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
    branches(pred, Nil, node.subtrees.map((List(node.value), _)))

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

  final def insert[T, T1 >: T](tree: NodeTree[T], branch: List[T1]): NodeTree[T1] =
    branch match {
      case x :: xs =>
        tree.subtrees.partition(_.value == x) match {

          case (Nil, bs) =>
            val c = insert(Tree(x), xs)
            Tree(tree.value, c :: bs)

          case (as, bs) =>
            as match {

              case a :: Nil =>
                val c = insert(a, xs)
                Tree(tree.value, c :: bs)

              case _ =>
                val cs = as.map(insert(_, xs))
                Tree(tree.value, cs ::: bs)
            }
        }

      case Nil => tree
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
    mkStringUsingBranches(
      show,
      valueSeparator,
      branchSeparator,
      branchEnd,
      maxDepth,
      new StringBuilder(branchStart),
      List((0, branchStart, node)),
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
          if (level > 0) builder.append(valueSeparator)
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
              subtrees.map((level + 1, prefix + (if (level > 0) valueSeparator else "") + string, _)) ::: xs,
              newBranch = false
            )
        }
    }

}
