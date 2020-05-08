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
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

import scala.annotation.tailrec
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

  /** Returns lazy subtree stream, goes depth-first. */
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

  /** Inserts subtree into a tree keeping children distinct.
    * @param prepend if true, subtree is eventually prepended to the existing children,
    *                if false, subtree is appended.
    */
  final def insertTreeDistinct[T, T1 >: T](tree: NodeTree[T], subtree: NodeTree[T1], prepend: Boolean): Tree[T1] =
    mergeDistinct(Vector((tree, List(subtree))), Nil, prepend)

  /** Merges a new children into an existing tree.
    * Processes recursively a queue of tuples (tree, newChildren)
    * while accumulating result list of (numberOfChildrenToCollect, nodeValue, partialChildren).
    * When queue is finally empty builds a tree from the result list.
    *
    * @param prepend if true, new distinct children are eventually prepended to the existing children,
    *                if false, they are appended.
    */
  @tailrec
  final def mergeDistinct[T, T1 >: T](
    queue: Vector[(NodeTree[T], List[NodeTree[T1]])],
    result: List[(Int, T, List[NodeTree[T1]])],
    prepend: Boolean = true
  ): Tree[T1] =
    if (queue.isEmpty) buildTreeFromPartials(result, Nil, prepend).headOption.getOrElse(Tree.empty)
    else {
      val (tree, newSubtrees) = queue.head
      if (newSubtrees.isEmpty) mergeDistinct(queue.drop(1), (0, tree.value, tree.subtrees) :: result, prepend)
      else {
        val (queue2, distinctNewSubtrees) =
          tree.subtrees.reverse.foldLeft((queue.drop(1), newSubtrees)) {
            case ((acc, toInsert), node) =>
              toInsert.find(_.value == node.value) match {
                case None => ((node, Nil) +: acc, toInsert)
                case Some(t) =>
                  ((node, t.subtrees) +: acc, toInsert.filterNot(_.value == node.value))
              }
          }
        mergeDistinct(queue2, (queue2.size - queue.size + 1, tree.value, distinctNewSubtrees) :: result, prepend)
      }

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
        ).asInstanceOf[NodeTree[T1]]
        buildTreeFromPartials(xs, node :: result.drop(numberOfChildrenToCollect), prepend)
    }

  final def insertTreeAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    nodeToInsert: NodeTree[T1],
    keepDistinct: Boolean
  ): Option[Tree[T1]] =
    splitTreeByPath(tree, pathIterator).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: NodeTree[T1] =
          TreeBuilder
            .fromTreeList((Tree(value) :: remainingBranchIterator.map(Tree.apply[T1]).toList) :+ nodeToInsert)
            .asInstanceOf[NodeTree[T1]]
        val newNode = Tree(remainingTree.value, branchTree :: remainingTree.subtrees)
        Some(TreeBuilder.fromTreeSplitAndChild(newNode, treeSplit))

      case (treeSplit, None, _, remainingTree) =>
        val newNode =
          if (keepDistinct && !remainingTree.isLeaf)
            remainingTree.insertTreeDistinct(nodeToInsert).asInstanceOf[NodeTree[T1]]
          else Tree(remainingTree.value, nodeToInsert :: remainingTree.subtrees)
        Some(TreeBuilder.fromTreeSplitAndChild(newNode, treeSplit))
    }

  final def insertTreeAt[T, T1 >: T: ClassTag, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    nodeToInsert: NodeTree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeByFullPath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          val newNode =
            if (keepDistinct && !recipientTree.isLeaf)
              recipientTree.insertTreeDistinct(nodeToInsert).asInstanceOf[NodeTree[T1]]
            else Tree(recipientTree.value, nodeToInsert :: recipientTree.subtrees)
          Right(TreeBuilder.fromTreeSplitAndChild(newNode, treeSplit))
      }
      .getOrElse(Left(tree))

  /** Splits the tree following the path and succeeds only if all path items exists.
    * The tree children split is a triple of (children list left of value, a value, children list right of value)
    * @return some pair of (the tree split, recipient tree holding the last path item)
    *         or none if:
    *         1) the path doesn't exist in full,
    *         2) the path root doesn't match the tree root.
    */
  final def splitTreeByFullPath[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1]
  ): Option[(List[TreeSplit[T]], NodeTree[T])] =
    splitTreeByPath[T, T1](tree, pathIterator).flatMap {
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
  final def splitTreeByPath[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1]
  ): Option[(List[TreeSplit[T]], Option[T1], Iterator[T1], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (tree.value == head) Some(splitTreeByPath(tree, pathIterator, Nil)) else None
    }

  @tailrec
  private def splitTreeByPath[T, T1 >: T: ClassTag](
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
          splitTreeByPath(node, pathIterator, (left, tree.value, right) :: queue)
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
  final def splitTreeByFullPath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K
  ): Option[(List[TreeSplit[T]], NodeTree[T])] =
    splitTreeByPath(tree, pathIterator, toPathItem).flatMap {
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
  final def splitTreeByPath[T, K](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K
  ): Option[(List[TreeSplit[T]], Option[K], Iterator[K], NodeTree[T])] =
    if (pathIterator.isEmpty) None
    else {
      val head = pathIterator.next
      if (toPathItem(tree.value) == head) Some(splitTreeByPath(tree, pathIterator, toPathItem, Nil))
      else None
    }

  @tailrec
  private def splitTreeByPath[T, K](
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
          splitTreeByPath(node, pathIterator, toPathItem, (left, tree.value, right) :: queue)
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
  final def join[T](split: TreeSplit[T]): NodeTree[T] = Tree(split._2, split._1 ++ split._3)

  /** Modifies a value of a child, and builds a tree back from the treeSplit. */
  final def modifyChildValueInSplit[T, T1 >: T: ClassTag](
    treeSplit: List[TreeSplit[T]],
    child: NodeTree[T],
    modify: T => T1,
    keepDistinct: Boolean
  ): Tree[T1] = {
    val newValue = modify(child.value)
    val newChild = Tree(newValue, child.subtrees)
    if (keepDistinct && treeSplit.nonEmpty) {
      val onLeft = treeSplit.head._1.exists(_.value == newValue)
      val onRight = !onLeft && treeSplit.head._3.exists(_.value == newValue)
      if (onLeft || onRight) {
        val newHeadTree = insertTreeDistinct(join(treeSplit.head), newChild, prepend = onRight)
        TreeBuilder.fromTreeSplitAndChild(newHeadTree, treeSplit.tail)
      } else {
        TreeBuilder.fromTreeSplitAndChild(newChild, treeSplit)
      }
    } else {
      TreeBuilder.fromTreeSplitAndChild(newChild, treeSplit)
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
        TreeBuilder.fromTreeSplitAndChild(Tree.empty, treeSplit)
      case newChild =>
        val newValue = newChild.valueOption.get
        if (keepDistinct && treeSplit.nonEmpty) {
          val onLeft = treeSplit.head._1.exists(_.value == newValue)
          val onRight = !onLeft && treeSplit.head._3.exists(_.value == newValue)
          if (onLeft || onRight) {
            val newHeadTree = newChild match {
              case t: NodeTree[T1] =>
                insertTreeDistinct(join(treeSplit.head), t, prepend = onRight)
              case t =>
                join(treeSplit.head).insertTreeDistinct(t) // TODO: add support for prepend flag
            }
            TreeBuilder.fromTreeSplitAndChild(newHeadTree, treeSplit.tail)
          } else {
            TreeBuilder.fromTreeSplitAndChild(newChild, treeSplit)
          }
        } else {
          TreeBuilder.fromTreeSplitAndChild(newChild, treeSplit)
        }
    }

  /** Modifies value of the node accessible by the path. */
  final def modifyValueAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    modify: T => T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeByFullPath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          Right(modifyChildValueInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node accessible by the path. */
  final def modifyTreeAt[T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[T1],
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeByFullPath[T, T1](tree, pathIterator)
      .map {
        case (treeSplit, recipientTree) =>
          Right(modifyChildInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node accessible by the path using a path item extractor. */
  final def modifyValueAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: T => T1,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeByFullPath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          Right(modifyChildValueInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

  /** Modifies value of the node accessible by the path. */
  final def modifyTreeAt[K, T, T1 >: T: ClassTag](
    tree: NodeTree[T],
    pathIterator: Iterator[K],
    toPathItem: T => K,
    modify: Tree[T] => Tree[T1],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    splitTreeByFullPath(tree, pathIterator, toPathItem)
      .map {
        case (treeSplit, recipientTree) =>
          Right(modifyChildInSplit(treeSplit, recipientTree, modify, keepDistinct))
      }
      .getOrElse(Left(tree))

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
          TreeBuilder.linearTreeFromList(branchHead :: branchTailIterator.toList).asInstanceOf[NodeTree[T1]]
        branchTree :: subtreesLeft

      case head :: tail if head.value == branchHead =>
        val modified = insertBranchUnsafe(head, branchTailIterator)
        subtreesLeft.reverse ::: (modified :: tail)

      case head :: tail =>
        insertBranchInSubtrees(branchHead, head :: subtreesLeft, tail, branchTailIterator)
    }

  final def insertBranch[T, T1 >: T: ClassTag](tree: NodeTree[T], branchIterator: Iterator[T1]): Option[Tree[T1]] =
    splitTreeByPath(tree, branchIterator).flatMap {
      case (treeSplit, Some(value), remainingBranchIterator, remainingTree) =>
        val branchTree: NodeTree[T1] =
          TreeBuilder.linearTreeFromList(value :: remainingBranchIterator.toList).asInstanceOf[NodeTree[T1]]

        val newNode = Tree(remainingTree.value, branchTree :: remainingTree.subtrees)
        Some(TreeBuilder.fromTreeSplitAndChild(newNode, treeSplit))

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

}
