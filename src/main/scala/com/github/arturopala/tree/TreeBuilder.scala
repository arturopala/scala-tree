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
import com.github.arturopala.tree.Tree.{ArrayTree, NodeTree}
import com.github.arturopala.tree.internal.{ArrayTree, ArrayTreeFunctions, NodeTree, Transformer}

import scala.annotation.tailrec
import scala.collection.Iterator
import scala.reflect.ClassTag
import scala.util.Try

/**
  * Common tree building helpers.
  */
object TreeBuilder {

  /** Builds a tree from an iterator of pairs (numberOfChildren, value), where:
    *   - `value` is the value of a new node, and
    *   - `numberOfChildren` is a number of preceding elements in the list
    *                        to become direct subtrees of the node.
    *
    * @note - Values of subtrees must always precede the value of a parent node, and appear in the reverse order.
    *       - The sum of all numberOfChildren values must be the size of the list minus one.
    */
  final def fromSizeAndValuePairsIterator[T](iterator: Iterator[(Int, T)]): List[Tree[T]] =
    fromSizeAndValuePairs(iterator, Nil)

  /** Builds a tree from an iterable of pairs (numberOfChildren, value). */
  final def fromSizeAndValuePairsIterable[T](iterable: Iterable[(Int, T)]): List[Tree[T]] =
    fromSizeAndValuePairs(iterable.iterator, Nil)

  @tailrec
  private final def fromSizeAndValuePairs[T](
    iterator: Iterator[(Int, T)],
    result: List[Tree[T]] = Nil
  ): List[Tree[T]] =
    if (iterator.hasNext) {
      val (size, value) = iterator.next()
      fromSizeAndValuePairs(iterator, Tree(value, result.take(size)) :: result.drop(size))
    } else if (result.isEmpty) List(Tree.empty)
    else result

  /** Builds a result from a pair of iterable collections of tree structure and content:
    *
    * @param structure a collection representing serialized tree structure
    * @param content a collection of node's values
    * @tparam T type of tree values
    * @tparam R type of the result
    *
    * @note Both collections have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromIterables[T, R[+_]: TreeFold](
    structure: Iterable[Int],
    content: Iterable[T],
    refine: Option[Iterable[R[T]] => Iterable[R[T]]]
  ): List[R[T]] =
    fromIterators(structure.iterator, content.iterator, refine)

  /** Builds a tree from a pair of iterators:
    *
    * @param structure an iterator over linearised tree structure
    * @param content an iterator over node's values
    * @tparam T type of tree values
    * @tparam R type of the result
    *
    * @note Both iterators have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromIterators[T, R[+_]: TreeFold](
    structure: Iterator[Int],
    content: Iterator[T],
    refine: Option[Iterable[R[T]] => Iterable[R[T]]]
  ): List[R[T]] =
    fromIterators(structure, content, Nil, refine)

  @tailrec
  private final def fromIterators[T, R[+_]: TreeFold](
    structure: Iterator[Int],
    content: Iterator[T],
    result: List[R[T]],
    refine: Option[Iterable[R[T]] => Iterable[R[T]]]
  ): List[R[T]] =
    if (structure.hasNext && content.hasNext) {
      val value = content.next()
      val size = structure.next()
      val children = result.take(size)
      val newNode = implicitly[TreeFold[R]].fold(value.asInstanceOf[T], refine.map(_(children)).getOrElse(children))
      fromIterators(structure, content, newNode :: result.drop(size), refine)
    } else if (result.isEmpty) List(implicitly[TreeFold[R]].empty)
    else result

  /** Builds a list of trees from a pair of arrays:
    *   - `structure` is an arrays holding linearized tree structure,
    *   - `values` is an arrays holding node's values.
    *
    * @note Both arrays have to return data following rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromArrays[T: ClassTag](structure: Array[Int], content: Array[T]): List[Tree[T]] =
    fromSlices(IntSlice.of(structure), Slice.of(content))

  /** Builds a list of trees from a pair of slices:
    *   - `structure` is a slice holding linearized tree structure,
    *   - `values` is a slice holding node's values.
    *
    * @note Both slices have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromSlices[T](structure: IntSlice, content: Slice[T]): List[Tree[T]] = {
    assert(
      structure.length == content.length,
      "When constructing a Tree from slices, structure slice and values slice must be of the same size."
    )

    if (structure.isEmpty) List(Tree.empty)
    else {

      val hasSingleTree = ArrayTreeFunctions.treeSize(structure.length - 1, structure) == structure.length

      if (hasSingleTree) {

        val width = structure.count(_ == 0)
        val height = ArrayTreeFunctions.calculateHeight(structure.length - 1, structure)
        val tree = new ArrayTree[T](structure, content, width, height)
        List(tree)

      } else {

        val length = structure.length
        var list: List[Tree[T]] = Nil

        Try {
          var i = 0
          while (i < length) {
            val tree = {
              val (s, c) = ArrayTreeFunctions.treeAt(length - i - 1, structure.dropRight(i), content.dropRight(i))
              Transformer.OfTree.fromSlices(s, c)
            }
            list = tree :: list
            i = i + tree.size
          }
        }.recover {
          case _: IllegalArgumentException =>
            list = Tree.empty :: list
        }

        list.reverse
      }
    }
  }

  /** Shortcut for [[TreeBuilder.fromArrays]].
    * @return head element from the produced list or an empty tree */
  final def fromArraysHead[T: ClassTag](structure: Array[Int], content: Array[T]): Tree[T] =
    fromArrays(structure, content).headOption.getOrElse(Tree.empty)

  /** Builds a tree from a sequence of pairs (numberOfChildren, node), where:
    *   - `node` is a new node, and
    *   - `numberOfChildren` is a number of preceding elements in the list
    *                      to become direct subtrees of the current node.
    *   - `strategy` final defines how to merge nodes and what to do with orphaned subtrees.
    * @note - Nodes of subtrees must always precede the parent node, and appear in the reverse order.
    *       - The sum of all numberOfChildren values must be the size of the list minus one.
    */
  final def fromSizeAndTreePairsSequence[T](
    sequence: Seq[(Int, Tree[T])],
    result: List[NodeTree[T]] = Nil,
    strategy: MergeStrategy = MergeStrategy.AppendLax
  ): Seq[Tree[T]] =
    fromSizeAndTreePairs(sequence.iterator, result, strategy)

  /** Builds a tree from an iterator of pairs (numberOfChildren, node). */
  final def fromSizeAndTreePairsIterator[T](
    iterator: Iterator[(Int, Tree[T])],
    strategy: MergeStrategy = MergeStrategy.AppendLax
  ): List[Tree[T]] =
    fromSizeAndTreePairs(iterator, Nil, strategy)

  /** Builds a tree from an iterable of pairs (numberOfChildren, node). */
  final def fromSizeAndTreePairsIterable[T](
    iterable: Iterable[(Int, Tree[T])],
    strategy: MergeStrategy = MergeStrategy.AppendLax
  ): List[Tree[T]] =
    fromSizeAndTreePairs(iterable.iterator, Nil, strategy)

  @tailrec
  private final def fromSizeAndTreePairs[T](
    iterator: Iterator[(Int, Tree[T])],
    result: List[Tree[T]] = Nil,
    strategy: MergeStrategy = MergeStrategy.AppendLax
  ): List[Tree[T]] =
    if (iterator.hasNext) {
      val (size, tree) = iterator.next()
      tree match {
        case Tree.empty =>
          val offset = if (strategy.keepOrphanedChildren) size else -1
          fromSizeAndTreePairs(iterator, result.drop(-offset), strategy)

        case tree =>
          val subtrees = result.take(size)
          val merged = strategy.merge(tree, subtrees)
          fromSizeAndTreePairs(iterator, merged :: result.drop(size), strategy)
      }
    } else if (result.isEmpty) List(Tree.empty)
    else result

  /** Builds a tree from a pair of buffers.
    *  - `structureBuffer` is a buffer holding linearized tree structure,
    *  - `valuesBuffer` is a buffer holding node's values.
    *
    * @note Both buffers have to follow rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromBuffersHead[T](structureBuffer: IntBuffer, contentBuffer: Buffer[T]): Tree[T] =
    fromBuffers(structureBuffer, contentBuffer).head

  /** Builds a list of trees from a pair of buffers.
    *  - `structureBuffer` is a buffer holding linearized tree structure,
    *  - `valuesBuffer` is a buffer holding node's values.
    *
    * @note Both buffers have to follow rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromBuffers[T](structureBuffer: IntBuffer, contentBuffer: Buffer[T]): List[Tree[T]] =
    fromSlices(structureBuffer.asSlice, contentBuffer.asSlice)

  /** Builds a single-branch tree from a sequence of values. */
  final def linearTreeFromSequence[T](seq: Seq[T]): Tree[T] = {
    val iterator = seq.reverseIterator
    if (iterator.hasNext) {
      val leaf = Tree(iterator.next())
      linearTreeFromReverseValueIterator(iterator, leaf)
    } else Tree.empty
  }

  /** Builds a single-branch tree from a reverse iterator over node's values. */
  @tailrec
  final def linearTreeFromReverseValueIterator[T](iterator: Iterator[T], child: Tree[T]): Tree[T] =
    if (iterator.hasNext) {
      linearTreeFromReverseValueIterator(iterator, Tree(iterator.next(), child))
    } else {
      child
    }

  /** Builds a main-branch tree from a list of trees. */
  final def fromTreeSequence[T](seq: Seq[Tree[T]]): Tree[T] =
    fromReverseTreeIterator(seq.reverseIterator, Tree.empty)

  /** Builds a main-branch tree from a list of trees. */
  final def fromTreeSequence[T](seq: Seq[Tree[T]], lastChild: Tree[T]): Tree[T] =
    fromReverseTreeIterator(seq.reverseIterator, lastChild)

  /** Builds a main-branch tree from a list of trees. */
  final def fromTreeSequence[T](head: T, seq: Seq[Tree[T]], lastChild: Tree[T]): Tree[T] =
    Tree(head, fromReverseTreeIterator(seq.reverseIterator, lastChild))

  /** Builds a single-branch tree from a reverse iterator over child trees */
  @tailrec
  final def fromReverseTreeIterator[T](iterator: Iterator[Tree[T]], lastChild: Tree[T]): Tree[T] =
    if (iterator.hasNext) {
      val tree = iterator.next().insertChild(lastChild)
      fromReverseTreeIterator(iterator, tree)
    } else {
      lastChild
    }

  /** Builds a tree from the sequence of tree splits (leftChildren, value, rightChildren).
    * @param child bottom tree node, put in the middle between first leftChildren and rightChildren.
    */
  final def fromChildAndTreeSplit[T](
    child: Tree[T],
    treeSplit: Seq[(Seq[Tree[T]], T, Seq[Tree[T]])]
  ): Tree[T] =
    treeSplit.foldLeft(child) { case (n, (l, v, r)) => Tree(v, l ++: (n +: r)) }

  /** Builds a tree from the sequence of tree splits (leftChildren, value, rightChildren).
    */
  final def fromTreeSplit[T](
    treeSplit: Seq[(Seq[Tree[T]], T, Seq[Tree[T]])]
  ): Tree[T] =
    if (treeSplit.isEmpty) Tree.empty
    else
      treeSplit.head match {
        case (hl, hv, hr) =>
          treeSplit.tail.foldLeft(Tree(hv, hl ++: hr)) { case (n, (l, v, r)) => Tree(v, l ++: (n +: r)) }
      }

  /** There are multiple ways to merge the tree after expanding a node.
    * As we don't want to be constrained by an arbitrary choice,
    * there is a possibility to create and/or use custom strategy.
    */
  trait MergeStrategy {

    /** When a value of a node expands into a new Node,
      * we need a way to deal with the existing subtrees. */
    def merge[T](newNode: Tree[T], existingChildren: Iterable[Tree[T]]): Tree[T]

    /** When a value of a node expands into an Empty tree,
      * we need to decide whether to keep or remove existing children. */
    def keepOrphanedChildren: Boolean
  }

  object MergeStrategy {

    /** Joins orphaned children to the parent node. */
    trait KeepOrphanedMergeStrategy extends MergeStrategy {
      override final def keepOrphanedChildren: Boolean = true
    }

    /** Appends new children to the existing. */
    object AppendLax extends KeepOrphanedMergeStrategy {

      /** Concatenates new and existing children of an expanded node. */
      override final def merge[T](newNode: Tree[T], existingChildren: Iterable[Tree[T]]): Tree[T] =
        Tree(newNode.head, existingChildren ++ newNode.children)
    }

    /** Prepends new children to the existing. */
    object PrependLax extends KeepOrphanedMergeStrategy {

      /** Concatenates new and existing children of an expanded node. */
      override final def merge[T](newNode: Tree[T], existingChildren: Iterable[Tree[T]]): Tree[T] =
        Tree(newNode.head, newNode.children ++ existingChildren)
    }

    /** Appends new children to the existing while keeping them distinct, merges down if necessary. */
    object AppendDistinct extends KeepOrphanedMergeStrategy {

      /** Concatenates new and existing children of an expanded node. */
      override final def merge[T](newNode: Tree[T], existingChildren: Iterable[Tree[T]]): Tree[T] =
        Tree(newNode.head, NodeTree.insertChildrenAfterDistinct(Vector.empty, existingChildren ++ newNode.children))
    }

    /** Prepends new children to the existing while keeping them distinct, merges down if necessary. */
    object PrependDistinct extends KeepOrphanedMergeStrategy {

      /** Concatenates new and existing children of an expanded node. */
      override final def merge[T](newNode: Tree[T], existingChildren: Iterable[Tree[T]]): Tree[T] =
        Tree(
          newNode.head,
          NodeTree.insertChildrenBeforeDistinct(newNode.children, existingChildren, preserveExisting = true)
        )
    }

  }
}
