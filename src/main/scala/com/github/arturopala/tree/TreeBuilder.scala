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
import com.github.arturopala.tree.internal.{ArrayTree, ArrayTreeFunctions}

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
    result: List[NodeTree[T]] = Nil
  ): List[Tree[T]] =
    if (iterator.hasNext) {
      val (size, value) = iterator.next()
      fromSizeAndValuePairs(iterator, Tree(value, result.take(size)) :: result.drop(size))
    } else if (result.isEmpty) List(Tree.empty)
    else result

  /** Builds a tree from a pair of iterable collections:
    *   - `structure` is a collection representing serialized tree structure,
    *   - `values` is a collection of node's values.
    *
    * @note Both collections have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromIterables[T](structure: Iterable[Int], values: Iterable[T]): List[Tree[T]] =
    fromIterators(structure.iterator, values.iterator)

  /** Builds a tree from a pair of iterators:
    *   - `structure` is an iterator over linearized tree structure,
    *   - `values` is an iterator over node's values.
    *
    * @note Both iterators have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromIterators[T](structure: Iterator[Int], values: Iterator[T]): List[Tree[T]] =
    fromIterators(structure, values, Nil)

  @tailrec
  private final def fromIterators[T](
    structure: Iterator[Int],
    values: Iterator[T],
    result: List[NodeTree[T]]
  ): List[Tree[T]] =
    if (structure.hasNext && values.hasNext) {
      val value = values.next()
      val size = structure.next()
      fromIterators(structure, values, Tree(value.asInstanceOf[T], result.take(size)) :: result.drop(size))
    } else if (result.isEmpty) List(Tree.empty)
    else result

  /** Builds a list of trees from a pair of arrays:
    *   - `structure` is an arrays holding linearized tree structure,
    *   - `values` is an arrays holding node's values.
    *
    * @note Both arrays have to return data following rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromArrays[T: ClassTag](structure: Array[Int], values: Array[T]): List[Tree[T]] =
    fromSlices(IntSlice.of(structure), Slice.of(values))

  /** Builds a list of trees from a pair of slices:
    *   - `structure` is a slice holding linearized tree structure,
    *   - `values` is a slice holding node's values.
    *
    * @note Both slices have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromSlices[T: ClassTag](structure: IntSlice, content: Slice[T]): List[Tree[T]] = {
    assert(
      structure.length == content.length,
      "When constructing Tree from slices, structure and values must be of the same size."
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
            val tree = ArrayTree.treeAt(length - i - 1, structure.dropRight(i), content.dropRight(i))
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
  final def fromArraysHead[T: ClassTag](structure: Array[Int], values: Array[T]): Tree[T] =
    fromArrays(structure, values).headOption.getOrElse(Tree.empty)

  /** Builds a tree from a list of pairs (numberOfChildren, node), where:
    *   - `node` is a new node, and
    *   - `numberOfChildren` is a number of preceding elements in the list
    *                      to become direct subtrees of the current node.
    *   - `strategy` final defines how to merge nodes and what to do with orphaned subtrees.
    * @note - Nodes of subtrees must always precede the parent node, and appear in the reverse order.
    *       - The sum of all numberOfChildren values must be the size of the list minus one.
    */
  final def fromSizeAndTreePairsList[T](
    list: List[(Int, Tree[T])],
    result: List[NodeTree[T]] = Nil,
    strategy: TreeMergeStrategy = TreeMergeStrategy.Join
  ): List[Tree[T]] =
    fromSizeAndTreePairs(list.iterator, result, strategy)

  /** Builds a tree from an iterator of pairs (numberOfChildren, node). */
  final def fromSizeAndTreePairsIterator[T](
    iterator: Iterator[(Int, Tree[T])],
    strategy: TreeMergeStrategy = TreeMergeStrategy.Join
  ): List[Tree[T]] =
    fromSizeAndTreePairs(iterator, Nil, strategy)

  /** Builds a tree from an iterable of pairs (numberOfChildren, node). */
  final def fromSizeAndTreePairsIterable[T](
    iterable: Iterable[(Int, Tree[T])],
    strategy: TreeMergeStrategy = TreeMergeStrategy.Join
  ): List[Tree[T]] =
    fromSizeAndTreePairs(iterable.iterator, Nil, strategy)

  @tailrec
  private final def fromSizeAndTreePairs[T](
    iterator: Iterator[(Int, Tree[T])],
    result: List[NodeTree[T]] = Nil,
    strategy: TreeMergeStrategy = TreeMergeStrategy.Join
  ): List[Tree[T]] =
    if (iterator.hasNext) {
      val (size, tree) = iterator.next()
      tree match {
        case Tree.empty =>
          val offset = if (strategy.keepOrphanedSubtrees) size else -1
          fromSizeAndTreePairs(iterator, result.drop(-offset), strategy)

        case tree =>
          val subtrees = result.take(size)
          val merged = strategy.merge(tree.inflated.asInstanceOf[NodeTree[T]], subtrees)
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
  @`inline` final def fromBuffersHead[T: ClassTag](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Tree[T] =
    fromBuffers(structureBuffer, valuesBuffer).head

  /** Builds a list of trees from a pair of buffers.
    *  - `structureBuffer` is a buffer holding linearized tree structure,
    *  - `valuesBuffer` is a buffer holding node's values.
    *
    * @note Both buffers have to follow rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromBuffers[T: ClassTag](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): List[Tree[T]] =
    fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)

  /** Builds a single-branch tree from a sequence of values. */
  final def linearTreeFromSequence[T: ClassTag](seq: Seq[T]): Tree[T] = {
    val iterator = seq.reverseIterator
    if (iterator.hasNext) {
      val leaf = Tree(iterator.next())
      linearTreeFromReverseValueIterator(iterator, leaf)
    } else Tree.empty
  }

  /** Builds a single-branch tree from a reverse iterator over node's values. */
  @tailrec
  final def linearTreeFromReverseValueIterator[T: ClassTag](iterator: Iterator[T], child: NodeTree[T]): Tree[T] =
    if (iterator.hasNext) {
      linearTreeFromReverseValueIterator(iterator, Tree(iterator.next(), child))
    } else {
      child
    }

  /** Builds a main-branch tree from a list of trees. */
  final def fromTreeSequence[T: ClassTag](seq: Seq[Tree[T]]): Tree[T] =
    fromReverseTreeIterator(seq.reverseIterator, Tree.empty)

  /** Builds a single-branch tree from a reverse iterator over child trees */
  @tailrec
  final def fromReverseTreeIterator[T: ClassTag](iterator: Iterator[Tree[T]], child: Tree[T]): Tree[T] =
    if (iterator.hasNext) {
      val tree = iterator.next().insertChild(child)
      fromReverseTreeIterator(iterator, tree)
    } else {
      child
    }

  /** Builds a tree from the list of tree splits (leftChildren, value, rightChildren).
    * @param child bottom tree node, put in the middle between first leftChildren and rightChildren.
    */
  final def fromChildAndTreeSplit[T](
    child: NodeTree[T],
    treeSplit: List[(List[NodeTree[T]], T, List[NodeTree[T]])]
  ): Tree[T] =
    treeSplit.foldLeft(child) { case (n, (l, v, r)) => Tree(v, l ::: (n :: r)) }

  /** Builds a tree from the list of tree splits (leftChildren, value, rightChildren).
    */
  final def fromTreeSplit[T](
    treeSplit: List[(List[NodeTree[T]], T, List[NodeTree[T]])]
  ): Tree[T] = treeSplit match {
    case Nil => Tree.empty
    case (hl, hv, hr) :: xs =>
      xs.foldLeft(Tree(hv, hl ::: hr)) { case (n, (l, v, r)) => Tree(v, l ::: (n :: r)) }
  }

  /** There are multiple ways to merge the tree after expanding a node.
    * As we don't want to be constrained by an arbitrary choice,
    * there is a possibility to create and/or use custom strategy.
    */
  trait TreeMergeStrategy {

    /** When a value of a node expands into a new Node,
      * we need a way to deal with the existing subtrees. */
    def merge[T](newNode: NodeTree[T], existingSubtrees: List[NodeTree[T]]): NodeTree[T]

    /** When a value of a node expands into an Empty tree,
      * we need to decide either to keep or remove existing subtrees. */
    def keepOrphanedSubtrees: Boolean
  }

  object TreeMergeStrategy {

    /** Default strategy is to preserve all existing subtrees. */
    object Join extends TreeMergeStrategy {

      /** Concatenates new and existing subtrees of an expanded node. */
      override final def merge[T](newNode: NodeTree[T], existingSubtrees: List[NodeTree[T]]): NodeTree[T] =
        Tree(newNode.head, existingSubtrees ::: newNode.children)

      /** Joins orphaned subtrees to the parent node. */
      override final def keepOrphanedSubtrees: Boolean = true
    }

  }
}
