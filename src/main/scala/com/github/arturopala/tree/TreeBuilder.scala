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

import com.github.arturopala.tree.Tree.{ArrayTree, NodeTree}
import com.github.arturopala.tree.util.{ArrayTree, Buffer, IntBuffer, IntSlice, Slice}

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
  final def fromPairsIterator[T](iterator: Iterator[(Int, T)]): List[Tree[T]] = fromPairsIterator(iterator, Nil)

  /** Builds a tree from an iterable of pairs (numberOfChildren, value). */
  final def fromPairsIterable[T](iterable: Iterable[(Int, T)]): List[Tree[T]] =
    fromPairsIterator(iterable.iterator, Nil)

  @tailrec
  private final def fromPairsIterator[T](iterator: Iterator[(Int, T)], result: List[NodeTree[T]] = Nil): List[Tree[T]] =
    if (iterator.hasNext) {
      val (size, value) = iterator.next()
      fromPairsIterator(iterator, Tree(value, result.take(size)) :: result.drop(size))
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

  /** Builds a tree from a pair of arrays:
    *   - `structure` is an arrays holding linearized tree structure,
    *   - `values` is an arrays holding node's values.
    *
    * @note Both arrays have to return data following rules set in [[Tree.toArrays]].
    */
  final def fromArrays[T: ClassTag](structure: Array[Int], values: Array[T]): List[Tree[T]] = {
    assert(
      structure.length == values.length,
      "When constructing Tree from arrays, structure and values must be of the same size."
    )

    if (structure.isEmpty) List(Tree.empty)
    else {
      val structureSlice = IntSlice.of(structure)
      val valuesSlice = Slice.of(values)

      val hasSingleTree = ArrayTree.treeSize(structure.length - 1, structure) == structure.length

      if (hasSingleTree) {

        val width = structure.count(_ == 0)
        val height = ArrayTree.calculateHeight(structure.length - 1, structure)
        val tree = new ArrayTree[T](structureSlice, valuesSlice, width, height)
        List(tree)

      } else {

        val length = structure.length
        var list: List[Tree[T]] = Nil

        Try {
          var i = 0
          while (i < length) {
            val tree = ArrayTree.treeAt(length - i - 1, structureSlice.dropRight(i), valuesSlice.dropRight(i))
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
  @tailrec
  final def fromTreePairsList[T](
    list: List[(Int, Tree[T])],
    result: List[NodeTree[T]] = Nil,
    offset: Int = 0,
    strategy: TreeMergeStrategy = TreeMergeStrategy.Join
  ): List[Tree[T]] =
    list match {
      case Nil => if (result.isEmpty) List(Tree.empty) else result
      case (size, tree) :: xs =>
        tree match {
          case Tree.empty =>
            val offset = if (strategy.keepOrphanedSubtrees) size else -1
            fromTreePairsList(xs, result.drop(size - offset), offset, strategy)
          case node: NodeTree[T] =>
            val merged = strategy.merge(node, result.take(size))
            fromTreePairsList(xs, merged :: result.drop(size), 0, strategy)
        }
    }

  /** Builds a tree from a pair of buffers.
    *  - `structureBuffer` is a buffer holding linearized tree structure,
    *  - `valuesBuffer` is a buffer holding node's values.
    *
    * @note Both buffers have to follow rules set in [[Tree.toArrays]].
    */
  @`inline` final def fromBuffers[T: ClassTag](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Tree[T] =
    fromSlices(structureBuffer.toSlice, valuesBuffer.toSlice)

  /** Builds a tree from a pair of array slices.
    *  - `structure` is a slice holding linearized tree structure,
    *  - `values` is a slice holding node's values.
    *
    * @note Both slices have to follow rules set in [[Tree.toArrays]].
    */
  final def fromSlices[T: ClassTag](structure: IntSlice, values: Slice[T]): Tree[T] =
    new ArrayTree[T](
      structure,
      values,
      ArrayTree.calculateWidth(structure),
      ArrayTree.calculateHeight(structure)
    )

  /** Builds a single-branch tree from a list of values. */
  final def fromValueList[T: ClassTag](list: List[T]): Tree[T] = list.reverse match {
    case Nil => Tree.empty
    case value :: tail =>
      tail.foldLeft(Tree(value))((t, v) => Tree(v, t))
  }

  /** Builds a main-branch tree from a list of trees. */
  final def fromTreeList[T: ClassTag](list: List[Tree[T]]): Tree[T] = list.reverse match {
    case Nil => Tree.empty
    case value :: tail =>
      tail.foldLeft(value)((t, v) => v.insertTree(t))
  }

  /** Builds a tree from the list of tree splits (nodeValue, leftChildrenList, rightChildrenList). */
  final def fromTreeSplitAndNewNode[T](
    newNode: Tree[T],
    list: List[(T, List[NodeTree[T]], List[NodeTree[T]])]
  ): Tree[T] = list match {
    case Nil => newNode
    case (hv, hl, hr) :: xs =>
      newNode match {
        case Tree.empty =>
          val newNode = Tree(hv, hl ::: hr)
          xs.foldLeft(newNode) { case (n, (v, l, r)) => Tree(v, l ::: (n :: r)) }

        case tree: NodeTree[T] =>
          list.foldLeft(tree) { case (n, (v, l, r)) => Tree(v, l ::: (n :: r)) }

        case tree: ArrayTree[T] => //TODO build ArrayTree instead
          list.foldLeft(tree.inflated.asInstanceOf[NodeTree[T]]) { case (n, (v, l, r)) => Tree(v, l ::: (n :: r)) }
      }
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

  final object TreeMergeStrategy {

    /** Default strategy is to preserve all existing subtrees. */
    object Join extends TreeMergeStrategy {

      /** Concatenates new and existing subtrees of an expanded node. */
      override final def merge[T](newNode: NodeTree[T], existingSubtrees: List[NodeTree[T]]): NodeTree[T] =
        Tree(newNode.value, existingSubtrees ::: newNode.subtrees)

      /** Joins orphaned subtrees to the parent node. */
      override final def keepOrphanedSubtrees: Boolean = true
    }

    /** A strategy to replace existing subtrees with the new ones. */
    object Replace extends TreeMergeStrategy {

      /** Replaces old subtrees with the new ones. */
      override final def merge[T](newNode: NodeTree[T], existingSubtrees: List[NodeTree[T]]): NodeTree[T] =
        newNode

      /** Removes orphaned subtrees completely. */
      override final def keepOrphanedSubtrees: Boolean = false
    }

  }
}
