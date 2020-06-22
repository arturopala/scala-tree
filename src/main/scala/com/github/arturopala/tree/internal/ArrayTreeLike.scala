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

import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.TreeOptions.TraversingMode
import com.github.arturopala.tree.TreeOptions.TraversingMode.TopDownDepthFirst
import com.github.arturopala.tree.internal.IterableOps._
import com.github.arturopala.tree.{Tree, TreeLike}

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.ArrayTree]] final functions set.
  */
abstract class ArrayTreeLike[F[+_]: Transformer, T] extends TreeLike[F, T] {

  protected def structure: IntSlice
  protected def content: Slice[T]
  protected def tree: F[T]

  /** The top index of the slices. */
  @`inline` private final def top: Int = structure.top

  // VALUES

  final override def head: T = content.last

  final override def headOption: Option[T] = Some(content.last)

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T] =
    if (mode.isDepthFirst) iterableFrom(content.reverseIterator)
    else
      iterableFrom(
        ArrayTree.valuesIterator(top, tree, depthFirst = false)
      )

  final override def leaves: Iterable[T] =
    iterableFrom(ArrayTree.leavesIterator(top, tree))

  final override def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T] = iterableFrom {
    if (mode == TopDownDepthFirst && maxDepth >= height) content.reverseIterator(pred)
    else
      ArrayTree
        .valuesIteratorWithLimit(
          top,
          tree,
          pred,
          maxDepth,
          mode.isDepthFirst
        )
  }

  def valuesAndLevelsWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, T, Boolean)] =
    iterableFrom(
      ArrayTree
        .valuesAndLevelsIteratorWithLimit(
          top,
          tree,
          pred,
          maxDepth,
          mode.isDepthFirst
        )
    )

  final override def childrenValues: Iterable[T] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexesIterator(top, structure)
        .map(content)
    )

  final override def children: Iterable[F[T]] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexesIterator(top, structure)
        .map(ArrayTree.instanceAt(_, tree))
    )

  final override def firstChildValue: Option[T] =
    if (sizeOf(tree) <= 1) None
    else Some(content(top - 1))

  final override def lastChildValue: Option[T] =
    if (sizeOf(tree) <= 1) None
    else {
      ArrayTreeFunctions
        .lastChildIndex(top, structure)
        .map(content)
    }

  final override def firstChild: Option[F[T]] =
    if (sizeOf(tree) <= 1) None
    else Some(ArrayTree.instanceAt(top - 1, tree))

  final override def lastChild: Option[F[T]] =
    if (sizeOf(tree) <= 1) None
    else {
      ArrayTreeFunctions
        .lastChildIndex(top, structure)
        .map(ArrayTree.instanceAt(_, tree))
    }

  // TREES

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[F[T]] =
    iterableFrom(ArrayTree.treesIterator(top, tree, mode.isDepthFirst))

  final override def treesWithFilter(
    pred: F[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[F[T]] =
    iterableFrom {
      if (maxDepth >= height)
        ArrayTree.treesIteratorWithFilter(
          top,
          tree,
          pred,
          mode.isDepthFirst
        )
      else
        ArrayTree
          .treesIteratorWithLimit(
            top,
            tree,
            pred,
            maxDepth,
            mode.isDepthFirst
          )
    }

  def treesAndLevelsWithFilter(
    pred: F[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, F[T])] =
    iterableFrom(
      ArrayTree.treesAndLevelsIteratorWithLimit(
        top,
        tree,
        pred,
        maxDepth,
        mode.isDepthFirst
      )
    )

  // BRANCHES

  final override def paths: Iterable[Iterable[T]] =
    iterableFrom(ArrayTree.pathsIterator(top, tree))

  final override def pathsWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom(
      if (maxDepth >= height)
        ArrayTree.pathsIteratorWithFilter(top, tree, pred)
      else
        ArrayTree
          .pathsIteratorWithLimit(top, tree, pred, maxDepth)
    )

  final override def branches: Iterable[Iterable[T]] =
    iterableFrom(ArrayTree.branchesIterator(top, tree))

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom(
      if (maxDepth >= height)
        ArrayTree.branchesIteratorWithFilter(top, tree, pred)
      else
        ArrayTree
          .branchesIteratorWithLimit(top, tree, pred, maxDepth)
    )

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(top, tree, pred)

  // PATH-BASED OPERATIONS

  final override def selectValue[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean = false): Option[T] =
    ArrayTree.selectValue(path, top, tree, toPathItem, rightmost)

  final override def selectTree[T1 >: T](path: Iterable[T1], rightmost: Boolean = false): Option[F[T]] =
    ArrayTree.selectTree(path, top, tree, rightmost)

  final override def selectTree[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean): Option[F[T]] =
    ArrayTree.selectTree(path, top, tree, toPathItem, rightmost)

  final override def containsValue[T1 >: T](value: T1): Boolean =
    content.reverseIterator.contains(value)

  final override def existsValue(pred: T => Boolean): Boolean =
    content.reverseIterator.exists(pred)

  final override def containsChildValue[T1 >: T](value: T1): Boolean =
    ArrayTree.containsChildValue(value, top, tree)

  final override def existsChildValue(pred: T => Boolean): Boolean =
    ArrayTree.existsChildValue(pred, top, tree)

  final override def containsChild[T1 >: T](child: F[T1]): Boolean =
    ArrayTree.containsChild(child, top, tree)

  final override def existsChild[T1 >: T](pred: F[T1] => Boolean): Boolean =
    ArrayTree.existsChild(pred, top, tree)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    ArrayTree.containsBranch(branch, top, tree)

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsBranch(branch, top, tree, toPathItem)

  final override def existsBranch(pred: Iterable[T] => Boolean): Boolean =
    ArrayTree.branchesIteratorWithFilter(top, tree, pred).nonEmpty

  final override def existsBranch[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    ArrayTree
      .branchesIteratorWithFilter(top, tree, pred, toPathItem)
      .nonEmpty

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
    ArrayTree.containsPath(path, top, tree)

  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsPath(path, top, tree, toPathItem)

  final override def existsPath(pred: Iterable[T] => Boolean): Boolean =
    ArrayTree.pathsIteratorWithFilter(top, tree, pred).nonEmpty

  final override def existsPath[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    ArrayTree
      .pathsIteratorWithFilter(top, tree, pred, toPathItem)
      .nonEmpty

  // TRANSFORMATIONS

  final override def map[K](f: T => K): F[K] =
    ArrayTree.map(tree, f)

  final override def flatMap[K: ClassTag](f: T => F[K]): F[K] =
    ArrayTree.flatMapDistinct(tree, f)

  // MODIFICATIONS

  final override def prepend[T1 >: T](value: T1): F[T1] =
    ArrayTree.prepend(value, tree)

  final override def insertLeaf[T1 >: T](value: T1, append: Boolean = false): F[T1] =
    ArrayTree.insertLeaf(top, value, tree, append, keepDistinct = true)

  final override def insertLeaves[T1 >: T](values: Iterable[T1], append: Boolean = false): F[T1] =
    if (values.isEmpty) tree
    else
      ArrayTree.insertLeaves(top, values, tree, append, keepDistinct = true)

  final override def insertLeafAt[T1 >: T](path: Iterable[T1], value: T1, append: Boolean = false): F[T1] =
    ArrayTree.insertLeafAt(path, value, tree, append, keepDistinct = true)

  final override def insertLeafAt[K, T1 >: T](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K,
    append: Boolean
  ): Either[F[T], F[T1]] = ArrayTree.insertLeafAt(path, value, tree, toPathItem, append, keepDistinct = true)

  final override def insertChild[T1 >: T](child: F[T1], append: Boolean = false): F[T1] =
    ArrayTree.insertChildDistinct(top, child, tree, append)

  final override def insertChildren[T1 >: T](
    children: Iterable[F[T1]],
    append: Boolean = false
  ): F[T1] =
    if (append) ArrayTree.insertAfterChildren(tree, children, keepDistinct = true)
    else ArrayTree.insertBeforeChildren(tree, children, keepDistinct = true)

  final override def insertChildAt[T1 >: T](
    path: Iterable[T1],
    child: F[T1],
    append: Boolean = false
  ): F[T1] =
    ArrayTree.insertChildAt(path, child, tree, append, keepDistinct = true)

  final override def insertChildAt[K, T1 >: T](
    path: Iterable[K],
    child: F[T1],
    toPathItem: T => K,
    append: Boolean
  ): Either[F[T], F[T1]] =
    ArrayTree.insertChildAt(path, child, tree, toPathItem, append, keepDistinct = true)

  final override def insertChildrenAt[T1 >: T](
    path: Iterable[T1],
    children: Iterable[F[T1]],
    append: Boolean = false
  ): F[T1] =
    ArrayTree.insertChildrenAt(path, children, tree, append, keepDistinct = true)

  final override def insertChildrenAt[K, T1 >: T](
    path: Iterable[K],
    children: Iterable[F[T1]],
    toPathItem: T => K,
    append: Boolean
  ): Either[F[T], F[T1]] =
    ArrayTree.insertChildrenAt(path, children, tree, toPathItem, append, keepDistinct = true)

  final override def insertBranch[T1 >: T](branch: Iterable[T1], append: Boolean = false): F[T1] =
    ArrayTree.insertBranch(top, branch, tree, append)

  final override def insertBranches[T1 >: T](
    branches: Iterable[Iterable[T1]],
    append: Boolean = false
  ): F[T1] =
    ArrayTree.insertBranches(top, branches, tree, append)

  // DISTINCT UPDATES

  final override def updateHead[T1 >: T](replacement: T1): F[T1] =
    ArrayTree.updateValue(top, replacement, tree, keepDistinct = false)

  final override def updateChildValue[T1 >: T](existingValue: T1, replacement: T1): F[T1] =
    ArrayTree.updateChildValue(existingValue, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateValueAt[T1 >: T](path: Iterable[T1], replacement: T1): Either[F[T], F[T1]] =
    ArrayTree.updateValueAt(path, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateValueAt[K, T1 >: T](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[F[T], F[T1]] =
    ArrayTree.updateValueAt(path, replacement, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def updateChild[T1 >: T](value: T1, replacement: F[T1]): F[T1] =
    ArrayTree.updateChild(value, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateTreeAt[T1 >: T](
    path: Iterable[T1],
    replacement: F[T1]
  ): Either[F[T], F[T1]] =
    ArrayTree.updateTreeAt(path, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateTreeAt[K, T1 >: T](
    path: Iterable[K],
    replacement: F[T1],
    toPathItem: T => K
  ): Either[F[T], F[T1]] =
    ArrayTree.updateTreeAt(path, replacement, tree, toPathItem, rightmost = false, keepDistinct = true)

  // DISTINCT MODIFICATIONS

  final override def modifyHead[T1 >: T](modify: T => T1): F[T1] =
    ArrayTree.modifyValue(top, modify, tree, keepDistinct = false)

  final override def modifyChildValue[T1 >: T](value: T1, modify: T => T1): F[T1] =
    ArrayTree.modifyChildValue(value, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyValueAt[T1 >: T](
    path: Iterable[T1],
    modify: T => T1
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyValueAt[K, T1 >: T](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def modifyChild[T1 >: T](value: T1, modify: F[T] => F[T1]): F[T1] =
    ArrayTree.modifyChild(value, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyChildren[T1 >: T](modify: Iterable[F[T]] => Iterable[F[T1]]): F[T1] =
    ArrayTree.modifyChildren(modify, tree)

  final override def modifyTreeAt[T1 >: T](
    path: Iterable[T1],
    modify: F[T] => F[T1]
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyTreeAt[K, T1 >: T](
    path: Iterable[K],
    modify: F[T] => F[T1],
    toPathItem: T => K
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def modifyChildrenAt[T1 >: T](
    path: Iterable[T1],
    modify: Iterable[F[T]] => Iterable[F[T1]]
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyChildrenAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyChildrenAt[K, T1 >: T](
    path: Iterable[K],
    modify: Iterable[F[T]] => Iterable[F[T1]],
    toPathItem: T => K
  ): Either[F[T], F[T1]] =
    ArrayTree.modifyChildrenAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  // REMOVALS

  final override def removeChildValue[T1 >: T](value: T1): F[T] =
    ArrayTree.removeChildValue(value, tree, rightmost = false, keepDistinct = true)

  final override def removeValueAt[T1 >: T](path: Iterable[T1]): F[T] =
    ArrayTree.removeValueAt(path, tree, rightmost = false, keepDistinct = true)

  final override def removeValueAt[K](path: Iterable[K], toPathItem: T => K): F[T] =
    ArrayTree.removeValueAt(path, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def removeChild[T1 >: T](value: T1): F[T] =
    ArrayTree.removeChild(tree, value, rightmost = false)

  final override def removeChildren[T1 >: T](): F[T] =
    ArrayTree.removeChildren(top, tree)

  final override def removeTreeAt[T1 >: T](path: Iterable[T1]): F[T] =
    ArrayTree.removeTreeAt(path, tree, rightmost = false)

  final override def removeTreeAt[K](path: Iterable[K], toPathItem: T => K): F[T] =
    ArrayTree.removeTreeAt(path, tree, toPathItem, rightmost = false)

  final override def removeChildrenAt[T1 >: T](path: Iterable[T1]): F[T] =
    ArrayTree.removeChildrenAt(path, tree, rightmost = false)

  final override def removeChildrenAt[K](path: Iterable[K], toPathItem: T => K): F[T] =
    ArrayTree.removeChildrenAt(path, tree, toPathItem, rightmost = false)

  final override def toPairsIterator: Iterator[(Int, T)] = structure.iterator.zip(content.iterator)

  @`inline` final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) =
    (structure.toArray, content.toArray[T1])

  @`inline` final override def toSlices[T1 >: T]: (IntSlice, Slice[T1]) =
    (structure, content.asInstanceOf[Slice[T1]]) // safe cast as Slice is read-only structure

  final def asSlices: (IntSlice, Slice[T]) = (structure, content)

  @`inline` final override def toBuffers[T1 >: T]: (IntBuffer, Buffer[T1]) =
    (structure.asBuffer, content.toBuffer[T1])

  @`inline` final override def toStructureArray: Array[Int] = structure.toArray

  final override def mkStringFromBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int
  ): String =
    ArrayTreeFunctions
      .mkStringFromBranches(
        top,
        structure,
        content,
        show,
        valueSeparator,
        branchSeparator,
        branchStart,
        branchEnd,
        maxDepth
      )
      .toString()

  /** Forwarder to the [[Transformer.sizeOf]]. */
  private final def sizeOf(target: F[T]): Int =
    implicitly[Transformer[F]].sizeOf(target)

}
