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
import com.github.arturopala.tree.Tree.ArrayTree
import com.github.arturopala.tree.TreeOptions.TraversingMode
import com.github.arturopala.tree.TreeOptions.TraversingMode.TopDownDepthFirst
import com.github.arturopala.tree.{Tree, TreeBuilder, TreeLike}
import com.github.arturopala.tree.internal.IterableOps._

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.ArrayTree]] final functions set.
  * Extracted from the [[Tree]] to de-clutter its codebase.
  */
abstract class ArrayTreeLike[T] extends TreeLike[T] {

  protected val arrayTree: ArrayTree[T]
  def tree: Tree[T] = arrayTree

  // VALUES

  final override def head: T = arrayTree.content.last

  final override def headOption: Option[T] = Some(arrayTree.content.last)

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T] =
    if (mode.isDepthFirst) iterableFrom(arrayTree.content.reverseIterator)
    else
      iterableFrom(
        ArrayTree.valuesIterator(arrayTree.structure.top, arrayTree.structure, arrayTree.content, depthFirst = false)
      )

  final override def leaves: Iterable[T] =
    iterableFrom(ArrayTree.leavesIterator(arrayTree.structure.top, arrayTree.structure, arrayTree.content))

  final override def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T] = iterableFrom {
    if (mode == TopDownDepthFirst && maxDepth >= height) arrayTree.content.reverseIterator(pred)
    else
      ArrayTree
        .valuesIteratorWithLimit(
          arrayTree.structure.top,
          arrayTree.structure,
          arrayTree.content,
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
          arrayTree.structure.top,
          arrayTree.structure,
          arrayTree.content,
          pred,
          maxDepth,
          mode.isDepthFirst
        )
    )

  final override def childrenValues: Iterable[T] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexesIterator(arrayTree.structure.top, arrayTree.structure)
        .map(arrayTree.content)
    )

  final override def children: Iterable[Tree[T]] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexesIterator(arrayTree.structure.top, arrayTree.structure)
        .map(ArrayTree.treeAt2(_, arrayTree.structure, arrayTree.content))
    )

  final override def firstChildValue: Option[T] =
    if (tree.size <= 1) None
    else Some(arrayTree.content(arrayTree.top - 1))

  final override def lastChildValue: Option[T] =
    if (tree.size <= 1) None
    else {
      ArrayTreeFunctions
        .lastChildIndex(arrayTree.top, arrayTree.structure)
        .map(arrayTree.content)
    }

  final override def firstChild: Option[Tree[T]] =
    if (tree.size <= 1) None
    else Some(ArrayTree.treeAt2(arrayTree.top - 1, arrayTree.structure, arrayTree.content))

  final override def lastChild: Option[Tree[T]] =
    if (tree.size <= 1) None
    else {
      ArrayTreeFunctions
        .lastChildIndex(arrayTree.top, arrayTree.structure)
        .map(ArrayTree.treeAt2(_, arrayTree.structure, arrayTree.content))
    }

  // TREES

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[T]] =
    iterableFrom(ArrayTree.treesIterator(arrayTree.structure.top, tree, mode.isDepthFirst))

  final override def treesWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[T]] =
    iterableFrom {
      if (maxDepth >= height)
        ArrayTree.treesIteratorWithFilter(
          arrayTree.structure.top,
          arrayTree.structure,
          arrayTree.content,
          pred,
          mode.isDepthFirst
        )
      else
        ArrayTree
          .treesIteratorWithLimit(
            arrayTree.structure.top,
            arrayTree.structure,
            arrayTree.content,
            pred,
            maxDepth,
            mode.isDepthFirst
          )
    }

  def treesAndLevelsWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[T])] =
    iterableFrom(
      ArrayTree.treesAndLevelsIteratorWithLimit(
        arrayTree.structure.top,
        arrayTree.structure,
        arrayTree.content,
        pred,
        maxDepth,
        mode.isDepthFirst
      )
    )

  // BRANCHES

  final override def paths: Iterable[Iterable[T]] =
    iterableFrom(ArrayTree.pathsIterator(arrayTree.structure.top, arrayTree.structure, arrayTree.content))

  final override def pathsWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom(
      if (maxDepth >= height)
        ArrayTree.pathsIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred)
      else
        ArrayTree
          .pathsIteratorWithLimit(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred, maxDepth)
    )

  final override def branches: Iterable[Iterable[T]] =
    iterableFrom(ArrayTree.branchesIterator(arrayTree.structure.top, arrayTree.structure, arrayTree.content))

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom(
      if (maxDepth >= height)
        ArrayTree.branchesIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred)
      else
        ArrayTree
          .branchesIteratorWithLimit(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred, maxDepth)
    )

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred)

  // MODIFICATIONS

  final override def prepend[T1 >: T](value: T1): Tree[T1] =
    ArrayTree.prepend(value, tree)

  final override def insertLeaf[T1 >: T](value: T1, append: Boolean = false): Tree[T1] =
    ArrayTree.insertLeaf(arrayTree.structure.top, value, tree, append, keepDistinct = true)

  final override def insertLeaves[T1 >: T](values: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (values.isEmpty) arrayTree
    else if (values.size == 1) Tree(values.head)
    else ArrayTree.insertLeaves(arrayTree.structure.top, values, tree, append, keepDistinct = true)

  final override def insertLeafAt[T1 >: T](path: Iterable[T1], value: T1, append: Boolean = false): Tree[T1] =
    ArrayTree.insertLeafAt(path, value, tree, append, keepDistinct = true)

  final override def insertLeafAt[K, T1 >: T](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertLeafAt(path, value, tree, toPathItem, append, keepDistinct = true)

  final override def insertChild[T1 >: T](child: Tree[T1], append: Boolean = false): Tree[T1] =
    ArrayTree.insertChildDistinct(arrayTree.structure.top, child, tree, append)

  final override def insertChildren[T1 >: T](
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] =
    if (append) ArrayTree.insertAfterChildren(tree, children.filterNot(_.isEmpty), keepDistinct = true)
    else ArrayTree.insertBeforeChildren(tree, children.filterNot(_.isEmpty), keepDistinct = true)

  final override def insertChildAt[T1 >: T](
    path: Iterable[T1],
    child: Tree[T1],
    append: Boolean = false
  ): Tree[T1] =
    ArrayTree.insertChildAt(path, child, tree, append, keepDistinct = true)

  final override def insertChildAt[K, T1 >: T](
    path: Iterable[K],
    child: Tree[T1],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertChildAt(path, child, tree, toPathItem, append, keepDistinct = true)

  final override def insertChildrenAt[T1 >: T](
    path: Iterable[T1],
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] =
    ArrayTree.insertChildrenAt(path, children, tree, append, keepDistinct = true)

  final override def insertChildrenAt[K, T1 >: T](
    path: Iterable[K],
    children: Iterable[Tree[T1]],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertChildrenAt(path, children, tree, toPathItem, append, keepDistinct = true)

  final override def insertBranch[T1 >: T](branch: Iterable[T1], append: Boolean = false): Tree[T1] =
    ArrayTree.insertBranch(arrayTree.structure.top, branch, tree, append)

  final override def insertBranches[T1 >: T](
    branches: Iterable[Iterable[T1]],
    append: Boolean = false
  ): Tree[T1] =
    ArrayTree.insertBranches(arrayTree.structure.top, branches, tree, append)

  // DISTINCT UPDATES

  final override def updateHead[T1 >: T](replacement: T1): Tree[T1] =
    ArrayTree.updateValue(arrayTree.structure.top, replacement, tree, keepDistinct = false)

  final override def updateChildValue[T1 >: T](existingValue: T1, replacement: T1): Tree[T1] =
    ArrayTree.updateChildValue(existingValue, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateValueAt[T1 >: T](path: Iterable[T1], replacement: T1): Either[Tree[T], Tree[T1]] =
    ArrayTree.updateValueAt(path, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateValueAt[K, T1 >: T](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.updateValueAt(path, replacement, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def updateChild[T1 >: T](value: T1, replacement: Tree[T1]): Tree[T1] =
    ArrayTree.updateChild(value, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateTreeAt[T1 >: T](
    path: Iterable[T1],
    replacement: Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.updateTreeAt(path, replacement, tree, rightmost = false, keepDistinct = true)

  final override def updateTreeAt[K, T1 >: T](
    path: Iterable[K],
    replacement: Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.updateTreeAt(path, replacement, tree, toPathItem, rightmost = false, keepDistinct = true)

  // DISTINCT MODIFICATIONS

  final override def modifyHead[T1 >: T](modify: T => T1): Tree[T1] =
    ArrayTree.modifyValue(arrayTree.structure.top, modify, tree, keepDistinct = false)

  final override def modifyChildValue[T1 >: T](value: T1, modify: T => T1): Tree[T1] =
    ArrayTree.modifyChildValue(value, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyValueAt[T1 >: T](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyValueAt[K, T1 >: T](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def modifyChild[T1 >: T](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    ArrayTree.modifyChild(value, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyChildren[T1 >: T](modify: Iterable[Tree[T]] => Iterable[Tree[T1]]): Tree[T1] =
    ArrayTree.modifyChildren(modify, tree)

  final override def modifyTreeAt[T1 >: T](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyTreeAt[K, T1 >: T](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def modifyChildrenAt[T1 >: T](
    path: Iterable[T1],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyChildrenAt(path, modify, tree, rightmost = false, keepDistinct = true)

  final override def modifyChildrenAt[K, T1 >: T](
    path: Iterable[K],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyChildrenAt(path, modify, tree, toPathItem, rightmost = false, keepDistinct = true)

  // REMOVALS

  final override def removeChildValue[T1 >: T](value: T1): Tree[T] =
    ArrayTree.removeChildValue(value, tree, rightmost = false, keepDistinct = true)

  final override def removeValueAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeValueAt(path, tree, rightmost = false, keepDistinct = true)

  final override def removeValueAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeValueAt(path, tree, toPathItem, rightmost = false, keepDistinct = true)

  final override def removeChild[T1 >: T](value: T1): Tree[T] =
    ArrayTree.removeChild(tree, value, rightmost = false)

  final override def removeChildren[T1 >: T](): Tree[T] =
    ArrayTree.removeChildren(arrayTree.top, tree)

  final override def removeTreeAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeTreeAt(path, tree, rightmost = false)

  final override def removeTreeAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeTreeAt(path, tree, toPathItem, rightmost = false)

  final override def removeChildrenAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeChildrenAt(path, tree, rightmost = false)

  final override def removeChildrenAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeChildrenAt(path, tree, toPathItem, rightmost = false)

  // TRANSFORMATIONS

  final override def map[K](f: T => K): Tree[K] =
    new ArrayTree[K](arrayTree.structure, arrayTree.content.map(f), tree.width, tree.height)

  // PATH-BASED OPERATIONS

  final override def selectValue[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean = false): Option[T] =
    ArrayTree.selectValue(path, arrayTree.structure.top, arrayTree.structure, arrayTree.content, toPathItem, rightmost)

  final override def selectTree[T1 >: T](path: Iterable[T1], rightmost: Boolean = false): Option[Tree[T]] =
    ArrayTree.selectTree(path, arrayTree.structure.top, arrayTree.structure, arrayTree.content, rightmost)

  final override def selectTree[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean): Option[Tree[T]] =
    ArrayTree.selectTree(path, arrayTree.structure.top, arrayTree.structure, arrayTree.content, toPathItem, rightmost)

  final override def containsValue[T1 >: T](value: T1): Boolean =
    arrayTree.content.reverseIterator.contains(value)

  final override def existsValue(pred: T => Boolean): Boolean =
    arrayTree.content.reverseIterator.exists(pred)

  final override def containsChildValue[T1 >: T](value: T1): Boolean =
    ArrayTree.containsChildValue(value, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def existsChildValue(pred: T => Boolean): Boolean =
    ArrayTree.existsChildValue(pred, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def containsChild[T1 >: T](child: Tree[T1]): Boolean =
    ArrayTree.containsChild(child, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def existsChild[T1 >: T](pred: Tree[T1] => Boolean): Boolean =
    ArrayTree.existsChild(pred, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    ArrayTree.containsBranch(branch, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsBranch(branch, arrayTree.structure.top, arrayTree.structure, arrayTree.content.map(toPathItem))

  final override def existsBranch(pred: Iterable[T] => Boolean): Boolean =
    ArrayTree.branchesIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred).nonEmpty

  final override def existsBranch[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    ArrayTree
      .branchesIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content.map(toPathItem), pred)
      .nonEmpty

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
    ArrayTree.containsPath(path, arrayTree.structure.top, arrayTree.structure, arrayTree.content)

  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsPath(path, arrayTree.structure.top, arrayTree.structure, arrayTree.content.map(toPathItem))

  final override def existsPath(pred: Iterable[T] => Boolean): Boolean =
    ArrayTree.pathsIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content, pred).nonEmpty

  final override def existsPath[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    ArrayTree
      .pathsIteratorWithFilter(arrayTree.structure.top, arrayTree.structure, arrayTree.content.map(toPathItem), pred)
      .nonEmpty

  final override def toPairsIterator: Iterator[(Int, T)] = arrayTree.structure.iterator.zip(arrayTree.content.iterator)

  @`inline` final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) =
    (arrayTree.structure.toArray, arrayTree.content.toArray[T1])

  @`inline` final override def toSlices[T1 >: T]: (IntSlice, Slice[T1]) =
    (arrayTree.structure, arrayTree.content.asInstanceOf[Slice[T1]]) // safe cast as Slice is read-only structure

  final def asSlices: (IntSlice, Slice[T]) = (arrayTree.structure, arrayTree.content)

  @`inline` final override def toBuffers[T1 >: T]: (IntBuffer, Buffer[T1]) =
    (arrayTree.structure.asBuffer, arrayTree.content.toBuffer[T1])

  @`inline` final override def toStructureArray: Array[Int] = arrayTree.structure.toArray

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
        arrayTree.structure.top,
        arrayTree.structure,
        arrayTree.content,
        show,
        valueSeparator,
        branchSeparator,
        branchStart,
        branchEnd,
        maxDepth
      )
      .toString()

}
