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
abstract class ArrayTreeLike[T: ClassTag] extends TreeLike[T] {

  protected val tree: ArrayTree[T]

  // VALUES

  final override def head: T = tree.content.last

  final override def headOption: Option[T] = Some(tree.content.last)

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T] =
    if (mode.isDepthFirst) iterableFrom(tree.content.reverseIterator)
    else
      iterableFrom(
        ArrayTree.valuesIterator(tree.structure.top, tree.structure, tree.content, depthFirst = false)
      )

  final override def leaves: Iterable[T] =
    iterableFrom(ArrayTree.leavesIterator(tree.structure.top, tree.structure, tree.content))

  final override def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T] = iterableFrom {
    if (mode == TopDownDepthFirst && maxDepth >= height) tree.content.reverseIterator(pred)
    else
      ArrayTree
        .valuesIteratorWithLimit(tree.structure.top, tree.structure, tree.content, pred, maxDepth, mode.isDepthFirst)
  }

  def valuesAndLevelsWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, T, Boolean)] =
    iterableFrom(
      ArrayTree
        .valuesAndLevelsIteratorWithLimit(
          tree.structure.top,
          tree.structure,
          tree.content,
          pred,
          maxDepth,
          mode.isDepthFirst
        )
    )

  final override def childrenValues: Iterable[T] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexes(tree.structure.top, tree.structure)
        .iterator
        .map(tree.content)
    )

  final override def children: Iterable[Tree[T]] =
    iterableFrom(
      ArrayTreeFunctions
        .childrenIndexes(tree.structure.top, tree.structure)
        .iterator
        .map(ArrayTree.treeAt(_, tree.structure, tree.content))
    )

  // TREES

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[T]] =
    iterableFrom(ArrayTree.treesIterator(tree.structure.top, tree.structure, tree.content, mode.isDepthFirst))

  final override def treesWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[T]] =
    iterableFrom {
      if (maxDepth >= height)
        ArrayTree.treesIteratorWithFilter(tree.structure.top, tree.structure, tree.content, pred, mode.isDepthFirst)
      else
        ArrayTree
          .treesIteratorWithLimit(tree.structure.top, tree.structure, tree.content, pred, maxDepth, mode.isDepthFirst)
    }

  def treesAndLevelsWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[T])] =
    iterableFrom(
      ArrayTree.treesAndLevelsIteratorWithLimit(
        tree.structure.top,
        tree.structure,
        tree.content,
        pred,
        maxDepth,
        mode.isDepthFirst
      )
    )

  // BRANCHES

  final override def branches: Iterable[Iterable[T]] =
    iterableFrom(ArrayTree.branchesIterator(tree.structure.top, tree.structure, tree.content))

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom(
      if (maxDepth >= height)
        ArrayTree.branchesIteratorWithFilter(tree.structure.top, tree.structure, tree.content, pred)
      else ArrayTree.branchesIteratorWithLimit(tree.structure.top, tree.structure, tree.content, pred, maxDepth)
    )

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(tree.structure.top, tree.structure, tree.content, pred)

  // MODIFICATIONS

  final override def prepend[T1 >: T: ClassTag](value: T1): ArrayTree[T1] =
    ArrayTree.prepend(value, tree)

  final override def insertLeaf[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertLeaf(tree.structure.top, value, tree, keepDistinct = true)

  final override def insertLeafAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    ArrayTree.insertLeafAt(path, value, tree, keepDistinct = true)

  final override def insertLeafAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertLeafAt(path, value, tree, toPathItem, keepDistinct = true)

  final override def insertChild[T1 >: T: ClassTag](child: Tree[T1]): Tree[T1] =
    ArrayTree.insertChildDistinct(tree.structure.top, child, tree)

  final override def insertChildAt[T1 >: T: ClassTag](path: Iterable[T1], child: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeAt(path, child, tree, keepDistinct = true)

  final override def insertChildAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    child: Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertTreeAt(path, child, tree, toPathItem, keepDistinct = true)

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    ArrayTree.insertBranch(tree.structure.top, branch, tree)

  // DISTINCT UPDATES

  final override def updateHead[T1 >: T: ClassTag](replacement: T1): Tree[T1] =
    ArrayTree.updateValue(tree.structure.top, replacement, tree, keepDistinct = false)

  final override def updateChildValue[T1 >: T: ClassTag](existingValue: T1, replacement: T1): Tree[T1] =
    ArrayTree.updateChildValue(existingValue, replacement, tree, keepDistinct = true)

  final override def updateValueAt[T1 >: T: ClassTag](path: Iterable[T1], replacement: T1): Either[Tree[T], Tree[T1]] =
    ArrayTree.updateValueAt(path, replacement, tree, keepDistinct = true)

  final override def updateValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.updateValueAt(path, replacement, tree, toPathItem, keepDistinct = true)

  // DISTINCT MODIFICATIONS

  final override def modifyHead[T1 >: T: ClassTag](modify: T => T1): Tree[T1] =
    ArrayTree.modifyValue(tree.structure.top, modify, tree, keepDistinct = false)

  final override def modifyChildValue[T1 >: T: ClassTag](value: T1, modify: T => T1): Tree[T1] =
    ArrayTree.modifyChildValue(value, modify, tree, keepDistinct = true)

  final override def modifyValueAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, keepDistinct = true)

  final override def modifyValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, toPathItem, keepDistinct = true)

  final override def modifyChild[T1 >: T: ClassTag](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    ArrayTree.modifyChild(value, modify, tree, keepDistinct = true)

  final override def modifyTreeAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, keepDistinct = true)

  final override def modifyTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, keepDistinct = true)

  // REMOVALS

  final override def removeChildValue[T1 >: T: ClassTag](value: T1): Tree[T] =
    ArrayTree.removeChildValue(value, tree, keepDistinct = true)

  final override def removeValueAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeValueAt(path, tree, keepDistinct = true)

  final override def removeValueAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeValueAt(path, tree, toPathItem, keepDistinct = true)

  final override def removeChild[T1 >: T: ClassTag](value: T1): Tree[T] =
    ArrayTree.removeChild(tree, value)

  final override def removeTreeAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeTreeAt(path, tree)

  final override def removeTreeAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeTreeAt(path, tree, toPathItem)

  // TRANSFORMATIONS

  final override def map[K: ClassTag](f: T => K): Tree[K] =
    new ArrayTree[K](tree.structure, tree.content.map(f), tree.width, tree.height)

  // PATH-BASED OPERATIONS

  final override def selectValue[K](path: Iterable[K], toPathItem: T => K): Option[T] =
    ArrayTree.selectValue(path, tree.structure.top, tree.structure, tree.content, toPathItem)

  final override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    ArrayTree.selectTree(path, tree.structure.top, tree.structure, tree.content)

  final override def selectTree[K](path: Iterable[K], toPathItem: T => K): Option[Tree[T]] =
    ArrayTree.selectTree(path, tree.structure.top, tree.structure, tree.content, toPathItem)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    ArrayTree.containsBranch(branch, tree.structure.top, tree.structure, tree.content)

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsBranch(branch, tree.structure.top, tree.structure, tree.content, toPathItem)

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
    ArrayTree.containsPath(path, tree.structure.top, tree.structure, tree.content)

  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean =
    ArrayTree.containsPath(path, tree.structure.top, tree.structure, tree.content, toPathItem)

  final override def toPairsIterator: Iterator[(Int, T)] = tree.structure.iterator.zip(tree.content.iterator)

  final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) =
    (tree.structure.toArray, tree.content.toArray.asInstanceOf[Array[T1]])

  final def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) =
    (tree.structure, tree.content.asInstanceOf[Slice[T1]])

  final override def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1]) =
    (tree.structure.toBuffer, Buffer(tree.content.toArray[T1]))

  final override def toStructureArray: Array[Int] = tree.structure.toArray

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
        tree.structure.top,
        tree.structure,
        tree.content,
        show,
        valueSeparator,
        branchSeparator,
        branchStart,
        branchEnd,
        maxDepth
      )
      .toString()

  final def inflated: Tree[T] =
    TreeBuilder.fromIterators(tree.structure.iterator, tree.content.iterator).headOption.getOrElse(Tree.empty)

  final def deflated[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1] = tree

}
