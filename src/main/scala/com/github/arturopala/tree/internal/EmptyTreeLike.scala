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

import com.github.arturopala.tree.Tree.empty
import com.github.arturopala.tree.{Tree, TreeBuilder, TreeLike}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}
import com.github.arturopala.tree.TreeOptions.TraversingMode
import com.github.arturopala.tree.TreeOptions.TraversingMode.TopDownDepthFirst

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.empty]] final functions set.
  */
trait EmptyTreeLike extends TreeLike[Tree, Nothing] {

  type T = Nothing

  final override val size: Int = 0
  final override val width: Int = 0
  final override val height: Int = 0
  final override val isLeaf: Boolean = false
  final override val isEmpty: Boolean = true
  final override val childrenCount: Int = 0

  final override def head: T = throw new NoSuchElementException

  final override val headOption: Option[T] = None

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T] =
    Iterable.empty

  final override val leaves: Iterable[T] = Iterable.empty

  def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T] = Iterable.empty

  def valuesAndLevelsWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, T, Boolean)] = Iterable.empty

  final override val childrenValues: Iterable[T] = Iterable.empty

  final override val firstChildValue: Option[T] = None

  final override val lastChildValue: Option[T] = None

  final override val children: Iterable[Tree[T]] = Iterable.empty

  final override val firstChild: Option[Tree[T]] = None

  final override val lastChild: Option[Tree[T]] = None

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[T]] =
    Iterable.empty

  final override def treesWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[T]] = Iterable.empty

  def treesAndLevelsWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[T])] = Iterable.empty

  final override val paths: Iterable[Iterable[T]] = Iterable.empty

  final override def pathsWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    Iterable.empty

  final override val branches: Iterable[Iterable[T]] = Iterable.empty

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] = Iterable.empty

  final override def countBranches(pred: Iterable[T] => Boolean): Int = 0

  // SELECTIONS

  final override def selectValue[K](
    path: Iterable[K],
    toPathItem: T => K,
    rightmost: Boolean = false
  ): Option[T] = None

  final override def selectTree[T1](path: Iterable[T1], rightmost: Boolean = false): Option[Tree[T]] =
    None

  final override def selectTree[K](
    path: Iterable[K],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[Tree[T]] = None

  final override def containsValue[T1](value: T1): Boolean = false

  final override def existsValue(pred: T => Boolean): Boolean = false

  final override def containsChildValue[T1](value: T1): Boolean = false

  final override def existsChildValue(pred: T => Boolean): Boolean = false

  final override def containsChild[T1 >: T](child: Tree[T1]): Boolean = false

  final override def existsChild[T1 >: T](pred: Tree[T1] => Boolean): Boolean = false

  final override def containsBranch[T1](branch: Iterable[T1]): Boolean = false

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean = false

  final override def existsBranch(branch: Iterable[T] => Boolean): Boolean = false

  final override def existsBranch[K](branch: Iterable[K] => Boolean, toPathItem: T => K): Boolean = false

  final override def containsPath[T1](path: Iterable[T1]): Boolean = false

  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean = false

  final override def existsPath(pred: Iterable[T] => Boolean): Boolean = false

  final override def existsPath[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean = false

  // TRANSFORMATIONS

  final override def map[K](f: T => K): Tree[K] = empty

  final override def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] = empty

  // INSERTIONS

  final override def prepend[T1](value: T1): Tree[T1] = Tree(value)

  final override def insertLeaf[T1](value: T1, append: Boolean = false): Tree[T1] = Tree(value)

  final override def insertLeaves[T1](values: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (values.size == 1) Tree(values.head) else empty

  final override def insertLeafAt[T1](path: Iterable[T1], value: T1, append: Boolean = false): Tree[T1] =
    empty.insertBranch(path.toList :+ value)

  final override def insertLeafAt[K, T1](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  final override def insertChild[T1](child: Tree[T1], append: Boolean = false): Tree[T1] = child

  final override def insertChildren[T1](children: Iterable[Tree[T1]], append: Boolean = false): Tree[T1] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.size == 1) validChildren.head else empty
  }

  final override def insertChildAt[T1](
    path: Iterable[T1],
    child: Tree[T1],
    append: Boolean = false
  ): Tree[T1] =
    if (path.isEmpty) child
    else if (child.isEmpty) empty
    else TreeBuilder.linearTreeFromSequence(path.toList).insertChildAt(path, child)

  final override def insertChildAt[K, T1](
    path: Iterable[K],
    child: Tree[T1],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  final override def insertChildrenAt[T1](
    path: Iterable[T1],
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] = empty

  final override def insertChildrenAt[K, T1](
    path: Iterable[K],
    children: Iterable[Tree[T1]],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  final override def insertBranch[T1](branch: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (branch.isEmpty) empty
    else TreeBuilder.linearTreeFromSequence(branch.toList)

  final override def insertBranches[T1 >: T](
    branches: Iterable[Iterable[T1]],
    append: Boolean = false
  ): Tree[T1] =
    if (branches.isEmpty) empty
    else branches.foldLeft[Tree[T1]](empty)((tree, branch) => tree.insertBranch(branch, append))

  // UPDATES

  final override def updateHead[T1](replacement: T1): Tree[T1] =
    empty

  final override def updateChildValue[T1](existingValue: T1, replacement: T1): Tree[T1] =
    empty

  final override def updateValueAt[T1](
    path: Iterable[T1],
    replacement: T1
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def updateValueAt[K, T1](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  final override def updateChild[T1](value: T1, replacement: Tree[T1]): Tree[T1] =
    empty

  final override def updateTreeAt[T1](
    path: Iterable[T1],
    replacement: Tree[T1]
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  final override def updateTreeAt[K, T1](
    path: Iterable[K],
    replacement: Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] = Left(empty)

  // MODIFICATIONS

  final override def modifyHead[T1](modify: T => T1): Tree[T1] =
    empty

  final override def modifyChildValue[T1](value: T1, modify: T => T1): Tree[T1] =
    empty

  final override def modifyValueAt[T1](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def modifyValueAt[K, T1](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def modifyChild[T1](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    empty

  final override def modifyChildren[T1](modify: Iterable[Tree[T]] => Iterable[Tree[T1]]): Tree[T1] =
    empty

  final override def modifyTreeAt[T1](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def modifyTreeAt[K, T1](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def modifyChildrenAt[T1 >: T](
    path: Iterable[T1],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]]
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  final override def modifyChildrenAt[K, T1 >: T](
    path: Iterable[K],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    Left(empty)

  // REMOVALS

  final override def removeChildValue[T1](value: T1): Tree[T] = empty

  final override def removeValueAt[T1](path: Iterable[T1]): Tree[T] = empty

  final override def removeValueAt[K](
    path: Iterable[K],
    toPathItem: T => K
  ): Tree[T] =
    empty

  final override def removeChild[T1](value: T1): Tree[T] = empty

  final override def removeChildren[T1](): Tree[T] = empty

  final override def removeTreeAt[T1](path: Iterable[T1]): Tree[T] = empty

  final override def removeTreeAt[K](
    path: Iterable[K],
    toPathItem: T => K
  ): Tree[T] =
    empty

  final override def removeChildrenAt[T1](path: Iterable[T1]): Tree[T] = empty

  final override def removeChildrenAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    empty

  final override def toPairsIterator: Iterator[(Int, T)] = Iterator.empty

  final override def toArrays[T1: ClassTag]: (Array[Int], Array[T1]) = (Array.empty[Int], Array.empty[T1])

  final override def toSlices[T1]: (IntSlice, Slice[T1]) = (IntSlice.empty, Slice.empty[T1])

  final override def toBuffers[T1]: (IntBuffer, Buffer[T1]) = (IntBuffer.empty, Buffer.empty[T1])

  final override val toStructureArray: Array[Int] = Array.empty[Int]

  final override def mkStringFromBranches(
    show: T => String,
    nodeSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String = ""

}
