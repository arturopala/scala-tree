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
  * Extracted from the [[Tree]] to de-clutter its codebase.
  */
trait EmptyTreeLike extends TreeLike[Nothing] {

  final override val size: Int = 0
  final override val width: Int = 0
  final override val height: Int = 0
  final override val isLeaf: Boolean = false
  final override val isEmpty: Boolean = true
  final override val childrenCount: Int = 0

  final override def head: Nothing = throw new NoSuchElementException

  final override val headOption: Option[Nothing] = None

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[Nothing] =
    Iterable.empty

  final override def leaves: Iterable[Nothing] = Iterable.empty

  def valuesWithFilter(
    pred: Nothing => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Nothing] = Iterable.empty

  def valuesAndLevelsWithFilter(
    pred: Nothing => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Nothing, Boolean)] = Iterable.empty

  final override val childrenValues: Iterable[Nothing] =
    Iterable.empty

  final override val children: Iterable[Tree[Nothing]] =
    Iterable.empty

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[Nothing]] =
    Iterable.empty

  final override def treesWithFilter(
    pred: Tree[Nothing] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[Nothing]] = Iterable.empty

  def treesAndLevelsWithFilter(
    pred: Tree[Nothing] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[Nothing])] = Iterable.empty

  final override val branches: Iterable[Iterable[Nothing]] =
    Iterable.empty

  final override def branchesWithFilter(
    pred: Iterable[Nothing] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[Nothing]] = Iterable.empty

  final override def countBranches(pred: Iterable[Nothing] => Boolean): Int = 0

  // SELECTIONS

  final override def selectValue[K](
    path: Iterable[K],
    toPathItem: Nothing => K,
    rightmost: Boolean = false
  ): Option[Nothing] = None

  final override def selectTree[T1: ClassTag](path: Iterable[T1], rightmost: Boolean = false): Option[Tree[Nothing]] =
    None

  final override def selectTree[K](
    path: Iterable[K],
    toPathItem: Nothing => K,
    rightmost: Boolean
  ): Option[Tree[Nothing]] = None

  final override def containsChild[T1](value: T1): Boolean = false

  final override def containsBranch[T1](branch: Iterable[T1]): Boolean = false

  final override def containsBranch[K](branch: Iterable[K], toPathItem: Nothing => K): Boolean = false

  final override def containsPath[T1](path: Iterable[T1]): Boolean = false

  final override def containsPath[K](path: Iterable[K], toPathItem: Nothing => K): Boolean = false

  // INSERTIONS

  final override def prepend[T1: ClassTag](value: T1): Tree[T1] = Tree(value)

  final override def insertLeaf[T1: ClassTag](value: T1, append: Boolean = false): Tree[T1] = Tree(value)

  final override def insertLeaves[T1: ClassTag](values: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (values.size == 1) Tree(values.head) else Tree.empty

  final override def insertLeafAt[T1: ClassTag](path: Iterable[T1], value: T1, append: Boolean = false): Tree[T1] =
    Tree.empty.insertBranch(path.toList :+ value)

  final override def insertLeafAt[K, T1: ClassTag](
    path: Iterable[K],
    value: T1,
    toPathItem: Nothing => K,
    append: Boolean
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def insertChild[T1: ClassTag](child: Tree[T1], append: Boolean = false): Tree[T1] = child

  final override def insertChildren[T1: ClassTag](children: Iterable[Tree[T1]], append: Boolean = false): Tree[T1] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.size == 1) validChildren.head else Tree.empty
  }

  final override def insertChildAt[T1: ClassTag](
    path: Iterable[T1],
    child: Tree[T1],
    append: Boolean = false
  ): Tree[T1] =
    if (path.isEmpty) child
    else if (child.isEmpty) empty
    else TreeBuilder.linearTreeFromSequence(path.toList).insertChildAt(path, child)

  final override def insertChildAt[K, T1: ClassTag](
    path: Iterable[K],
    child: Tree[T1],
    toPathItem: Nothing => K,
    append: Boolean
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def insertChildrenAt[T1: ClassTag](
    path: Iterable[T1],
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] = empty

  final override def insertChildrenAt[K, T1: ClassTag](
    path: Iterable[K],
    children: Iterable[Tree[T1]],
    toPathItem: Nothing => K,
    append: Boolean
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def insertBranch[T1: ClassTag](branch: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (branch.isEmpty) Tree.empty
    else TreeBuilder.linearTreeFromSequence(branch.toList)

  final override def insertBranches[T1 >: Nothing: ClassTag](
    branches: Iterable[Iterable[T1]],
    append: Boolean = false
  ): Tree[T1] =
    if (branches.isEmpty) Tree.empty
    else branches.foldLeft[Tree[T1]](Tree.empty)((tree, branch) => tree.insertBranch(branch, append))

  // UPDATES

  final override def updateHead[T1: ClassTag](replacement: T1): Tree[T1] =
    Tree.empty

  final override def updateChildValue[T1: ClassTag](existingValue: T1, replacement: T1): Tree[T1] =
    Tree.empty

  final override def updateValueAt[T1: ClassTag](
    path: Iterable[T1],
    replacement: T1
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def updateValueAt[K, T1: ClassTag](
    path: Iterable[K],
    replacement: T1,
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def updateChild[T1: ClassTag](value: T1, replacement: Tree[T1]): Tree[T1] =
    Tree.empty

  final override def updateTreeAt[T1: ClassTag](
    path: Iterable[T1],
    replacement: Tree[T1]
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def updateTreeAt[K, T1: ClassTag](
    path: Iterable[K],
    replacement: Tree[T1],
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  // MODIFICATIONS

  final override def modifyHead[T1: ClassTag](modify: Nothing => T1): Tree[T1] =
    Tree.empty

  final override def modifyChildValue[T1: ClassTag](value: T1, modify: Nothing => T1): Tree[T1] =
    Tree.empty

  final override def modifyValueAt[T1: ClassTag](
    path: Iterable[T1],
    modify: Nothing => T1
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyValueAt[K, T1: ClassTag](
    path: Iterable[K],
    modify: Nothing => T1,
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyChild[T1: ClassTag](value: T1, modify: Tree[Nothing] => Tree[T1]): Tree[T1] =
    Tree.empty

  final override def modifyTreeAt[T1: ClassTag](
    path: Iterable[T1],
    modify: Tree[Nothing] => Tree[T1]
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyTreeAt[K, T1: ClassTag](
    path: Iterable[K],
    modify: Tree[Nothing] => Tree[T1],
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  // REMOVALS

  final override def removeChildValue[T1: ClassTag](value: T1): Tree[Nothing] =
    Tree.empty

  final override def removeValueAt[T1: ClassTag](path: Iterable[T1]): Tree[Nothing] =
    Tree.empty

  final override def removeValueAt[K, T1: ClassTag](
    path: Iterable[K],
    toPathItem: Nothing => K
  ): Tree[Nothing] =
    Tree.empty

  final override def removeChild[T1: ClassTag](value: T1): Tree[Nothing] =
    Tree.empty

  final override def removeTreeAt[T1: ClassTag](path: Iterable[T1]): Tree[Nothing] =
    Tree.empty

  final override def removeTreeAt[K, T1: ClassTag](
    path: Iterable[K],
    toPathItem: Nothing => K
  ): Tree[Nothing] =
    Tree.empty

  final override def map[K: ClassTag](f: Nothing => K): Tree[K] = empty

  final override def toPairsIterator: Iterator[(Int, Nothing)] = Iterator.empty

  final override def toArrays[T1: ClassTag]: (Array[Int], Array[T1]) = (Array.empty[Int], Array.empty[T1])

  final override def toSlices[T1: ClassTag]: (IntSlice, Slice[T1]) = (IntSlice.empty, Slice.empty[T1])

  final override def toBuffers[T1: ClassTag]: (IntBuffer, Buffer[T1]) = (IntBuffer.empty, Buffer.empty[T1])

  final override val toStructureArray: Array[Int] = Array.empty[Int]

  final override def mkStringFromBranches(
    show: Nothing => String,
    nodeSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String = ""

  final val inflated: Tree[Nothing] = Tree.empty
  final def deflated[T1](implicit tag: ClassTag[T1]): Tree[T1] = Tree.empty

}
