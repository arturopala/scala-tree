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

import com.github.arturopala.tree.Tree.empty
import com.github.arturopala.tree.{Tree, TreeBuilder, TreeLike}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.empty]] final override functions implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait EmptyTreeLike extends TreeLike[Nothing] {

  final override val size: Int = 0
  final override val width: Int = 0
  final override val height: Int = 0
  final override val isLeaf: Boolean = false
  final override val isEmpty: Boolean = true
  final override val childrenCount: Int = 0
  final override def value: Nothing = throw new NoSuchElementException
  final override val valueOption: Option[Nothing] = None
  final override val values: List[Nothing] = Nil

  final override def valueIterator(pred: Nothing => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Nothing] =
    Iterator.empty

  final override val childrenValues: List[Nothing] = Nil
  final override val children: List[Tree[Nothing]] = Nil
  final override val trees: List[Tree[Nothing]] = Nil

  final override def treeIterator(
    pred: Tree[Nothing] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterator[Tree[Nothing]] = Iterator.empty

  final override val branches: List[List[Nothing]] = Nil

  final override def branchIterator(
    pred: Iterable[Nothing] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterator[List[Nothing]] = Iterator.empty

  final override def countBranches(pred: Iterable[Nothing] => Boolean): Int = 0

  // MODIFICATIONS

  final override def prependValue[T1 >: Nothing: ClassTag](value: T1): Tree[T1] = Tree(value)

  final override def insertValue[T1: ClassTag](value: T1): Tree[T1] = Tree(value)

  final override def insertValueDistinct[T1: ClassTag](value: T1): Tree[T1] = Tree(value)

  final override def insertValueAt[T1: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    Tree.empty.insertBranch(path.toList :+ value)

  final override def insertValueDistinctAt[T1: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    insertValueAt(path, value)

  final override def insertValueAt[K, T1: ClassTag](
    path: Iterable[K],
    value: T1,
    f: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = Left(Tree.empty)

  final override def insertValueDistinctAt[K, T1 >: Nothing: ClassTag](
    path: Iterable[K],
    value: T1,
    f: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = insertValueAt(path, value, f)

  final override def insertTree[T1: ClassTag](subtree: Tree[T1]): Tree[T1] = subtree

  final override def insertTreeDistinct[T1: ClassTag](subtree: Tree[T1]): Tree[T1] = subtree

  final override def insertTreeAt[T1: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    if (path.isEmpty) subtree
    else if (subtree.isEmpty) empty
    else TreeBuilder.linearTreeFromList(path.toList).insertTreeAt(path, subtree)

  final override def insertTreeDistinctAt[T1 >: Nothing: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    insertTreeAt(path, subtree)

  final override def insertTreeAt[K, T1: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = if (path.isEmpty) Right(subtree) else Left(empty)

  final override def insertTreeDistinctAt[K, T1 >: Nothing: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = insertTreeAt(path, subtree, f)

  final override def insertBranch[T1: ClassTag](branch: Iterable[T1]): Tree[T1] =
    if (branch.isEmpty) Tree.empty
    else TreeBuilder.linearTreeFromList(branch.toList)

  final override def modifyValueAt[T1 >: Nothing: ClassTag](
    path: Iterable[T1],
    modify: Nothing => T1
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def modifyValueDistinctAt[T1 >: Nothing: ClassTag](
    path: Iterable[T1],
    modify: Nothing => T1
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyValueAt[K, T1 >: Nothing: ClassTag](
    path: Iterable[K],
    modify: Nothing => T1,
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] = Left(empty)

  final override def modifyValueDistinctAt[K, T1 >: Nothing: ClassTag](
    path: Iterable[K],
    modify: Nothing => T1,
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyTreeAt[T1 >: Nothing: ClassTag](
    path: Iterable[T1],
    modify: Tree[Nothing] => Tree[T1]
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyTreeDistinctAt[T1: ClassTag](
    path: Iterable[T1],
    modify: Tree[Nothing] => Tree[T1]
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyTreeAt[K, T1 >: Nothing: ClassTag](
    path: Iterable[K],
    modify: Tree[Nothing] => Tree[T1],
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def modifyTreeDistinctAt[K, T1: ClassTag](
    path: Iterable[K],
    modify: Tree[Nothing] => Tree[T1],
    toPathItem: Nothing => K
  ): Either[Tree[Nothing], Tree[T1]] =
    Left(empty)

  final override def selectValue[K](path: Iterable[K], f: Nothing => K): Option[Nothing] = None
  final override def selectTree[T1: ClassTag](path: Iterable[T1]): Option[Tree[Nothing]] = None
  final override def selectTree[K](path: Iterable[K], f: Nothing => K): Option[Tree[Nothing]] = None
  final override def containsBranch[T1](branch: Iterable[T1]): Boolean = false
  final override def containsBranch[K](branch: Iterable[K], f: Nothing => K): Boolean = false
  final override def containsPath[T1 >: Nothing](path: Iterable[T1]): Boolean = false
  final override def containsPath[K](path: Iterable[K], f: Nothing => K): Boolean = false

  final override def map[K: ClassTag](f: Nothing => K): Tree[K] = empty
  final override def flatMap[K: ClassTag](f: Nothing => Tree[K]): Tree[K] = empty

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
