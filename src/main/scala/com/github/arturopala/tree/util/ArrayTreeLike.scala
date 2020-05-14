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

import com.github.arturopala.tree.{Tree, TreeBuilder, TreeLike}
import com.github.arturopala.tree.Tree.{ArrayTree, NodeTree}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.ArrayTree]] final functions set.
  * Extracted from the [[Tree]] to de-clutter its codebase.
  */
abstract class ArrayTreeLike[T: ClassTag] extends TreeLike[T] {

  protected val tree: ArrayTree[T]

  private def all[A]: A => Boolean = _ => true

  final override def value: T = tree.content.last
  final override def valueOption: Option[T] = Some(tree.content.last)
  final override def values: Seq[T] = tree.content.reverseIterator.toSeq
  final override def valueIterator(pred: T => Boolean, maxDepth: Int = Int.MaxValue): Iterator[T] =
    if (maxDepth >= height) tree.content.reverseIterator(pred)
    else
      ArrayTree.valueIterator(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def childrenValues: Seq[T] =
    ArrayTreeFunctions
      .childrenIndexes(tree.structure.length - 1, tree.structure)
      .map(tree.content)
      .toSeq

  final override def children: Seq[Tree[T]] =
    ArrayTreeFunctions
      .childrenIndexes(tree.structure.length - 1, tree.structure)
      .map(ArrayTree.treeAt(_, tree.structure, tree.content))
      .toSeq

  final override def trees: Seq[Tree[T]] = treeIterator(all).toSeq

  final override def treeIterator(pred: Tree[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Tree[T]] =
    if (maxDepth >= height) ArrayTree.treeIterator(tree.structure.length - 1, tree.structure, tree.content, pred)
    else ArrayTree.treeIteratorWithLimit(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def branches: Seq[Iterable[T]] = branchIterator(all).toSeq
  final override def branchIterator(pred: Iterable[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Iterable[T]] =
    ArrayTree.branchIterator(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(tree.structure.length - 1, tree.structure, tree.content, pred)

  // MODIFICATIONS

  final override def prependWith[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.prependValue(value, tree)

  final override def insertValueLax[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertValue(tree.structure.length - 1, value, tree, keepDistinct = false)

  final override def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertValue(tree.structure.length - 1, value, tree, keepDistinct = true)

  final override def insertValueLaxAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    ArrayTree.insertValueAt(path, value, tree, keepDistinct = false)

  final override def insertValueAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    ArrayTree.insertValueAt(path, value, tree, keepDistinct = true)

  final override def insertValueLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    f: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertValueAt(path, value, tree, f, keepDistinct = false)

  final override def insertValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    f: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertValueAt(path, value, tree, f, keepDistinct = true)

  final override def insertTreeLax[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTree(tree.structure.length - 1, subtree, tree)

  final override def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeDistinct(tree.structure.length - 1, subtree, tree)

  final override def insertTreeLaxAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeAt(path, subtree, tree, keepDistinct = false)

  final override def insertTreeAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeAt(path, subtree, tree, keepDistinct = true)

  final override def insertTreeLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertTreeAt(path, subtree, tree, f, keepDistinct = false)

  final override def insertTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertTreeAt(path, subtree, tree, f, keepDistinct = true)

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    ArrayTree.insertBranch(tree.structure.length - 1, branch, tree)

  // MODIFICATIONS

  final override def modifyValueLaxAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, keepDistinct = false)

  final override def modifyValueAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, keepDistinct = true)

  final override def modifyValueLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, toPathItem, keepDistinct = false)

  final override def modifyValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyValueAt(path, modify, tree, toPathItem, keepDistinct = true)

  final override def modifyTreeLaxAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, keepDistinct = false)

  final override def modifyTreeAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, keepDistinct = true)

  final override def modifyTreeLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, keepDistinct = false)

  final override def modifyTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, keepDistinct = true)

  // REMOVALS

  final override def removeValueAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    ArrayTree.removeValueAt(path, tree, keepDistinct = true)

  final override def removeValueAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    ArrayTree.removeValueAt(path, tree, toPathItem, keepDistinct = true)

  // TRANSFORMATIONS

  final override def map[K: ClassTag](f: T => K): Tree[K] =
    new ArrayTree[K](tree.structure, tree.content.map(f), tree.width, tree.height)

  final override def flatMapLax[K: ClassTag](f: T => Tree[K]): Tree[K] =
    ArrayTree.flatMap(tree.structure, tree.content, f)

  // PATH-BASED OPERATIONS

  final override def selectValue[K](path: Iterable[K], f: T => K): Option[T] =
    ArrayTree.selectValue(path, tree.structure.length - 1, tree.structure, tree.content, f)

  final override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    ArrayTree.selectTree(path, tree.structure.length - 1, tree.structure, tree.content)

  final override def selectTree[K](path: Iterable[K], f: T => K): Option[Tree[T]] =
    ArrayTree.selectTree(path, tree.structure.length - 1, tree.structure, tree.content, f)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    ArrayTree.containsBranch(branch, tree.structure.length - 1, tree.structure, tree.content)

  final override def containsBranch[K](branch: Iterable[K], f: T => K): Boolean =
    ArrayTree.containsBranch(branch, tree.structure.length - 1, tree.structure, tree.content, f)

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
    ArrayTree.containsPath(path, tree.structure.length - 1, tree.structure, tree.content)

  final override def containsPath[K](path: Iterable[K], f: T => K): Boolean =
    ArrayTree.containsPath(path, tree.structure.length - 1, tree.structure, tree.content, f)

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
        tree.structure.length - 1,
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
