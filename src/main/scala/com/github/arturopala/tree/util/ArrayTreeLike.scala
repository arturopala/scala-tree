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
import com.github.arturopala.tree.Tree.ArrayTree

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.ArrayTree]] final functions implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
abstract class ArrayTreeLike[T: ClassTag] extends TreeLike[T] {

  protected val tree: ArrayTree[T]

  private def all[A]: A => Boolean = _ => true

  final override def valueOption: Option[T] = Some(tree.content.last)
  final override def values: List[T] = tree.content.reverseIterator.toList
  final override def valueIterator(pred: T => Boolean, maxDepth: Int = Int.MaxValue): Iterator[T] =
    if (maxDepth >= height) tree.content.reverseIterator(pred)
    else
      ArrayTree.valueIterator(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def childrenValues: List[T] =
    ArrayTree.childrenIndexes(tree.structure.length - 1, tree.structure).map(tree.content)

  final override def children: List[Tree[T]] =
    ArrayTree
      .childrenIndexes(tree.structure.length - 1, tree.structure)
      .map(ArrayTree.treeAt(_, tree.structure, tree.content))

  final override def trees: List[Tree[T]] = treeIterator(all).toList

  final override def treeIterator(pred: Tree[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Tree[T]] =
    if (maxDepth >= height) ArrayTree.treeIterator(tree.structure.length - 1, tree.structure, tree.content, pred)
    else ArrayTree.treeIteratorWithLimit(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def branches: List[List[T]] = branchIterator(all).map(_.toList).toList
  final override def branchIterator(pred: Iterable[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Iterable[T]] =
    ArrayTree.branchIterator(tree.structure.length - 1, tree.structure, tree.content, pred, maxDepth)

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(tree.structure.length - 1, tree.structure, tree.content, pred)

  // MODIFICATIONS

  final override def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertValue(tree.structure.length - 1, value, tree, keepDistinct = false)

  final override def insertValueDistinct[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertValue(tree.structure.length - 1, value, tree, keepDistinct = true)

  final override def insertValueAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    ArrayTree.insertValueAt(path, value, tree, keepDistinct = false)

  final override def insertValueDistinctAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    ArrayTree.insertValueAt(path, value, tree, keepDistinct = true)

  final override def insertValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    f: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertValueAt(path, value, tree, f, keepDistinct = false)

  final override def insertValueDistinctAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    f: T => K
  ): Either[Tree[T], Tree[T1]] = ArrayTree.insertValueAt(path, value, tree, f, keepDistinct = true)

  final override def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertSubtree(tree.structure.length - 1, subtree, tree)

  final override def insertTreeDistinct[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertSubtreeDistinct(tree.structure.length - 1, subtree, tree)

  final override def insertTreeAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeAt(path, subtree, tree, keepDistinct = false)

  final override def insertTreeDistinctAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertTreeAt(path, subtree, tree, keepDistinct = true)

  final override def insertTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K
  ): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertTreeAt(path, subtree, tree, f, keepDistinct = false)

  final override def insertTreeDistinctAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K): Either[Tree[T], Tree[T1]] =
    ArrayTree.insertTreeAt(path, subtree, tree, f, keepDistinct = true)

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    ArrayTree.insertBranch(tree.structure.length - 1, branch, tree)

  // TRANSFORMATIONS

  final override def map[K: ClassTag](f: T => K): Tree[K] =
    new ArrayTree[K](tree.structure, tree.content.map(f), tree.width, tree.height)

  final override def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] =
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
    (tree.structure.toBuffer, tree.content.toBuffer.asInstanceOf[Buffer[T1]])
  final override def toStructureArray: Array[Int] = tree.structure.toArray

  final override def mkStringUsingBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int
  ): String =
    ArrayTree
      .mkStringUsingBranches(
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

  private def streamFromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) {
      new Stream.Cons(it.next(), streamFromIterator(it))
    } else Stream.Empty

}
