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

import com.github.arturopala.tree.Tree
import com.github.arturopala.tree.Tree.{ArrayTree, Builder}

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.ArrayTree]] final functions implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait ArrayTreeOps[T] {

  protected val tree: ArrayTree[T]
  protected val classTag: ClassTag[T]

  private implicit def tag: ClassTag[T] = classTag

  private def all[A]: A => Boolean = _ => true

  final def valueOption: Option[T] = Some(tree.content.last)
  final def values: List[T] = tree.content.reverseIterator.toList
  final def valuesUnsafe: List[T] = values
  final def valueIterator(pred: T => Boolean): Iterator[T] = tree.content.reverseIterator(pred)
  final def valueStream: Stream[T] = valueStream(all)
  final def valueStream(pred: T => Boolean): Stream[T] = streamFromIterator(valueIterator(pred))

  final def childrenValues: List[T] =
    ArrayTree.childrenIndexes(tree.structure.length - 1, tree.structure).map(tree.content)

  final def children: List[Tree[T]] =
    ArrayTree
      .childrenIndexes(tree.structure.length - 1, tree.structure)
      .map(ArrayTree.treeAt(_, tree.structure, tree.content))

  final def trees: List[Tree[T]] = treeIterator(all).toList
  final def treesUnsafe: List[Tree[T]] = trees
  final def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]] =
    ArrayTree.treeIterator(tree.structure.length - 1, tree.structure, tree.content, pred)

  final def treeStream: Stream[Tree[T]] = treeStream(all)
  final def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]] = streamFromIterator(treeIterator(pred))

  final def branches: List[List[T]] = branchIterator(all).map(_.toList).toList
  final def branchesUnsafe: List[List[T]] = branches
  final def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]] =
    ArrayTree.branchIterator(tree.structure.length - 1, tree.structure, tree.content, pred)

  final def branchStream: Stream[List[T]] = branchStream(all).map(_.toList)
  final def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]] =
    streamFromIterator(branchIterator(pred))

  final def countBranches(pred: Iterable[T] => Boolean): Int =
    ArrayTree.countBranches(tree.structure.length - 1, tree.structure, tree.content, pred)

  final def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1] =
    ArrayTree.insertValue(tree.structure.length - 1, value, tree)

  final def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] =
    ArrayTree.insertSubtree(tree.structure.length - 1, subtree, tree)

  final def insertBranch[T1 >: T: ClassTag](branch: List[T1]): Tree[T1] = ???

  final def map[K: ClassTag](f: T => K): Tree[K] =
    new ArrayTree[K](tree.structure, tree.content.map(f), tree.width, tree.height)

  final def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = map(f)

  final def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] =
    ArrayTree.flatMap(tree.structure, tree.content, f)

  final def selectValue[T1 >: T](path: Iterable[T1]): Option[T] =
    ArrayTree.selectValue(path, tree.structure.length - 1, tree.structure, tree.content)

  final def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    ArrayTree.selectTree(path, tree.structure.length - 1, tree.structure, tree.content)

  final def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    ArrayTree.containsBranch(branch, tree.structure.length - 1, tree.structure, tree.content)

  final def containsPath[T1 >: T](path: Iterable[T1]): Boolean =
    ArrayTree.containsPath(path, tree.structure.length - 1, tree.structure, tree.content)

  final def toPairsIterator: Iterator[(Int, T)] = ???
  final def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) =
    (tree.structure.toArray, tree.content.toArray.asInstanceOf[Array[T1]])
  final def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = (tree.structure, tree.content.asInstanceOf[Slice[T1]])

  final def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1]) =
    (tree.structure.toBuffer, tree.content.toBuffer.asInstanceOf[Buffer[T1]])
  final def toStructureArray: Array[Int] = tree.structure.toArray

  final def mkStringUsingBranches(
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
    Builder.fromIterators(tree.structure.iterator, tree.content.iterator).headOption.getOrElse(Tree.empty)

  final def deflated[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1] = tree

  private def streamFromIterator[A](it: Iterator[A]): Stream[A] =
    if (it.hasNext) {
      new Stream.Cons(it.next(), streamFromIterator(it))
    } else Stream.Empty

}
