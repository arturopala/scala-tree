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

import com.github.arturopala.tree.{Tree, TreeLike}
import com.github.arturopala.tree.Tree.empty

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.empty]] final override functions implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait EmptyTreeOps extends TreeLike[Nothing] {

  final override val size: Int = 0
  final override val width: Int = 0
  final override val height: Int = 0
  final override val isLeaf: Boolean = false
  final override val isEmpty: Boolean = true
  final override val childrenCount: Int = 0
  final override val valueOption: Option[Nothing] = None
  final override val values: List[Nothing] = Nil
  final override val valuesUnsafe: List[Nothing] = Nil
  final override def valueIterator(pred: Nothing => Boolean): Iterator[Nothing] = Iterator.empty
  final override def valueStream: Stream[Nothing] = Stream.empty
  final override def valueStream(pred: Nothing => Boolean): Stream[Nothing] = Stream.empty
  final override val childrenValues: List[Nothing] = Nil
  final override val children: List[Tree[Nothing]] = Nil
  final override val trees: List[Tree[Nothing]] = List(empty)
  final override val treesUnsafe: List[Tree[Nothing]] = List(empty)
  final override def treeIterator(pred: Tree[Nothing] => Boolean): Iterator[Tree[Nothing]] = Iterator.empty
  final override val treeStream: Stream[Nothing] = Stream.empty
  final override def treeStream(pred: Tree[Nothing] => Boolean): Stream[Nothing] = Stream.empty
  final override val branches: List[List[Nothing]] = Nil
  final override val branchesUnsafe: List[List[Nothing]] = Nil
  final override def branchIterator(pred: Iterable[Nothing] => Boolean): Iterator[List[Nothing]] = Iterator.empty
  final override val branchStream: Stream[List[Nothing]] = Stream.empty
  final override def branchStream(pred: Iterable[Nothing] => Boolean): Stream[List[Nothing]] = Stream.empty
  final override def countBranches(pred: Iterable[Nothing] => Boolean): Int = 0
  final override def insertValue[T1: ClassTag](value: T1): Tree[T1] = Tree(value)
  final override def insertTree[T1: ClassTag](subtree: Tree[T1]): Tree[T1] = subtree

  final override def insertBranch[T1: ClassTag](branch: Iterable[T1]): Tree[T1] =
    if (branch.isEmpty) Tree.empty
    else {
      val iterator = branch.iterator
      NodeTree.insertBranch(Tree(iterator.next()), iterator) match {
        case None       => empty
        case Some(tree) => tree
      }
    }

  final override def selectValue[K](path: Iterable[K], f: Nothing => K): Option[Nothing] = None
  final override def selectTree[T1: ClassTag](path: Iterable[T1]): Option[Tree[Nothing]] = None
  final override def selectTree[K](path: Iterable[K], f: Nothing => K): Option[Tree[Nothing]] = None
  final override def containsBranch[T1](branch: Iterable[T1]): Boolean = false
  final override def containsPath[T1 >: Nothing](path: Iterable[T1]): Boolean = false
  final override def map[K: ClassTag](f: Nothing => K): Tree[K] = empty
  final override def mapUnsafe[K: ClassTag](f: Nothing => K): Tree[K] = empty
  final override def flatMap[K: ClassTag](f: Nothing => Tree[K]): Tree[K] = empty
  final override def toPairsIterator: Iterator[(Int, Nothing)] = Iterator.empty
  final override def toArrays[T1: ClassTag]: (Array[Int], Array[T1]) = (Array.empty[Int], Array.empty[T1])
  final override def toSlices[T1: ClassTag]: (IntSlice, Slice[T1]) = (IntSlice.empty, Slice.empty[T1])
  final override def toBuffers[T1: ClassTag]: (IntBuffer, Buffer[T1]) = (IntBuffer.empty, Buffer.empty[T1])
  final override val toStructureArray: Array[Int] = Array.empty[Int]

  final override def mkStringUsingBranches(
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
