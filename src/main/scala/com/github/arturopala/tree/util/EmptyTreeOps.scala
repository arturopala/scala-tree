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
import com.github.arturopala.tree.Tree.empty

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.empty]] final functions implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait EmptyTreeOps {

  final val size: Int = 0
  final val width: Int = 0
  final val height: Int = 0
  final val isLeaf: Boolean = false
  final val isEmpty: Boolean = true
  final val childrenCount: Int = 0

  final val valueOption: Option[Nothing] = None

  final val values: List[Nothing] = Nil
  final val valuesUnsafe: List[Nothing] = Nil
  final def valueIterator(pred: Nothing => Boolean): Iterator[Nothing] = Iterator.empty
  final def valueStream: Stream[Nothing] = Stream.empty
  final def valueStream(pred: Nothing => Boolean): Stream[Nothing] = Stream.empty
  final val childrenValues: List[Nothing] = Nil

  final val children: List[Tree[Nothing]] = Nil
  final val trees: List[Tree[Nothing]] = List(empty)
  final val treesUnsafe: List[Tree[Nothing]] = List(empty)
  final def treeIterator(pred: Tree[Nothing] => Boolean): Iterator[Tree[Nothing]] = Iterator.empty
  final val treeStream: Stream[Nothing] = Stream.empty
  final def treeStream(pred: Tree[Nothing] => Boolean): Stream[Nothing] = Stream.empty

  final val branches: List[List[Nothing]] = Nil
  final val branchesUnsafe: List[List[Nothing]] = Nil
  final def branchIterator(pred: Iterable[Nothing] => Boolean): Iterator[List[Nothing]] = Iterator.empty
  final val branchStream: Stream[List[Nothing]] = Stream.empty
  final def branchStream(pred: Iterable[Nothing] => Boolean): Stream[List[Nothing]] = Stream.empty
  final def countBranches(pred: Iterable[Nothing] => Boolean): Int = 0

  final def insertValue[T1: ClassTag](value: T1): Tree[T1] = Tree(value)
  final def insertTree[T1: ClassTag](subtree: Tree[T1]): Tree[T1] = subtree
  final def insertBranch[T1: ClassTag](branch: List[T1]): Tree[T1] = branch match {
    case x :: xs => NodeTree.insert(Tree(x), xs)
    case _       => empty
  }

  final def selectValue[T1 >: Nothing](path: Iterable[T1]): Option[Nothing] = None
  final def selectTree[T1: ClassTag](path: Iterable[T1]): Option[Tree[Nothing]] =
    if (path.isEmpty) Some(empty) else None
  final def containsBranch[T1](branch: Iterable[T1]): Boolean = branch.isEmpty
  final def containsPath[T1 >: Nothing](path: Iterable[T1]): Boolean = path.isEmpty

  final def map[K: ClassTag](f: Nothing => K): Tree[K] = empty
  final def mapUnsafe[K: ClassTag](f: Nothing => K): Tree[K] = empty
  final def flatMap[K: ClassTag](f: Nothing => Tree[K]): Tree[K] = empty
  final def toPairsIterator: Iterator[(Int, Nothing)] = Iterator.empty
  final def toArrays[T1: ClassTag]: (Array[Int], Array[T1]) = (Array.empty[Int], Array.empty[T1])
  final def toSlices[T1: ClassTag]: (IntSlice, Slice[T1]) = (IntSlice.empty, Slice.empty[T1])
  final def toBuffers[T1: ClassTag]: (IntBuffer, Buffer[T1]) = (IntBuffer.empty, Buffer.empty[T1])
  final val toStructureArray: Array[Int] = Array.empty[Int]

  final def mkStringUsingBranches(
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
