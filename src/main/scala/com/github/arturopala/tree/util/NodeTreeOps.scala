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

import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.Tree.{NodeTree, empty}

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.NodeTree]] functions final implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait NodeTreeOps[+T] {

  /** [[Tree.NodeTree]] under consideration. */
  protected val node: NodeTree[T]

  final def valueOption: Option[T] = Some(node.value)
  final def isEmpty: Boolean = node.size == 0

  private final def all[A]: A => Boolean = _ => true

  final def values: List[T] = NodeTree.values[T](all, node)
  final def valuesUnsafe: List[T] = node.value :: node.subtrees.flatMap(_.valuesUnsafe)
  final def valueIterator(pred: T => Boolean): Iterator[T] = NodeTree.valueIterator(pred, node)
  final def valueStream: Stream[T] = valueStream(all)
  final def valueStream(pred: T => Boolean): Stream[T] = NodeTree.valueStream(pred, node)
  final def childrenValues: List[T] = node.subtrees.map(_.value)

  final def children: List[Tree[T]] = node.subtrees
  final def trees: List[Tree[T]] = NodeTree.trees[T](all, node)
  final def treesUnsafe: List[Tree[T]] = node :: node.subtrees.flatMap(_.treesUnsafe)
  final def treeIterator(pred: Tree[T] => Boolean): Iterator[Tree[T]] = NodeTree.treeIterator(pred, node)
  final def treeStream: Stream[Tree[T]] = treeStream(all)
  final def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]] = NodeTree.treeStream(pred, node)

  final def branches: List[List[T]] = NodeTree.branches[T](all, node)

  final def branchesUnsafe: List[List[T]] = node.subtrees match {
    case Nil => List(List(node.value))
    case _ =>
      node.subtrees.flatMap(_.branchesUnsafe).map(node.value :: _)
  }

  final def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]] =
    NodeTree.branchIterator(pred, node)

  final def branchStream: Stream[List[T]] = branchStream(all).map(_.toList)

  final def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]] =
    NodeTree.branchStream(pred, node)

  final def countBranches(pred: Iterable[T] => Boolean): Int =
    NodeTree.countBranches(pred, node)

  final def insertValue[T1 >: T: ClassTag](newValue: T1): Tree[T1] =
    Tree(node.value, Tree(newValue) :: node.subtrees)

  final def insertTree[T1 >: T: ClassTag](tree: Tree[T1]): Tree[T1] = tree match {
    case `empty`         => node
    case n: NodeTree[T1] => Tree(node.value, n :: node.subtrees)
  }

  final def insertBranch[T1 >: T: ClassTag](branch: List[T1]): Tree[T1] =
    branch match {
      case value :: xs if value == node.value => NodeTree.insert(node, xs)
      case _                                  => node
    }

  final def map[K: ClassTag](f: T => K): Tree[K] = {
    val (structure, values) = NodeTree.arrayMap(f, node)
    TreeBuilder.fromIterators(structure.iterator, values.iterator).headOption.getOrElse(empty)
  }

  final def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = {
    def mapNodeUnsafe(n: NodeTree[T]): NodeTree[K] = Tree(f(n.value), n.subtrees.map(mapNodeUnsafe))
    mapNodeUnsafe(node)
  }

  final def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] = {
    val list: List[(Int, Tree[K])] = NodeTree.listFlatMap(f, List((node.subtrees.size, f(node.value))), node.subtrees)
    TreeBuilder.fromTreeList(list, Nil, 0, TreeBuilder.TreeMergeStrategy.Join).headOption.getOrElse(empty)
  }

  final def selectValue[T1 >: T](path: Iterable[T1]): Option[T] =
    NodeTree.selectTree(node, path).map(_.value)

  final def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    NodeTree.selectTree(node, path)

  final def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean = NodeTree.containsBranch(node, branch)
  final def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(node, path)

  final def toPairsIterator: Iterator[(Int, T)] = NodeTree.toPairsList(node).iterator
  final def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) = NodeTree.toArrays(node)
  final def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = NodeTree.toSlices(node)
  final def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1]) = NodeTree.toBuffers(node)
  final def toStructureArray: Array[Int] = NodeTree.toStructureArray(node)

  final def mkStringUsingBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String = {
    val string = show(node.value)
    node.subtrees match {
      case Nil => branchStart + string + branchEnd
      case _ =>
        NodeTree
          .mkStringUsingBranches(node, show, valueSeparator, branchSeparator, branchStart, branchEnd, maxDepth)
          .mkString
    }
  }

  final def inflated: Tree[T] = node
  final def deflated[T1 >: T](implicit tag: ClassTag[T1]): Tree[T1] = Tree.deflate[T1](node)

}
