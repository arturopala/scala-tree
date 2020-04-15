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
import com.github.arturopala.tree.Tree.{NodeTree, empty}

import scala.collection.Iterator
import scala.collection.immutable.Stream
import scala.reflect.ClassTag

/**
  * [[Tree.NodeTree]] functions final implementations.
  * Extracted from [[Tree]] to de-clutter its codebase.
  */
trait NodeTreeLike[+T] extends TreeLike[T] {

  /** [[Tree.NodeTree]] under consideration. */
  protected val node: NodeTree[T]

  final override def valueOption: Option[T] = Some(node.value)
  final def isEmpty: Boolean = node.size == 0

  @`inline` final def all[A]: A => Boolean = _ => true
  @`inline` final def identity[A, A1 >: A]: A => A1 = x => x

  final override def values: List[T] = NodeTree.values[T](all, node)
  final def valuesUnsafe: List[T] = node.value :: node.subtrees.flatMap(_.valuesUnsafe)
  final override def valueIterator(pred: T => Boolean, maxDepth: Int = Int.MaxValue): Iterator[T] =
    if (maxDepth == Int.MaxValue) NodeTree.valueIterator(pred, node)
    else NodeTree.valueIteratorWithLimit(pred, node, maxDepth)

  final override def valueStream: Stream[T] = valueStream(all)
  final override def valueStream(pred: T => Boolean): Stream[T] = NodeTree.valueStream(pred, node)
  final override def childrenValues: List[T] = node.subtrees.map(_.value)
  final override def children: List[Tree[T]] = node.subtrees
  final override def trees: List[Tree[T]] = NodeTree.trees[T](all, node)
  final def treesUnsafe: List[Tree[T]] = node :: node.subtrees.flatMap(_.treesUnsafe)
  final override def treeIterator(pred: Tree[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Tree[T]] =
    if (maxDepth == Int.MaxValue) NodeTree.treeIterator(pred, node)
    else NodeTree.treeIteratorWithLimit(pred, node, maxDepth)

  final override def treeStream: Stream[Tree[T]] = treeStream(all)
  final override def treeStream(pred: Tree[T] => Boolean): Stream[Tree[T]] = NodeTree.treeStream(pred, node)

  final override def branches: List[List[T]] = NodeTree.branches[T](all, node)

  final def branchesUnsafe: List[List[T]] = node.subtrees match {
    case Nil => List(List(node.value))
    case _ =>
      node.subtrees.flatMap(_.branchesUnsafe).map(node.value :: _)
  }

  final override def branchIterator(pred: Iterable[T] => Boolean): Iterator[Iterable[T]] =
    NodeTree.branchIterator(pred, node)

  final override def branchStream: Stream[List[T]] = branchStream(all).map(_.toList)

  final override def branchStream(pred: Iterable[T] => Boolean): Stream[Iterable[T]] =
    NodeTree.branchStream(pred, node)

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    NodeTree.countBranches(pred, node)

  final override def insertValue[T1 >: T: ClassTag](newValue: T1): NodeTree[T1] =
    Tree(node.value, Tree(newValue) :: node.subtrees)

  final override def insertTree[T1 >: T: ClassTag](tree: Tree[T1]): Tree[T1] = tree match {
    case `empty`         => node
    case n: NodeTree[T1] => Tree(node.value, n :: node.subtrees)
  }

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    if (branch.isEmpty) node
    else {
      val iterator = branch.iterator
      val value = iterator.next
      if (node.value == value) NodeTree.insertBranch(node, iterator) match {
        case None       => node
        case Some(tree) => tree
      }
      else node
    }

  final override def map[K: ClassTag](f: T => K): Tree[K] = {
    val (structure, values) = NodeTree.arrayMap(f, node)
    TreeBuilder.fromIterators(structure.iterator, values.iterator).headOption.getOrElse(empty)
  }

  final def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = {
    def mapNodeUnsafe(n: NodeTree[T]): NodeTree[K] = Tree(f(n.value), n.subtrees.map(mapNodeUnsafe))
    mapNodeUnsafe(node)
  }

  final override def flatMap[K: ClassTag](f: T => Tree[K]): Tree[K] = {
    val list: List[(Int, Tree[K])] = NodeTree.listFlatMap(f, List((node.subtrees.size, f(node.value))), node.subtrees)
    TreeBuilder.fromTreeList(list, Nil, 0, TreeBuilder.TreeMergeStrategy.Join).headOption.getOrElse(empty)
  }

  final override def selectValue[K](path: Iterable[K], f: T => K): Option[T] =
    NodeTree.select(node, path, f, (n: NodeTree[T]) => n.value)

  final override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    NodeTree.select(node, path, identity[T, T1], (n: NodeTree[T]) => n)

  final override def selectTree[K](path: Iterable[K], f: T => K): Option[Tree[T]] =
    NodeTree.select(node, path, f, (n: NodeTree[T]) => n)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean = NodeTree.containsBranch(node, branch)
  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(node, path)
  final override def toPairsIterator: Iterator[(Int, T)] = NodeTree.toPairsList(node).iterator
  final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) = NodeTree.toArrays(node)
  final override def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = NodeTree.toSlices(node)
  final override def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1]) = NodeTree.toBuffers(node)
  final override def toStructureArray: Array[Int] = NodeTree.toStructureArray(node)

  final override def mkStringUsingBranches(
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
