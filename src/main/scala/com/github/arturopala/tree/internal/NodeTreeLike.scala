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

import com.github.arturopala.tree.{Tree, TreeBuilder, TreeLike}
import com.github.arturopala.tree.Tree.{ArrayTree, NodeTree, empty}
import com.github.arturopala.bufferandslice.{Buffer, IntBuffer, IntSlice, Slice}

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.NodeTree]] final functions set.
  * Extracted from the [[Tree]] to de-clutter its codebase.
  */
trait NodeTreeLike[+T] extends TreeLike[T] {

  /** [[Tree.NodeTree]] under consideration. */
  protected val node: NodeTree[T]

  final override def headOption: Option[T] = Some(node.head)
  final def isEmpty: Boolean = node.size == 0

  @`inline` final def all[A]: A => Boolean = _ => true
  @`inline` final def identity[A, A1 >: A]: A => A1 = x => x

  final override def values: List[T] = NodeTree.values[T](all, node)
  final def valuesUnsafe: List[T] = node.head :: node.children.flatMap(_.valuesUnsafe)
  final override def valueIterator(pred: T => Boolean, maxDepth: Int = Int.MaxValue): Iterator[T] =
    if (maxDepth >= height) NodeTree.valueIterator(pred, node)
    else NodeTree.valueIteratorWithLimit(pred, node, maxDepth)

  final override def childrenValues: List[T] = node.children.map(_.head)
  final override def trees: List[Tree[T]] = NodeTree.trees[T](all, node)
  final def treesUnsafe: List[Tree[T]] = node :: node.children.flatMap(_.treesUnsafe)
  final override def treeIterator(pred: Tree[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Tree[T]] =
    if (maxDepth >= height) NodeTree.treeIterator(pred, node)
    else NodeTree.treeIteratorWithLimit(pred, node, maxDepth)

  final override def branches: Seq[Iterable[T]] = NodeTree.branches[T](all, node)

  final def branchesUnsafe: List[List[T]] = node.children match {
    case Nil => List(List(node.head))
    case _ =>
      node.children.flatMap(_.branchesUnsafe).map(node.head :: _)
  }

  final override def branchIterator(pred: Iterable[T] => Boolean, maxDepth: Int = Int.MaxValue): Iterator[Iterable[T]] =
    if (maxDepth >= height) NodeTree.branchIterator(pred, node)
    else NodeTree.branchIteratorWithLimit(pred, node, maxDepth)

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    NodeTree.countBranches(pred, node)

  // DISTINCT INSERTIONS

  final override def prepend[T1 >: T: ClassTag](value: T1): Tree[T1] = Tree(value, node)

  final override def insertValue[T1 >: T: ClassTag](value: T1): Tree[T1] =
    if (node.children.exists(_.head == value)) node
    else Tree(node.head, Tree(value) :: node.children)

  final override def insertValueAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] =
    NodeTree.insertTreeAt(node, path.iterator, Tree(value), keepDistinct = true).getOrElse(node)

  final override def insertValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    f: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.insertTreeAt(node, path.iterator, f, Tree(value), keepDistinct = true)

  final override def insertTree[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] = subtree match {
    case Tree.empty         => node
    case tree: NodeTree[T1] => NodeTree.insertTreeDistinct(node, tree)
    case tree: ArrayTree[T1] =>
      if (Tree.preferInflated(node, tree))
        NodeTree.insertTreeDistinct(node, tree.inflated.asInstanceOf[NodeTree[T1]])
      else node.deflated[T1].insertTree(tree)
  }

  final override def insertTreeAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] =
    subtree match {
      case Tree.empty         => node
      case tree: NodeTree[T1] => NodeTree.insertTreeAt(node, path.iterator, tree, keepDistinct = true).getOrElse(node)
      case tree: ArrayTree[T1] =>
        if (Tree.preferInflated(node, tree))
          NodeTree
            .insertTreeAt(node, path.iterator, tree.inflated.asInstanceOf[NodeTree[T1]], keepDistinct = true)
            .getOrElse(node)
        else node.deflated[T1].insertTreeAt(path, tree)
    }

  final override def insertTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    subtree: Tree[T1],
    f: T => K
  ): Either[Tree[T], Tree[T1]] = subtree match {
    case Tree.empty         => Left(node)
    case tree: NodeTree[T1] => NodeTree.insertTreeAt(node, path.iterator, f, tree, keepDistinct = true)
    case tree: ArrayTree[T1] =>
      NodeTree.insertTreeAt(node, path.iterator, f, tree.inflated.asInstanceOf[NodeTree[T1]], keepDistinct = true)
  }

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    NodeTree.insertBranch(node, branch.iterator).getOrElse(node)

  // DISTINCT MODIFICATIONS

  final override def modifyValue[T1 >: T: ClassTag](value: T1, modify: T => T1): Tree[T1] =
    NodeTree.modifyValue(node, value, modify, keepDistinct = true)

  final override def modifyValueAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyValueAt(node, path.iterator, modify, keepDistinct = true)

  final override def modifyValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyValueAt(node, path.iterator, toPathItem, modify, keepDistinct = true)

  final override def modifyTree[T1 >: T: ClassTag](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    NodeTree.modifyTree(node, value, modify, keepDistinct = true)

  final override def modifyTreeAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyTreeAt(node, path.iterator, modify, keepDistinct = true)

  final override def modifyTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyTreeAt(node, path.iterator, toPathItem, modify, keepDistinct = true)

  // DISTINCT REMOVALS

  final override def removeValue[T1 >: T: ClassTag](value: T1): Tree[T] =
    NodeTree.removeValue(node, value, keepDistinct = true)

  final override def removeValueAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, keepDistinct = true)

  final override def removeValueAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, toPathItem, keepDistinct = true)

  final override def removeTree[T1 >: T: ClassTag](value: T1): Tree[T] =
    NodeTree.removeTree(node, value)

  final override def removeTreeAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    NodeTree.removeTreeAt(node, path.iterator)

  final override def removeTreeAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeTreeAt(node, path.iterator, toPathItem)

  // TRANSFORMATIONS

  final override def map[K: ClassTag](f: T => K): Tree[K] = {
    val (structure, values) = NodeTree.arrayMap(f, node)
    TreeBuilder.fromIterators(structure.iterator, values.iterator).headOption.getOrElse(empty)
  }

  final def mapUnsafe[K: ClassTag](f: T => K): Tree[K] = {
    def mapNodeUnsafe(n: NodeTree[T]): NodeTree[K] = Tree(f(n.head), n.children.map(mapNodeUnsafe))
    mapNodeUnsafe(node)
  }

  final override def selectValue[K](path: Iterable[K], f: T => K): Option[T] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n.head, f)

  final override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n)

  final override def selectTree[K](path: Iterable[K], f: T => K): Option[Tree[T]] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n, f)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    NodeTree.containsBranch(node, branch)

  final override def containsBranch[K](branch: Iterable[K], f: T => K): Boolean =
    NodeTree.containsBranch(node, branch, f)

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(node, path)
  final override def containsPath[K](path: Iterable[K], f: T => K): Boolean = NodeTree.containsPath(node, path, f)

  final override def toPairsIterator: Iterator[(Int, T)] = NodeTree.toPairsList(node).iterator
  final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) = NodeTree.toArrays(node)
  final override def toSlices[T1 >: T: ClassTag]: (IntSlice, Slice[T1]) = NodeTree.toSlices(node)
  final override def toBuffers[T1 >: T: ClassTag]: (IntBuffer, Buffer[T1]) = NodeTree.toBuffers(node)
  final override def toStructureArray: Array[Int] = NodeTree.toStructureArray(node)

  final override def mkStringFromBranches(
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int = Int.MaxValue
  ): String = {
    val string = show(node.head)
    node.children match {
      case Nil => branchStart + string + branchEnd
      case _ =>
        NodeTree
          .mkStringUsingBranches(node, show, valueSeparator, branchSeparator, branchStart, branchEnd, maxDepth)
          .mkString
    }
  }

  final def inflated: Tree[T] = node
  final def deflated[T1 >: T](implicit tag: ClassTag[T1]): ArrayTree[T1] = {
    val (structure, values) = node.toSlices[T1]
    new ArrayTree[T1](structure, values, node.width, node.height)
  }

}
