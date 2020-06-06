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
import com.github.arturopala.tree.TreeOptions.TraversingMode
import com.github.arturopala.tree.TreeOptions.TraversingMode.TopDownDepthFirst
import com.github.arturopala.tree.internal.IterableOps._

import scala.collection.Iterator
import scala.reflect.ClassTag

/**
  * The [[Tree.NodeTree]] final functions set.
  * Extracted from the [[Tree]] to de-clutter its codebase.
  */
trait NodeTreeLike[+T] extends TreeLike[T] {

  /** [[Tree.NodeTree]] under consideration. */
  protected val node: NodeTree[T]

  @`inline` final def isEmpty: Boolean = node.size == 0

  // VALUES

  @`inline` final override def headOption: Option[T] = Some(node.head)

  final override def values(mode: TraversingMode = TopDownDepthFirst): Iterable[T] =
    iterableFrom(NodeTree.valuesIterator(node, mode.isDepthFirst))

  final def valuesUnsafe: Iterable[T] = node.head +: node.children.flatMap(_.valuesUnsafe)

  final override def leaves: Iterable[T] =
    iterableFrom(NodeTree.leavesIterator(node))

  final override def valuesWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[T] = iterableFrom {
    if (maxDepth >= height) NodeTree.valuesIteratorWithFilter(pred, node, mode.isDepthFirst)
    else NodeTree.valuesIteratorWithLimit(pred, node, maxDepth, mode.isDepthFirst)
  }

  def valuesAndLevelsWithFilter(
    pred: T => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, T, Boolean)] =
    iterableFrom(NodeTree.valuesAndLevelsIteratorWithFilter(pred, node, maxDepth, mode.isDepthFirst))

  final override def childrenValues: Iterable[T] = iterableFrom(node.children.iterator.map(_.head))

  // TREES

  final override def trees(mode: TraversingMode = TopDownDepthFirst): Iterable[Tree[T]] =
    iterableFrom(NodeTree.treesIterator(node, mode.isDepthFirst))

  final def treesUnsafe: Iterable[Tree[T]] = node +: node.children.flatMap(_.treesUnsafe)

  final override def treesWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Tree[T]] =
    iterableFrom {
      if (maxDepth >= height) NodeTree.treesIteratorWithFilter(pred, node, mode.isDepthFirst)
      else NodeTree.treesIteratorWithLimit(pred, node, maxDepth, mode.isDepthFirst)
    }

  def treesAndLevelsWithFilter(
    pred: Tree[T] => Boolean,
    mode: TraversingMode = TopDownDepthFirst,
    maxDepth: Int = Int.MaxValue
  ): Iterable[(Int, Tree[T])] =
    iterableFrom(NodeTree.treesAndLevelsIteratorWithFilter(pred, node, maxDepth, mode.isDepthFirst))

  // BRANCHES

  final override def branches: Iterable[Iterable[T]] = iterableFrom(NodeTree.branchesIterator(node))

  final def branchesUnsafe: Seq[Seq[T]] = node.children match {
    case Nil => List(List(node.head))
    case _ =>
      node.children.flatMap(_.branchesUnsafe).map(node.head +: _)
  }

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom {
      if (maxDepth >= height) NodeTree.branchesIteratorWithFilter(pred, node)
      else NodeTree.branchesIteratorWithLimit(pred, node, maxDepth)
    }

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    NodeTree.countBranches(pred, node)

  // DISTINCT INSERTIONS

  final override def prepend[T1 >: T: ClassTag](value: T1): Tree[T1] = Tree(value, node)

  final override def insertLeaf[T1 >: T: ClassTag](value: T1, append: Boolean = false): Tree[T1] =
    if (node.children.exists(_.head == value)) node
    else Tree(node.head, if (append) node.children :+ Tree(value) else Tree(value) +: node.children)

  final override def insertLeaves[T1 >: T: ClassTag](values: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (values.isEmpty) node
    else {
      val distinctLeafs = values.filterNot(node.containsChild)
      if (distinctLeafs.isEmpty) node
      else
        Tree(
          node.head,
          if (append) node.children ++ distinctLeafs.map(Tree.apply[T1])
          else distinctLeafs.map(Tree.apply[T1]) ++: node.children
        )
    }

  final override def insertLeafAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1, append: Boolean = false): Tree[T1] =
    NodeTree.insertChildAt(node, path.iterator, Tree(value), append, keepDistinct = true).getOrElse(node)

  final override def insertLeafAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.insertChildAt(node, path.iterator, toPathItem, Tree(value), append, keepDistinct = true)

  final override def insertChild[T1 >: T: ClassTag](child: Tree[T1], append: Boolean = false): Tree[T1] = child match {
    case Tree.empty         => node
    case tree: NodeTree[T1] => NodeTree.insertChildDistinct(node, tree, append)
    case tree: ArrayTree[T1] =>
      if (Tree.preferInflated(node, tree))
        NodeTree.insertChildDistinct(node, tree.inflated.asInstanceOf[NodeTree[T1]], append)
      else node.deflated[T1].insertChild(tree, append)
  }

  final override def insertChildren[T1 >: T: ClassTag](
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.isEmpty) node
    else if (validChildren.size == 1) node.insertChild(children.head, append)
    else if (validChildren.forall(_.isInstanceOf[NodeTree[T1]]))
      if (append)
        NodeTree.insertChildrenDistinct(
          node.head,
          node.children,
          validChildren.asInstanceOf[Iterable[NodeTree[T1]]],
          Nil,
          preserveExisting = true
        )
      else
        NodeTree.insertChildrenDistinct(
          node.head,
          Nil,
          validChildren.asInstanceOf[Iterable[NodeTree[T1]]],
          node.children,
          preserveExisting = true
        )
    else if (append)
      ArrayTree.insertAfterChildren(node, children, keepDistinct = true)
    else
      ArrayTree.insertBeforeChildren(node, children, keepDistinct = true)
  }

  final override def insertChildAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    child: Tree[T1],
    append: Boolean = false
  ): Tree[T1] =
    child match {
      case Tree.empty => node
      case tree: NodeTree[T1] =>
        NodeTree.insertChildAt(node, path.iterator, tree, append, keepDistinct = true).getOrElse(node)
      case tree: ArrayTree[T1] =>
        if (Tree.preferInflated(node, tree))
          NodeTree
            .insertChildAt(node, path.iterator, tree.inflated.asInstanceOf[NodeTree[T1]], append, keepDistinct = true)
            .getOrElse(node)
        else node.deflated[T1].insertChildAt(path, tree)
    }

  final override def insertChildAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    child: Tree[T1],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = child match {
    case Tree.empty => Left(node)
    case tree: NodeTree[T1] =>
      NodeTree.insertChildAt(node, path.iterator, toPathItem, tree, append, keepDistinct = true)
    case tree: ArrayTree[T1] =>
      NodeTree
        .insertChildAt(
          node,
          path.iterator,
          toPathItem,
          tree.inflated.asInstanceOf[NodeTree[T1]],
          append,
          keepDistinct = true
        )
  }

  final override def insertChildrenAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.isEmpty) node
    else if (validChildren.size == 1) node.insertChild(children.head, append)
    else if (validChildren.forall(_.isInstanceOf[NodeTree[T1]]))
      NodeTree
        .insertChildrenAt(
          node,
          path.iterator,
          children.asInstanceOf[Iterable[NodeTree[T1]]],
          append,
          keepDistinct = true
        )
        .getOrElse(node)
    else
      ArrayTree.insertChildrenAt(path, children, node.deflated[T1], append, keepDistinct = true)
  }

  final override def insertChildrenAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    children: Iterable[Tree[T1]],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = ???

  final override def insertBranch[T1 >: T: ClassTag](branch: Iterable[T1]): Tree[T1] =
    NodeTree.insertBranch(node, branch.iterator).getOrElse(node)

  // DISTINCT UPDATES

  final override def updateHead[T1 >: T: ClassTag](replacement: T1): Tree[T1] =
    Tree(replacement, node.children)

  final override def updateChildValue[T1 >: T: ClassTag](existingValue: T1, replacement: T1): Tree[T1] =
    NodeTree.updateChildValue(node, existingValue, replacement, keepDistinct = true)

  final override def updateValueAt[T1 >: T: ClassTag](path: Iterable[T1], replacement: T1): Either[Tree[T], Tree[T1]] =
    NodeTree.updateValueAt(node, path.iterator, replacement, keepDistinct = true)

  final override def updateValueAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateValueAt(node, path.iterator, toPathItem, replacement, keepDistinct = true)

  final override def updateChild[T1 >: T: ClassTag](value: T1, replacement: Tree[T1]): Tree[T1] =
    NodeTree.updateChild(node, value, replacement, keepDistinct = true)

  final override def updateTreeAt[T1 >: T: ClassTag](
    path: Iterable[T1],
    replacement: Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateTreeAt(node, path.iterator, replacement, keepDistinct = true)

  final override def updateTreeAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    replacement: Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateTreeAt(node, path.iterator, toPathItem, replacement, keepDistinct = true)

  // DISTINCT MODIFICATIONS

  final override def modifyHead[T1 >: T: ClassTag](modify: T => T1): Tree[T1] =
    Tree(modify(node.head), node.children)

  final override def modifyChildValue[T1 >: T: ClassTag](value: T1, modify: T => T1): Tree[T1] =
    NodeTree.modifyChildValue(node, value, modify, keepDistinct = true)

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

  final override def modifyChild[T1 >: T: ClassTag](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    NodeTree.modifyChild(node, value, modify, keepDistinct = true)

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

  final override def removeChildValue[T1 >: T: ClassTag](value: T1): Tree[T] =
    NodeTree.removeChildValue(node, value, keepDistinct = true)

  final override def removeValueAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, keepDistinct = true)

  final override def removeValueAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, toPathItem, keepDistinct = true)

  final override def removeChild[T1 >: T: ClassTag](value: T1): Tree[T] =
    NodeTree.removeChild(node, value)

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

  final override def selectValue[K](path: Iterable[K], toPathItem: T => K): Option[T] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n.head, toPathItem)

  final override def selectTree[T1 >: T: ClassTag](path: Iterable[T1]): Option[Tree[T]] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n)

  final override def selectTree[K](path: Iterable[K], toPathItem: T => K): Option[Tree[T]] =
    NodeTree.select(node, path, (n: NodeTree[T]) => n, toPathItem)

  final override def containsChild[T1 >: T](value: T1): Boolean = node.children.exists(_.head == value)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    NodeTree.containsBranch(node, branch)

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean =
    NodeTree.containsBranch(node, branch, toPathItem)

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(node, path)
  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean =
    NodeTree.containsPath(node, path, toPathItem)

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
