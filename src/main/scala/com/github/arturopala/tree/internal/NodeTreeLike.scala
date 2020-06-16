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

  final override def paths: Iterable[Iterable[T]] =
    iterableFrom(NodeTree.pathsIterator(node))

  final override def pathsWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom {
      if (maxDepth >= height) NodeTree.branchesIteratorWithFilter(pred, node, partialPaths = true)
      else NodeTree.branchesIteratorWithLimit(pred, node, maxDepth, partialPaths = true)
    }

  final override def branches: Iterable[Iterable[T]] =
    iterableFrom(NodeTree.branchesIterator(node))

  final override def branchesWithFilter(
    pred: Iterable[T] => Boolean,
    maxDepth: Int = Int.MaxValue
  ): Iterable[Iterable[T]] =
    iterableFrom {
      if (maxDepth >= height) NodeTree.branchesIteratorWithFilter(pred, node, partialPaths = false)
      else NodeTree.branchesIteratorWithLimit(pred, node, maxDepth, partialPaths = false)
    }

  final override def countBranches(pred: Iterable[T] => Boolean): Int =
    NodeTree.countBranches(pred, node)

  // SELECTIONS

  final override def selectValue[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean = false): Option[T] =
    NodeTree.select(node, path, (n: Tree[T]) => n.head, toPathItem, rightmost = rightmost)

  final override def selectTree[T1 >: T](path: Iterable[T1], rightmost: Boolean = false): Option[Tree[T]] =
    NodeTree.select(node, path, (n: Tree[T]) => n, rightmost = rightmost)

  final override def selectTree[K](path: Iterable[K], toPathItem: T => K, rightmost: Boolean): Option[Tree[T]] =
    NodeTree.select(node, path, (n: Tree[T]) => n, toPathItem, rightmost)

  final override def containsValue[T1 >: T](value: T1): Boolean = values().iterator.contains(value)

  final override def existsValue(pred: T => Boolean): Boolean = values().iterator.exists(pred)

  final override def containsChildValue[T1 >: T](value: T1): Boolean = node.children.exists(_.head == value)

  final override def existsChildValue(pred: T => Boolean): Boolean = node.children.exists(c => pred(c.head))

  final override def containsChild[T1 >: T](child: Tree[T1]): Boolean = node.children.exists(_ == child)

  final override def existsChild[T1 >: T](pred: Tree[T1] => Boolean): Boolean = node.children.exists(pred)

  final override def containsBranch[T1 >: T](branch: Iterable[T1]): Boolean =
    NodeTree.containsBranch(node, branch)

  final override def containsBranch[K](branch: Iterable[K], toPathItem: T => K): Boolean =
    NodeTree.containsBranch(node, branch, toPathItem)

  final override def existsBranch(pred: Iterable[T] => Boolean): Boolean =
    NodeTree.existsBranch(pred, node, partialPaths = false)

  final override def existsBranch[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    NodeTree.existsBranch(pred, node, partialPaths = false, toPathItem)

  final override def containsPath[T1 >: T](path: Iterable[T1]): Boolean = NodeTree.containsPath(node, path)

  final override def containsPath[K](path: Iterable[K], toPathItem: T => K): Boolean =
    NodeTree.containsPath(node, path, toPathItem)

  final override def existsPath(pred: Iterable[T] => Boolean): Boolean =
    NodeTree.existsBranch(pred, node, partialPaths = true)

  final override def existsPath[K](pred: Iterable[K] => Boolean, toPathItem: T => K): Boolean =
    NodeTree.existsBranch(pred, node, partialPaths = true, toPathItem)

  // DISTINCT INSERTIONS

  final override def prepend[T1 >: T](value: T1): Tree[T1] = Tree(value, node)

  final override def insertLeaf[T1 >: T](value: T1, append: Boolean = false): Tree[T1] =
    if (node.children.exists(_.head == value)) node
    else Tree(node.head, if (append) node.children.toSeq :+ Tree(value) else Tree(value) +: node.children.toSeq)

  final override def insertLeaves[T1 >: T](values: Iterable[T1], append: Boolean = false): Tree[T1] =
    if (values.isEmpty) node
    else {
      val distinctLeafs = values.filterNot(node.containsChildValue)
      if (distinctLeafs.isEmpty) node
      else
        Tree(
          node.head,
          if (append) node.children ++ distinctLeafs.map(Tree.apply[T1])
          else distinctLeafs.map(Tree.apply[T1]) ++ node.children
        )
    }

  final override def insertLeafAt[T1 >: T](path: Iterable[T1], value: T1, append: Boolean = false): Tree[T1] =
    NodeTree.insertChildAt(node, path.iterator, Tree(value), append, keepDistinct = true).getOrElse(node)

  final override def insertLeafAt[K, T1 >: T](
    path: Iterable[K],
    value: T1,
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.insertChildAt(node, path.iterator, toPathItem, Tree(value), append, keepDistinct = true)

  final override def insertChild[T1 >: T](child: Tree[T1], append: Boolean = false): Tree[T1] = child match {
    case Tree.empty         => node
    case tree: NodeTree[T1] => NodeTree.insertChildDistinct(node, tree, append)
    case tree: ArrayTree[T1] =>
      if (Tree.preferInflated(node, tree))
        NodeTree.insertChildDistinct(node, tree.inflated.asInstanceOf[NodeTree[T1]], append)
      else node.deflated.insertChild(tree, append)
  }

  final override def insertChildren[T1 >: T](
    children: Iterable[Tree[T1]],
    append: Boolean = false
  ): Tree[T1] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.isEmpty) node
    else if (validChildren.size == 1) node.insertChild(children.head, append)
    else if (validChildren.forall(_.isInstanceOf[NodeTree[T1]]))
      if (append)
        Tree(
          node.head,
          NodeTree.insertChildrenAfterDistinct(
            node.children,
            validChildren
          )
        )
      else
        Tree(
          node.head,
          NodeTree.insertChildrenBeforeDistinct(
            validChildren,
            node.children,
            preserveExisting = true
          )
        )
    else if (append)
      ArrayTree.insertAfterChildren(node, children, keepDistinct = true)
    else
      ArrayTree.insertBeforeChildren(node, children, keepDistinct = true)
  }

  final override def insertChildAt[T1 >: T](
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
        else node.deflated.insertChildAt(path, tree)
    }

  final override def insertChildAt[K, T1 >: T](
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

  final override def insertChildrenAt[T1 >: T](
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
      ArrayTree.insertChildrenAt(path, children, node.deflated, append, keepDistinct = true)
  }

  final override def insertChildrenAt[K, T1 >: T](
    path: Iterable[K],
    children: Iterable[Tree[T1]],
    toPathItem: T => K,
    append: Boolean
  ): Either[Tree[T], Tree[T1]] = {
    val validChildren = children.filterNot(_.isEmpty)
    if (validChildren.isEmpty) Right(node)
    else if (validChildren.size == 1) Right(node.insertChild(children.head, append))
    else
      NodeTree
        .insertChildrenAt(
          node,
          path.iterator,
          toPathItem,
          children.map(_.inflated).asInstanceOf[Iterable[NodeTree[T1]]],
          append,
          keepDistinct = true
        )
  }

  final override def insertBranch[T1 >: T](branch: Iterable[T1], append: Boolean = false): Tree[T1] =
    NodeTree.insertBranch(node, branch.iterator, append).getOrElse(node)

  final override def insertBranches[T1 >: T](
    branches: Iterable[Iterable[T1]],
    append: Boolean = false
  ): Tree[T1] =
    branches.foldLeft[Tree[T1]](node)((tree, branch) => tree.insertBranch(branch, append))

  // DISTINCT UPDATES

  final override def updateHead[T1 >: T](replacement: T1): Tree[T1] =
    Tree(replacement, node.children)

  final override def updateChildValue[T1 >: T](existingValue: T1, replacement: T1): Tree[T1] =
    NodeTree.updateChildValue(node, existingValue, replacement, keepDistinct = true, rightmost = false)

  final override def updateValueAt[T1 >: T](path: Iterable[T1], replacement: T1): Either[Tree[T], Tree[T1]] =
    NodeTree.updateValueAt(node, path.iterator, replacement, keepDistinct = true, rightmost = false)

  final override def updateValueAt[K, T1 >: T](
    path: Iterable[K],
    replacement: T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateValueAt(node, path.iterator, toPathItem, replacement, keepDistinct = true, rightmost = false)

  final override def updateChild[T1 >: T](value: T1, replacement: Tree[T1]): Tree[T1] =
    NodeTree.updateChild(node, value, replacement, keepDistinct = true, rightmost = false)

  final override def updateTreeAt[T1 >: T](
    path: Iterable[T1],
    replacement: Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateTreeAt(node, path.iterator, replacement, keepDistinct = true, rightmost = false)

  final override def updateTreeAt[K, T1 >: T](
    path: Iterable[K],
    replacement: Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.updateTreeAt(node, path.iterator, toPathItem, replacement, keepDistinct = true, rightmost = false)

  // DISTINCT MODIFICATIONS

  final override def modifyHead[T1 >: T](modify: T => T1): Tree[T1] =
    Tree(modify(node.head), node.children)

  final override def modifyChildValue[T1 >: T](value: T1, modify: T => T1): Tree[T1] =
    NodeTree.modifyChildValue(node, value, modify, keepDistinct = true, rightmost = false)

  final override def modifyValueAt[T1 >: T](
    path: Iterable[T1],
    modify: T => T1
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyValueAt(node, path.iterator, modify, keepDistinct = true, rightmost = false)

  final override def modifyValueAt[K, T1 >: T](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyValueAt(node, path.iterator, toPathItem, modify, keepDistinct = true, rightmost = false)

  final override def modifyChild[T1 >: T](value: T1, modify: Tree[T] => Tree[T1]): Tree[T1] =
    NodeTree.modifyChild(node, value, modify, keepDistinct = true, rightmost = false)

  final override def modifyChildren[T1 >: T](modify: Iterable[Tree[T]] => Iterable[Tree[T1]]): Tree[T1] =
    Tree(node.head, modify(node.children))

  final override def modifyTreeAt[T1 >: T](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1]
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyTreeAt(node, path.iterator, modify, keepDistinct = true, rightmost = false)

  final override def modifyTreeAt[K, T1 >: T](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyTreeAt(node, path.iterator, toPathItem, modify, keepDistinct = true, rightmost = false)

  final override def modifyChildrenAt[T1 >: T](
    path: Iterable[T1],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]]
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyChildrenAt(node, path.iterator, modify, keepDistinct = true, rightmost = false)

  final override def modifyChildrenAt[K, T1 >: T](
    path: Iterable[K],
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]] =
    NodeTree.modifyChildrenAt(node, path.iterator, toPathItem, modify, keepDistinct = true, rightmost = false)

  // DISTINCT REMOVALS

  final override def removeChildValue[T1 >: T](value: T1): Tree[T] =
    NodeTree.removeChildValue(node, value, keepDistinct = true, rightmost = false)

  final override def removeValueAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, keepDistinct = true, rightmost = false)

  final override def removeValueAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeValueAt(node, path.iterator, toPathItem, keepDistinct = true, rightmost = false)

  final override def removeChild[T1 >: T](value: T1): Tree[T] =
    NodeTree.removeChild(node, value, rightmost = false)

  final override def removeChildren[T1 >: T](): Tree[T] = Tree.Leaf(node.head)

  final override def removeTreeAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    NodeTree.removeTreeAt(node, path.iterator, rightmost = false)

  final override def removeTreeAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeTreeAt(node, path.iterator, toPathItem, rightmost = false)

  final override def removeChildrenAt[T1 >: T](path: Iterable[T1]): Tree[T] =
    NodeTree.removeChildrenAt(node, path.iterator, rightmost = false)

  final override def removeChildrenAt[K](path: Iterable[K], toPathItem: T => K): Tree[T] =
    NodeTree.removeChildrenAt(node, path.iterator, toPathItem, rightmost = false)

  // TRANSFORMATIONS

  final override def map[K](f: T => K): Tree[K] = {
    val (structure, values) = NodeTree.arrayMap(f, node)
    TreeBuilder.fromIterators(structure.iterator, values.iterator).headOption.getOrElse(empty)
  }

  final def mapUnsafe[K](f: T => K): Tree[K] = {
    def mapNodeUnsafe(n: Tree[T]): Tree[K] = Tree(f(n.head), n.children.map(mapNodeUnsafe))
    mapNodeUnsafe(node)
  }

  final override def toPairsIterator: Iterator[(Int, T)] = NodeTree.toPairsList(node).iterator

  final override def toArrays[T1 >: T: ClassTag]: (Array[Int], Array[T1]) = NodeTree.toArrays(node)

  final override def toSlices[T1 >: T]: (IntSlice, Slice[T1]) = NodeTree.toSlices(node)

  final override def toBuffers[T1 >: T]: (IntBuffer, Buffer[T1]) = NodeTree.toBuffers(node)

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

}
