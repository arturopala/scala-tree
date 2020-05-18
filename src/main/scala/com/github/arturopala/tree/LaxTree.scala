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

package com.github.arturopala.tree

import com.github.arturopala.tree.Tree.{ArrayTree, NodeTree, empty}
import com.github.arturopala.tree.internal.{ArrayTree, NodeTree}

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Lax Tree extensions.
  *
  * @note The [[Tree]] does not mandate children to be unique
  *       and main [[TreeLike]] API functions keeps them distinct.
  *       However, if your dataset is unique per se, or you do not
  *       care about uniqueness and do not want to pay a price of
  *       additional checks, this extension methods allow you to so.
  *
  * @groupprio laxInsertion 71
  * @groupname laxInsertion Lax insert
  * @groupprio laxModification 72
  * @groupname laxModification Lax modify
  * @groupprio laxRemoval 75
  * @groupname laxRemoval Lax removal
  */
trait LaxTree[T] {

  /** Flat-maps all nodes of the tree using provided function and returns a new tree.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @group transformation */
  def flatMapLax[K: ClassTag](f: T => Tree[K]): Tree[K]

  // LAX INSERTIONS

  /** Inserts a new child node holding the value and returns updated tree.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @group laxInsertion */
  def insertValueLax[T1 >: T: ClassTag](value: T1): Tree[T1]

  /** Inserts, at the given path, a new child node holding the value and returns a whole tree updated.
    * If path doesn't fully exist in the tree then remaining suffix will be created.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param value a value to insert as a new child
    * @group laxInsertion */
  def insertValueLaxAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1]

  /** Attempts to insert, at the given path, a new child node holding the value and returns a whole tree updated.
    * If path doesn't fully exist in the tree then tree will remain NOT updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of K items forming a path from the root to the parent node.
    * @param f extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with existing unmodified tree
    * @group laxInsertion */
  def insertValueLaxAt[K, T1 >: T: ClassTag](path: Iterable[K], value: T1, f: T => K): Either[Tree[T], Tree[T1]]

  /** Inserts a new sub-tree and returns updated tree.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @group laxInsertion */
  def insertTreeLax[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1]

  /** Inserts, at the given path, a new sub-tree and returns a whole tree updated.
    * If path doesn't fully exist in the tree then remaining suffix will be created.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @group laxInsertion */
  def insertTreeLaxAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1]

  /** Attempts to insert, at the given path, a new sub-tree and return a whole tree updated.
    * If path doesn't fully exist in the tree then tree will remain NOT updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list K items forming a path from the root to the parent node.
    * @return either right of modified tree or left with existing unmodified tree
    * @group laxInsertion */
  def insertTreeLaxAt[K, T1 >: T: ClassTag](path: Iterable[K], subtree: Tree[T1], f: T => K): Either[Tree[T], Tree[T1]]

  // LAX MODIFICATIONS

  /** Modifies the value selected by the given path, and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param modify function to modify the value
    * @return either right of modified tree or left with the tree intact
    * @group laxModification */
  def modifyValueLaxAt[T1 >: T: ClassTag](path: Iterable[T1], modify: T => T1): Either[Tree[T], Tree[T1]]

  /** Modifies the value selected by the given path, and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of K items forming a path from the root to the parent node.
    * @param modify function to modify the value
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with the tree intact
    * @group laxModification */
  def modifyValueLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]]

  /** Modifies the tree selected by the given path, and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @param modify function transforming the tree
    * @return either right of modified tree or left with the tree intact
    * @group laxModification */
  def modifyTreeLaxAt[T1 >: T: ClassTag](path: Iterable[T1], modify: Tree[T] => Tree[T1]): Either[Tree[T], Tree[T1]]

  /** Modifies the tree selected by the given path, and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list K items forming a path from the root to the parent node.
    * @param modify function transforming the tree
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return either right of modified tree or left with the tree intact
    * @group laxModification */
  def modifyTreeLaxAt[K, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    toPathItem: T => K
  ): Either[Tree[T], Tree[T1]]

  // LAX REMOVALS

  /** Removes the value selected by the given path, merges node's children with remaining siblings,
    * and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of node's values forming a path from the root to the parent node.
    * @return modified tree
    * @group laxRemoval */
  def removeValueLaxAt[T1 >: T: ClassTag](path: Iterable[T1]): Tree[T] = ???

  /** Removes the value selected by the given path, merges node's children with remaining siblings,
    * and returns a whole tree updated.
    * @note BEWARE: this method is lax, it will not keep node's children distinct.
    * @param path list of K items forming a path from the root to the parent node.
    * @param toPathItem extractor of the K path item from the tree's node value
    * @return modified tree
    * @group laxRemoval */
  def removeValueLaxAt[K, T1 >: T: ClassTag](path: Iterable[K], toPathItem: T => K): Tree[T] = ???

}

object LaxTreeOps {

  /** [[LaxTree]] extensions for a [[Tree]]. */
  implicit class LaxTreeExt[T: ClassTag](t: Tree[T]) extends LaxTree[T] {

    final override def flatMapLax[K: ClassTag](f: T => Tree[K]): Tree[K] = t match {
      case Tree.empty => Tree.empty

      case node: NodeTree[T] =>
        val list: List[(Int, Tree[K])] =
          NodeTree.listFlatMap(f, List((node.subtrees.size, f(node.value))), node.subtrees)
        TreeBuilder.fromTreePairsList(list, Nil, 0, TreeBuilder.TreeMergeStrategy.Join).headOption.getOrElse(empty)

      case tree: ArrayTree[T] =>
        ArrayTree.flatMap(tree.structure, tree.content, f)
    }

    final override def insertValueLax[T1 >: T: ClassTag](value: T1): Tree[T1] = t match {
      case Tree.empty => Tree(value)

      case node: NodeTree[T] =>
        Tree(node.value, Tree(value) :: node.subtrees)

      case tree: ArrayTree[T] =>
        ArrayTree.insertValue(tree.structure.length - 1, value, tree, keepDistinct = false)
    }

    final override def insertValueLaxAt[T1 >: T: ClassTag](path: Iterable[T1], value: T1): Tree[T1] = t match {
      case Tree.empty =>
        Tree.empty.insertBranch(path.toSeq :+ value)

      case node: NodeTree[T] =>
        NodeTree.insertTreeAt(node, path.iterator, Tree(value), keepDistinct = false).getOrElse(node)

      case tree: ArrayTree[T] =>
        ArrayTree.insertValueAt(path, value, tree, keepDistinct = false)
    }

    final override def insertValueLaxAt[K, T1 >: T: ClassTag](
      path: Iterable[K],
      value: T1,
      f: T => K
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty => Left(Tree.empty)

      case node: NodeTree[T] =>
        NodeTree.insertTreeAt(node, path.iterator, f, Tree(value), keepDistinct = false)

      case tree: ArrayTree[T] =>
        ArrayTree.insertValueAt(path, value, tree, f, keepDistinct = false)
    }

    final override def insertTreeLax[T1 >: T: ClassTag](subtree: Tree[T1]): Tree[T1] = t match {
      case Tree.empty => subtree

      case node: NodeTree[T] =>
        subtree match {
          case Tree.empty         => node
          case tree: NodeTree[T1] => Tree(node.value, tree :: node.subtrees)
          case tree: ArrayTree[T1] =>
            if (Tree.preferInflated(node, tree))
              Tree(node.value, tree.inflated.asInstanceOf[NodeTree[T1]] :: node.subtrees)
            else node.deflated[T1].insertTreeLax(tree)
        }

      case tree: ArrayTree[T] =>
        ArrayTree.insertTree(tree.structure.length - 1, subtree, tree)
    }

    final override def insertTreeLaxAt[T1 >: T: ClassTag](path: Iterable[T1], subtree: Tree[T1]): Tree[T1] = t match {
      case Tree.empty =>
        if (path.isEmpty) subtree
        else if (subtree.isEmpty) empty
        else TreeBuilder.linearTreeFromSequence(path.toSeq).insertTreeLaxAt(path, subtree)

      case node: NodeTree[T] =>
        subtree match {
          case Tree.empty => node
          case tree: NodeTree[T1] =>
            NodeTree.insertTreeAt(node, path.iterator, tree, keepDistinct = false).getOrElse(node)
          case tree: ArrayTree[T1] =>
            if (Tree.preferInflated(node, tree))
              NodeTree
                .insertTreeAt(node, path.iterator, tree.inflated.asInstanceOf[NodeTree[T1]], keepDistinct = false)
                .getOrElse(node)
            else node.deflated[T1].insertTreeLaxAt(path, tree)
        }

      case tree: ArrayTree[T] =>
        ArrayTree.insertTreeAt(path, subtree, tree, keepDistinct = false)
    }

    final override def insertTreeLaxAt[K, T1 >: T: ClassTag](
      path: Iterable[K],
      subtree: Tree[T1],
      f: T => K
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty =>
        if (path.isEmpty) Right(subtree) else Left(empty)

      case node: NodeTree[T] =>
        subtree match {
          case Tree.empty         => Left(node)
          case tree: NodeTree[T1] => NodeTree.insertTreeAt(node, path.iterator, f, tree, keepDistinct = false)
          case tree: ArrayTree[T1] =>
            NodeTree
              .insertTreeAt(node, path.iterator, f, tree.inflated.asInstanceOf[NodeTree[T1]], keepDistinct = false)
        }

      case tree: ArrayTree[T] =>
        ArrayTree.insertTreeAt(path, subtree, tree, f, keepDistinct = false)
    }

    final override def modifyValueLaxAt[T1 >: T: ClassTag](
      path: Iterable[T1],
      modify: T => T1
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty => Left(empty)

      case node: NodeTree[T] =>
        NodeTree.modifyValueAt(node, path.iterator, modify, keepDistinct = false)

      case tree: ArrayTree[T] =>
        ArrayTree.modifyValueAt(path, modify, tree, keepDistinct = false)
    }

    final override def modifyValueLaxAt[K, T1 >: T: ClassTag](
      path: Iterable[K],
      modify: T => T1,
      toPathItem: T => K
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty => Left(empty)

      case node: NodeTree[T] =>
        NodeTree.modifyValueAt(node, path.iterator, toPathItem, modify, keepDistinct = false)

      case tree: ArrayTree[T] =>
        ArrayTree.modifyValueAt(path, modify, tree, toPathItem, keepDistinct = false)
    }

    final override def modifyTreeLaxAt[T1 >: T: ClassTag](
      path: Iterable[T1],
      modify: Tree[T] => Tree[T1]
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty => Left(empty)

      case node: NodeTree[T] =>
        NodeTree.modifyTreeAt(node, path.iterator, modify, keepDistinct = false)

      case tree: ArrayTree[T] =>
        ArrayTree.modifyTreeAt(path, modify, tree, keepDistinct = false)

    }

    final override def modifyTreeLaxAt[K, T1 >: T: ClassTag](
      path: Iterable[K],
      modify: Tree[T] => Tree[T1],
      toPathItem: T => K
    ): Either[Tree[T], Tree[T1]] = t match {
      case Tree.empty => Left(empty)

      case node: NodeTree[T] =>
        NodeTree.modifyTreeAt(node, path.iterator, toPathItem, modify, keepDistinct = false)

      case tree: ArrayTree[T] =>
        ArrayTree.modifyTreeAt(path, modify, tree, toPathItem, keepDistinct = false)

    }

  }

}
