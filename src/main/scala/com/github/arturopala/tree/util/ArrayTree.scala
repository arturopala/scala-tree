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

import com.github.arturopala.bufferandslice._
import com.github.arturopala.tree.Tree.ArrayTree
import com.github.arturopala.tree.util.ArrayTreeFunctions.reversedChildrenOf
import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.util.IntOps._

import scala.reflect.ClassTag

/**
  * Collection of high-level operations on the linear encoding of the tree.
  *
  * @note For low-level functions look into [[ArrayTreeFunctions]].
  */
object ArrayTree {

  /** Iterates over filtered values, top-down, depth-first. */
  final def valueIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: T => Boolean,
    maxDepth: Int
  ): Iterator[T] =
    new MapFilterIterator[Int, T](
      ArrayTreeFunctions.nodeIndexIteratorWithLimit(startIndex, treeStructure, maxDepth),
      treeValues,
      pred
    )

  /** Iterates over filtered tree's branches with pred. */
  final def branchIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean,
    maxDepth: Int
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntSlice, Iterable[T]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, treeStructure, maxDepth),
      _.map(treeValues).asIterable,
      pred
    )

  /** Iterates over filtered subtrees (including the tree itself), top-down, depth-first. */
  final def treeIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    new MapFilterIterator[Int, Tree[T]](
      ArrayTreeFunctions.nodeIndexIterator(startIndex, treeStructure),
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Iterates over filtered subtrees (including the tree itself) with depth limit, top-down, depth-first. */
  final def treeIteratorWithLimit[T: ClassTag](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean,
    maxDepth: Int
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    new MapFilterIterator[Int, Tree[T]](
      ArrayTreeFunctions.nodeIndexIteratorWithLimit(startIndex, treeStructure, maxDepth),
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Checks if the tree contains given branch. */
  @`inline` final def containsBranch[T, T1 >: T](
    branch: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, fullMatch) = ArrayTreeFunctions.followPath(branch, startIndex, treeStructure, treeValues)
    fullMatch && unmatched.isEmpty
  }

  /** Checks if the tree contains given branch. */
  @`inline` final def containsBranch[T, K](
    branch: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Boolean = {
    val (_, unmatched, _, fullMatch) =
      ArrayTreeFunctions.followPath(branch, startIndex, treeStructure, treeValues, toPathItem)
    fullMatch && unmatched.isEmpty
  }

  /** Count branches starting at index fulfilling the predicate. */
  final def countBranches[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: List[T] => Boolean
  ): Int =
    ArrayTreeFunctions.foldLeftBranchesIndexLists(
      startIndex,
      treeStructure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.reverseIterator.map(treeValues).toList)) 1 else 0)
    )

  /** Checks if the tree contains given path (as a branch prefix). */
  @`inline` final def containsPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean =
    ArrayTreeFunctions.followEntirePath(path, startIndex, treeStructure, treeValues).isDefined

  /** Checks if the tree contains given path (as a branch prefix). */
  @`inline` final def containsPath[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Boolean =
    ArrayTreeFunctions.followEntirePath(path, startIndex, treeStructure, treeValues, toPathItem).isDefined

  /** Selects node's value accessible by path using item extractor function. */
  @`inline` final def selectValue[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Option[T] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues, toPathItem)
      .map(indexes => treeValues(indexes.last))

  /** Selects tree accessible by path. */
  @`inline` final def selectTree[T: ClassTag, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Option[Tree[T]] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues)
      .map(indexes => treeAt[T](indexes.last, treeStructure, treeValues))

  /** Selects tree accessible by path using item extractor function. */
  @`inline` final def selectTree[T: ClassTag, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    toPathItem: T => K
  ): Option[Tree[T]] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues, toPathItem)
      .map(indexes => treeAt[T](indexes.last, treeStructure, treeValues))

  /** Returns tree rooted at the given index. */
  final def treeAt[T: ClassTag](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): Tree[T] = {
    val (structure, values) = ArrayTreeFunctions.subtreeAt(index, treeStructure, treeValues)
    fromSlices(structure, values)
  }

  /** Modifies the tree using provided function if delta !=0, or returns unmodified.
    * Before modification decomposes the tree into the buffers, and assembles it again after.
    */
  final def modifyTreeUsingBuffers[T: ClassTag](
    tree: Tree[T]
  )(modify: (IntBuffer, Buffer[T]) => Option[Int]): Tree[T] = {
    val (structureBuffer, valuesBuffer) = tree.toBuffers[T]
    modify(structureBuffer, valuesBuffer) match {
      case None    => tree
      case Some(_) => fromBuffers(structureBuffer, valuesBuffer)
    }
  }

  /** Assembles [[ArrayTree]] from slices of given buffers. */
  @`inline` final def fromBuffers[T: ClassTag](structureBuffer: IntBuffer, valuesBuffer: Buffer[T]): Tree[T] =
    fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)

  /** Assembles [[ArrayTree]] from given slices. */
  @`inline` final def fromSlices[T: ClassTag](treeStructure: IntSlice, treeValues: Slice[T]): Tree[T] =
    if (treeStructure.length == 0) Tree.empty
    else
      new ArrayTree[T](
        treeStructure,
        treeValues,
        ArrayTreeFunctions.calculateWidth(treeStructure),
        ArrayTreeFunctions.calculateHeight(treeStructure)
      )

  /** FlatMaps the tree without checking for duplicated children. */
  final def flatMap[T: ClassTag, K: ClassTag](
    treeStructure: IntSlice,
    treeValues: Slice[T],
    f: T => Tree[K]
  ): Tree[K] = {

    assert(treeStructure.length == treeValues.length, "Structure and values arrays of the tree MUST be the same size.")

    val structureBuffer = treeStructure.toBuffer
    val valuesBuffer = new ArrayBuffer[K](new Array[K](treeValues.length))

    var index = 0
    var offset = 0

    while (index < treeStructure.length) {
      val tree = f(treeValues(index))
      if (tree.isEmpty) {
        val parent = ArrayTreeFunctions.parentIndex(index, treeStructure.length, treeStructure) + offset
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, valuesBuffer)
        offset = offset - 1
      } else if (tree.isLeaf) {
        valuesBuffer(index + offset) = tree.value
      } else {
        val (structure, values) = tree.toSlices
        val delta =
          ArrayTreeFunctions.expandValueIntoTree(structure, values, index + offset, structureBuffer, valuesBuffer)
        offset = offset + delta
      }
      index = index + 1
    }

    val trees = TreeBuilder.fromArrays(structureBuffer.toArray, valuesBuffer.toArray)
    if (trees.size == 1) trees.head else Tree.empty
  }

  /** FlatMaps the tree while keeping children distinct. */
  final def flatMapDistinct[T: ClassTag, K: ClassTag](
    treeStructure: IntSlice,
    treeValues: Slice[T],
    f: T => Tree[K]
  ): Tree[K] = {

    assert(treeStructure.length == treeValues.length, "Structure and values arrays of the tree MUST be the same size.")

    val structureBuffer = treeStructure.toBuffer
    val valuesBuffer = Buffer.ofSize[K](treeValues.length)

    var index = 0
    var offset = 0

    while (index < treeStructure.length) {
      val parent = ArrayTreeFunctions.parentIndex(index + offset, structureBuffer.length, structureBuffer)
      val tree = f(treeValues(index))
      if (tree.isEmpty) {
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, valuesBuffer)
        offset = offset - 1
      } else {
        val (structure, values) = tree.toSlices
        val delta =
          ArrayTreeFunctions
            .expandValueIntoTreeDistinct(structure, values, index + offset, parent, structureBuffer, valuesBuffer)
        offset = offset + delta
      }
      index = index + 1
    }

    val trees = TreeBuilder.fromArrays(structureBuffer.toArray, valuesBuffer.toArray)
    if (trees.size == 1) trees.head else Tree.empty
  }

  /** Inserts a value to a sub-tree rooted at the path.
    * @param path sequence of a node's values forming a path to a sub-tree
    * @param value value to insert
    * @param target whole tree
    * @param keepDistinct if true keeps children distinct
    * @return modified tree */
  final def insertValueAt[T, T1 >: T: ClassTag](
    path: Iterable[T1],
    value: T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] = {
    val structure = target.structure
    val values = target.content
    val (indexes, unmatched, remaining, _) = ArrayTreeFunctions.followPath(path, target.size - 1, structure, values)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val valueList = (item :: remaining.toList) :+ value
            val newNode: Tree[T1] = TreeBuilder.linearTreeFromList(valueList)
            insertTree(index, newNode, target)
          case None =>
            insertValue(index, value, target, keepDistinct)
        }
    }
  }

  /** Inserts a value to a tree at a path using an extractor function.
    * @return modified tree */
  final def insertValueAt[T, T1 >: T: ClassTag, K](
    path: Iterable[K],
    value: T1,
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] = {
    val structure = target.structure
    val values = target.content
    val (indexes, unmatched, _, _) =
      ArrayTreeFunctions.followPath(path, target.size - 1, structure, values, toPathItem)
    indexes.lastOption match {
      case None => Left(target)
      case Some(index) =>
        if (unmatched.isDefined) Left(target)
        else {
          Right(insertValue(index, value, target, keepDistinct))
        }
    }
  }

  /** Checks if children of the tree rooted at the index contains the given value. */
  final def hasChildValue[T](index: Int, value: T, tree: Tree[T]): Boolean = tree match {
    case Tree.empty          => false
    case t: Tree.NodeTree[T] => t.subtrees.exists(_.value == value)
    case t: ArrayTree[T]     => ArrayTreeFunctions.leftmostIndexOfChildValue(value, index, t.structure, t.content).isDefined
  }

  final def prependValue[T: ClassTag, T1 >: T: ClassTag](value: T1, tree: ArrayTree[T]): Tree[T1] =
    modifyTreeUsingBuffers[T1](tree) { (structureBuffer, valuesBuffer) =>
      structureBuffer.push(1)
      valuesBuffer.push(value)
      Some(1)
    }

  /** Inserts a value to a tree at an index.
    * @param index index of the root of a target sub-tree
    * @param value value to insert
    * @param target whole tree
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertValue[T: ClassTag](
    index: Int,
    value: T,
    target: Tree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    if (target.isEmpty) Tree(value).deflated
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      if (keepDistinct && hasChildValue(index, value, target)) target
      else
        modifyTreeUsingBuffers(target) { (structureBuffer, valuesBuffer) =>
          ArrayTreeFunctions.insertValue(index, value, structureBuffer, valuesBuffer).asOption
        }
    }

  /** Inserts a subtree to a tree at a path.
    * @return modified tree */
  final def insertTreeAt[T, T1 >: T: ClassTag](
    path: Iterable[T1],
    subtree: Tree[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] = {
    val (indexes, unmatched, remaining, _) =
      ArrayTreeFunctions.followPath(path, target.size - 1, target.structure, target.content)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val treeList = (Tree(item) :: remaining.toList.map(Tree.apply[T1])) :+ subtree
            val newNode: Tree[T1] = TreeBuilder.fromTreeList(treeList)
            insertTree(index, newNode, target)
          case None =>
            if (keepDistinct) insertTreeDistinct(index, subtree, target)
            else insertTree(index, subtree, target)
        }
    }
  }

  /** Inserts a subtree to a tree at a path using an extractor function.
    * @return either modified tree or existing */
  final def insertTreeAt[T, T1 >: T: ClassTag, K](
    path: Iterable[K],
    subtree: Tree[T1],
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] = {
    val (indexes, unmatched, _, _) =
      ArrayTreeFunctions.followPath(path, target.size - 1, target.structure, target.content, toPathItem)
    indexes.lastOption match {
      case None => Left(target)
      case Some(index) =>
        if (unmatched.isDefined) Left(target)
        else if (keepDistinct) Right(insertTreeDistinct(index, subtree, target))
        else Right(insertTree(index, subtree, target))
    }
  }

  /** Inserts a subtree to a tree at an index.
    * @return modified tree */
  final def insertTree[T: ClassTag](
    index: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      modifyTreeUsingBuffers(target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = source.toSlices
        structureBuffer.increment(index)
        ArrayTreeFunctions.insertTree(index, structure, values, structureBuffer, valuesBuffer).asOption
      }
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertTreeDistinct[T: ClassTag](
    index: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      modifyTreeUsingBuffers(target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = source.toSlices
        ArrayTreeFunctions
          .insertLeftSubtreeListDistinct(List((index, structure, values)), structureBuffer, valuesBuffer, 0)
          .asNonZeroOption
      }
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertBranch[T: ClassTag](
    index: Int,
    branch: Iterable[T],
    target: Tree[T]
  ): Tree[T] =
    if (branch.isEmpty) target
    else if (target.isEmpty) TreeBuilder.linearTreeFromList(branch.toList)
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      val iterator: Iterator[T] = branch.iterator
      modifyTreeUsingBuffers(target) { (structureBuffer, valuesBuffer) =>
        if (iterator.hasNext && valuesBuffer(index) != iterator.next()) None
        else {
          ArrayTreeFunctions.insertBranch(iterator, index, structureBuffer, valuesBuffer, 0).asNonZeroOption
        }
      }
    }

  /** Updates value of the node at the index.
    * @return updated tree */
  final def updateValue[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    value: T1,
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else if (keepDistinct) {
      modifyTreeUsingBuffers[T1](tree) { (structureBuffer, valuesBuffer) =>
        valuesBuffer(index) = value
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer.length, structureBuffer))
        if (parentIndex >= 0) {
          ArrayTreeFunctions
            .childrenIndexesFor(value, parentIndex, structureBuffer, valuesBuffer)
            .find(_ != index) match {
            case Some(duplicateIndex) =>
              ArrayTreeFunctions.mergeTwoTrees(
                Math.max(index, duplicateIndex),
                Math.min(index, duplicateIndex),
                structureBuffer,
                valuesBuffer
              )
            case None =>
              new ArrayTree(tree.structure, valuesBuffer.asSlice, tree.width, tree.height)
          }
        }
        Some(-1)
      }
    } else {
      new ArrayTree(tree.structure, tree.content.update(index, value), tree.width, tree.height)
    }

  /** Modifies value of the node at the index.
    * @return modified tree */
  final def modifyValue[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    modify: T => T1,
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else updateValue(index, parentIndexOpt, modify(tree.content(index)), tree, keepDistinct)

  /** Removes node at the index and merges children to parent.
    * @return updated tree */
  final def removeValue[T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    if (index < 0) tree
    else if (index == tree.size - 1 && tree.childrenCount > 1)
      throw new RuntimeException("Cannot remove top tree node having more than a single child")
    else
      modifyTreeUsingBuffers(tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer.length, structureBuffer))
        val delta1 = ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, valuesBuffer)
        val delta2 = if (keepDistinct && parentIndex >= 0) {
          ArrayTreeFunctions.makeChildrenDistinct(parentIndex - 1, structureBuffer, valuesBuffer)
        } else 0
        Some(delta1 + delta2)
      }

  /** Modifies value of the node addressed by the path.
    * @return either modified tree or existing */
  final def modifyValueAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: T => T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyValue(indexes.last, indexes.get(indexes.length - 2), modify, target, keepDistinct)))
      .getOrElse(Left(target))

  /** Modifies value of the node addressed by the path.
    * @return either modified tree or existing */
  final def modifyValueAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: T => T1,
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyValue(indexes.last, indexes.get(indexes.length - 2), modify, target, keepDistinct)))
      .getOrElse(Left(target))

  /** Updates tree at the index.
    * @return updated tree */
  final def updateTree[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    subtree: Tree[T1],
    tree: Tree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else {
      modifyTreeUsingBuffers[T1](tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer.length, structureBuffer))
        val delta1 = ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer)

        if (subtree.isEmpty) {
          Some(delta1)
        } else {
          val (structure, values) = subtree.toSlices[T1]
          val delta2 = if (keepDistinct && parentIndex + delta1 > 0) {
            ArrayTreeFunctions
              .leftmostIndexOfChildValue(values.last, parentIndex + delta1, structureBuffer, valuesBuffer) match {
              case None =>
                structureBuffer.increment(parentIndex + delta1)
                ArrayTreeFunctions.insertTree(index + delta1 + 1, structure, values, structureBuffer, valuesBuffer)

              case Some(insertIndex) =>
                val queue = reversedChildrenOf(structure, values).map {
                  case (s, v) => (insertIndex, s, v)
                }
                ArrayTreeFunctions
                  .insertLeftSubtreeListDistinct(queue, structureBuffer, valuesBuffer, 0)
            }
          } else {
            if (parentIndex + delta1 >= 0) {
              structureBuffer.increment(parentIndex + delta1)
            }
            ArrayTreeFunctions.insertTree(index + delta1 + 1, structure, values, structureBuffer, valuesBuffer)
          }

          Some(delta1 + delta2)
        }
      }
    }

  /** Modifies the tree at the index.
    * @return modified tree */
  final def modifyTree[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    modify: Tree[T] => Tree[T1],
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else {
      val newTree = treeAt(index, tree.structure, tree.content)
      updateTree(index, parentIndexOpt, modify(newTree), tree, keepDistinct)
    }

  /** Removes the tree at the index.
    * @return updated tree */
  final def removeTree[T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    tree: Tree[T]
  ): Tree[T] =
    if (index < 0) tree
    else
      modifyTreeUsingBuffers(tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer.length, structureBuffer))
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer).asOption
      }

  /** Modifies a subtree addressed by the path.
    * @return either modified tree or existing */
  final def modifyTreeAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyTree(indexes.last, indexes.get(indexes.length - 2), modify, target, keepDistinct)))
      .getOrElse(Left(target))

  /** Modifies a subtree addressed by the path.
    * @return either modified tree or existing */
  final def modifyTreeAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    modify: Tree[T] => Tree[T1],
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyTree(indexes.last, indexes.get(indexes.length - 2), modify, target, keepDistinct)))
      .getOrElse(Left(target))

  /** Removes the node addressed by the last index, inserts children into the parent. private*/
  private def removeValueAt[T: ClassTag](indexes: IntSlice, target: ArrayTree[T], keepDistinct: Boolean): Tree[T] =
    if (indexes.isEmpty) target
    else if (indexes.last == target.size - 1) {
      if (target.isLeaf) Tree.empty
      else if (target.childrenCount == 1) treeAt(target.size - 2, target.structure, target.content)
      else target
    } else removeValue(indexes.last, indexes.get(indexes.length - 2), target, keepDistinct)

  /** Removes the node addressed by the path, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeValueAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(removeValueAt(_, target, keepDistinct))
      .getOrElse(target)

  /** Removes the node addressed by the path using an extractor function, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeValueAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Tree[T] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(removeValueAt(_, target, keepDistinct))
      .getOrElse(target)

}
