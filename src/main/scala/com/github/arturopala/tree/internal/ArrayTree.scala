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

import com.github.arturopala.bufferandslice._
import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.internal.IntOps._
import com.github.arturopala.tree.internal.IterableOps.iterableFrom

import scala.collection.Iterator.continually

/**
  * Collection of high-level operations on the linear encoding of the tree.
  *
  * @note For low-level functions look into [[ArrayTreeFunctions]].
  */
object ArrayTree {

  /** Iterates right-to-left over all leaves of the tree rooted at startIndex. */
  def leavesIterator[F[+_]: Transformer, T](startIndex: Int, target: F[T]): Iterator[T] = {
    val (structure, content) = toSlices(target)
    val treeSize =
      if (startIndex == structure.top) structure.length
      else ArrayTreeFunctions.treeSize(startIndex, structure)
    structure
      .slice(startIndex - treeSize + 1, startIndex + 1)
      .reverseIndexIterator(_ == 0)
      .map(content)
  }

  /** Iterates over all content of the tree rooted at startIndex.
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def valuesIterator[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    depthFirst: Boolean
  ): Iterator[T] = {
    val (structure, content) = toSlices(target)
    (if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirst(startIndex, structure)
     else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, structure))
      .map(content)
  }

  /** Iterates over filtered content of the tree.
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def valuesIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: T => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[T] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[Int, T](
      if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirstWithLimit(startIndex, structure, maxDepth)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirstWithLimit(startIndex, structure, maxDepth),
      content,
      pred
    )
  }

  /** Iterates over filtered tuples of (level, value, isLeaf) of the tree.
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def valuesAndLevelsIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: T => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, T, Boolean)] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[(Int, Int), (Int, T, Boolean)](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexAndLevelIteratorDepthFirstWithLimit(startIndex, structure, maxDepth)
      else
        ArrayTreeFunctions.nodesIndexAndLevelIteratorBreadthFirstWithLimit(startIndex, structure, maxDepth),
      { case (level: Int, index: Int) => (level, content(index), structure(index) == 0) },
      (t: (Int, T, Boolean)) => pred(t._2)
    )
  }

  /** Iterates over all paths of the tree. */
  final def pathsIterator[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T]
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .pathsIndexListIterator(startIndex, structure)
      .map(_.map(content))
  }

  /** Iterates over filtered tree's paths. */
  final def pathsIteratorWithFilter[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: Iterable[T] => Boolean
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.pathsIndexListIterator(startIndex, structure),
      _.map(content),
      pred
    )
  }

  /** Iterates over filtered and mapped tree's paths. */
  final def pathsIteratorWithFilter[F[+_]: Transformer, T, K](
    startIndex: Int,
    target: F[T],
    pred: Iterable[K] => Boolean,
    toPathItem: T => K
  ): Iterator[Iterable[K]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[K]](
      ArrayTreeFunctions.pathsIndexListIterator(startIndex, structure),
      _.map(content).map(toPathItem),
      pred
    )
  }

  /** Iterates over filtered tree's path with depth limit. */
  final def pathsIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: Iterable[T] => Boolean,
    maxDepth: Int
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.pathsIndexListIterator(startIndex, structure, maxDepth),
      _.map(content),
      pred
    )
  }

  /** Iterates over all branches of the tree. */
  final def branchesIterator[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T]
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .branchesIndexListIterator(startIndex, structure)
      .map(_.map(content))
  }

  /** Iterates over filtered tree's branches. */
  final def branchesIteratorWithFilter[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: Iterable[T] => Boolean
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, structure),
      _.map(content),
      pred
    )
  }

  /** Iterates over filtered and mapped tree's branches. */
  final def branchesIteratorWithFilter[F[+_]: Transformer, T, K](
    startIndex: Int,
    target: F[T],
    pred: Iterable[K] => Boolean,
    toPathItem: T => K
  ): Iterator[Iterable[K]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[K]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, structure),
      _.map(content).map(toPathItem),
      pred
    )
  }

  /** Iterates over filtered tree's branches with depth limit. */
  final def branchesIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: Iterable[T] => Boolean,
    maxDepth: Int
  ): Iterator[Iterable[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, structure, maxDepth),
      _.map(content),
      pred
    )
  }

  /** Iterates over all subtrees (including the tree itself).
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def treesIterator[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    depthFirst: Boolean
  ): Iterator[F[T]] = {
    val (structure, content) = toSlices(target)
    (if (depthFirst)
       ArrayTreeFunctions
         .nodesIndexIteratorDepthFirst(startIndex, structure)
     else
       ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, structure))
      .map(instanceAt2(_, structure, content))
  }

  /** Iterates over filtered subtrees (including the tree itself).
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithFilter[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: F[T] => Boolean,
    depthFirst: Boolean
  ): Iterator[F[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[Int, F[T]](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexIteratorDepthFirst(startIndex, structure)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, structure),
      instanceAt2(_, structure, content),
      pred
    )
  }

  /** Iterates over filtered subtrees (including the tree itself) with depth limit.
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: F[T] => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[F[T]] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[Int, F[T]](
      if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirstWithLimit(startIndex, structure, maxDepth)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirstWithLimit(startIndex, structure, maxDepth),
      instanceAt2(_, structure, content),
      pred
    )
  }

  /** Iterates over filtered pairs of (level, tree) of the tree.
    * @param depthFirst if true, enumerates content depth-first,
    *                   if false, breadth-first. */
  final def treesAndLevelsIteratorWithLimit[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: F[T] => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, F[T])] = {
    val (structure, content) = toSlices(target)
    new MapFilterIterator[(Int, Int), (Int, F[T])](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexAndLevelIteratorDepthFirstWithLimit(startIndex, structure, maxDepth)
      else
        ArrayTreeFunctions.nodesIndexAndLevelIteratorBreadthFirstWithLimit(startIndex, structure, maxDepth),
      { case (level: Int, index: Int) => (level, instanceAt2(index, structure, content)) },
      (t: (Int, F[T])) => pred(t._2)
    )
  }

  /** Follows the given path of content into the tree.
    * @return a tuple consisting of:
    *         - an array of travelled indexes,
    *         - optionally non matching path segment,
    *         - remaining path iterator,
    *         - flag set to true if path matched an entire branch.
    */
  @`inline` final def followPath[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean
  ): (IntSlice, Option[T1], Iterator[T1], Boolean) = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followPath(path, structure.top, structure, content, rightmost)
  }

  /** Follows the given path into the tree using a path item extractor function.
    * @return a tuple consisting of:
    *         - an array of travelled indexes,
    *         - optionally non matching path segment,
    *         - remaining path iterator,
    *         - flag set to true if path matched an entire branch.
    */
  @`inline` final def followPath[F[+_]: Transformer, T, K](
    path: Iterable[K],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): (IntSlice, Option[K], Iterator[K], Boolean) = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followPath(path, structure.top, structure, content, toPathItem, rightmost)
  }

  /** Follows the entire path of content into the tree.
    * @return a Some of an array of travelled indexes, or None if path doesn't exist.
    */
  @`inline` final def followEntirePath[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean
  ): Option[IntSlice] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, structure.top, structure, content, rightmost)
  }

  /** Follows the entire path of into the tree using a path item extractor function.
    * @return a Some of an array of travelled indexes, or None if path doesn't exist.
    */
  @`inline` final def followEntirePath[F[+_]: Transformer, T, K](
    path: Iterable[K],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[IntSlice] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, structure.top, structure, content, toPathItem, rightmost)
  }

  /** Checks if the tree starting at parentIndex contains given direct child value. */
  @`inline` final def containsChildValue[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    parentIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, structure)
      .exists(i => content(i) == value)
  }

  /** Checks if the tree starting at parentIndex contains given direct child. */
  @`inline` final def containsChild[F[+_]: Transformer, T, T1 >: T](
    child: F[T1],
    parentIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, structure)
      .exists(i => instanceAt2(i, structure, content) == child)
  }

  /** Checks if the tree starting at parentIndex contains direct child value fulfilling the predicate. */
  @`inline` final def existsChildValue[F[+_]: Transformer, T, T1 >: T](
    pred: T1 => Boolean,
    parentIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, structure)
      .exists(i => pred(content(i)))
  }

  /** Checks if the tree starting at parentIndex contains direct child fulfilling the predicate. */
  @`inline` final def existsChild[F[+_]: Transformer, T, T1 >: T](
    pred: F[T1] => Boolean,
    parentIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, structure)
      .exists(i => pred(instanceAt2(i, structure, content)))
  }

  /** Checks if the tree starting at parentIndex contains given branch. */
  @`inline` final def containsBranch[F[+_]: Transformer, T, T1 >: T](
    branch: Iterable[T1],
    startIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    val (_, unmatched, _, fullMatch) =
      ArrayTreeFunctions.followPath(branch, startIndex, structure, content, rightmost = false)
    fullMatch && unmatched.isEmpty
  }

  /** Checks if the tree starting at parentIndex contains given branch using an extractor item. */
  @`inline` final def containsBranch[F[+_]: Transformer, T, K](
    branch: Iterable[K],
    startIndex: Int,
    target: F[T],
    toPathItem: T => K
  ): Boolean = {
    val (structure, content) = toSlices(target)
    val (_, unmatched, _, fullMatch) =
      ArrayTreeFunctions.followPath(branch, startIndex, structure, content, toPathItem, rightmost = false)
    fullMatch && unmatched.isEmpty
  }

  /** Count branches starting at index fulfilling the predicate. */
  final def countBranches[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    pred: Iterable[T] => Boolean
  ): Int = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.foldLeftBranchesIndexLists(
      startIndex,
      structure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.map(content).asIterable)) 1 else 0)
    )
  }

  /** Checks if the tree starting at parentIndex contains given path (a branch prefix) using item extractor function. */
  @`inline` final def containsPath[F[+_]: Transformer, T, K](
    path: Iterable[K],
    startIndex: Int,
    target: F[T],
    toPathItem: T => K
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, startIndex, structure, content, toPathItem, rightmost = false).isDefined
  }

  /** Checks if the tree starting at parentIndex contains given path (a branch prefix). */
  @`inline` final def containsPath[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    target: F[T]
  ): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, startIndex, structure, content, rightmost = false).isDefined
  }

  /** Selects node's value accessible by path using item extractor function. */
  @`inline` final def selectValue[F[+_]: Transformer, T, K](
    path: Iterable[K],
    startIndex: Int,
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[T] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, structure, content, toPathItem, rightmost)
      .map(indexes => content(indexes.last))
  }

  /** Selects tree accessible by path. */
  @`inline` final def selectTree[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    target: F[T],
    rightmost: Boolean
  ): Option[F[T]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, structure, content, rightmost)
      .map(indexes => instanceAt2(indexes.last, structure, content))
  }

  /** Selects tree accessible by path using item extractor function. */
  @`inline` final def selectTree[F[+_]: Transformer, T, K](
    path: Iterable[K],
    startIndex: Int,
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[F[T]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, structure, content, toPathItem, rightmost)
      .map(indexes => instanceAt2(indexes.last, structure, content))
  }

  // TRANSFORMATIONS

  /** Returns tree rooted at the given index. */
  final def instanceAt2[F[+_]: Transformer, T](index: Int, structure: IntSlice, content: Slice[T]): F[T] = {
    val (structure2, values2) = ArrayTreeFunctions.treeAt(index, structure, content)
    fromSlices(structure2, values2)
  }

  /** Returns instance rooted at the given index. */
  final def instanceAt[F[+_]: Transformer, T](index: Int, target: F[T]): F[T] = {
    val (structure, content) = toSlices(target)
    if (index == structure.top) target
    else {
      val (childStructure, childValues) = ArrayTreeFunctions.treeAt(index, structure, content)
      fromSlices(childStructure, childValues)
    }
  }

  /** Transforms the tree using provided function, or returns unmodified if result in None.
    * Before modification decomposes the tree into the buffers, and assembles it again after.
    * @tparam T type of the input tree content
    * @tparam T1 type of the output tree content
    */
  private final def transform[F[+_]: Transformer, T, T1 >: T](
    target: F[T]
  )(modify: (IntBuffer, Buffer[T1]) => Option[Int]): F[T1] = {
    val (structureBuffer, contentBuffer) = implicitly[Transformer[F]].toBuffers[T, T1](target)
    modify(structureBuffer, contentBuffer) match {
      case None    => target
      case Some(_) => implicitly[Transformer[F]].fromBuffers(structureBuffer, contentBuffer)
    }
  }

  /** Creates an instance from a pair of slices. */
  @`inline` private final def fromSlices[F[+_]: Transformer, T](structure: IntSlice, content: Slice[T]): F[T] =
    implicitly[Transformer[F]].fromSlices(structure, content)

  /** Outputs tree linearisation as a pair of slices. */
  @`inline` private final def toSlices[F[+_]: Transformer, T](target: F[T]): (IntSlice, Slice[T]) =
    implicitly[Transformer[F]].toSlices(target)

  /** Returns true if instance is empty. */
  @`inline` private final def isEmpty[F[+_]: Transformer, T](target: F[T]): Boolean =
    implicitly[Transformer[F]].isEmpty(target)

  /** Returns size of the tree, i.e. the number of nodes. */
  @`inline` private final def sizeOf[F[+_]: Transformer, T](target: F[T]): Int =
    implicitly[Transformer[F]].sizeOf(target)

  /** Maps the tree's content. */
  final def map[F[+_]: Transformer, T, K](target: F[T], f: T => K): F[K] = {
    val (structure, content) = toSlices(target)
    fromSlices(structure, content.map(f))
  }

  /** FlatMaps the tree without checking for duplicated children. */
  final def flatMapLax[F[+_]: Transformer, T, K](target: F[T], f: T => F[K]): F[K] = {

    val (structure, content) = toSlices(target)

    assert(structure.length == content.length, "Structure and content arrays of the tree MUST be the same size.")

    val structureBuffer = structure.asBuffer
    val contentBuffer = Buffer.ofSize[K](content.length)

    var index = 0
    var offset = 0

    while (index < structure.length) {
      val tree = f(content(index))
      if (isEmpty(tree)) {
        val parent = ArrayTreeFunctions.parentIndex(index, structure) + offset
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, contentBuffer)
        offset = offset - 1
      } else {
        val (structure, content) = toSlices(tree)
        if (sizeOf(tree) == 1) {
          contentBuffer(index + offset) = content.last
        } else {
          val delta =
            ArrayTreeFunctions
              .expandValueIntoTreeLax(index + offset, structure, content, structureBuffer, contentBuffer)
          offset = offset + delta
        }
      }
      index = index + 1
    }

    if (ArrayTreeFunctions.hasValidTreeStructure(structureBuffer))
      fromSlices(structureBuffer.asSlice, contentBuffer.asSlice)
    else
      implicitly[Transformer[F]].empty
  }

  /** FlatMaps the tree while keeping children distinct. */
  final def flatMapDistinct[F[+_]: Transformer, T, K](target: F[T], f: T => F[K]): F[K] = {

    val (structure, content) = toSlices(target)

    assert(structure.length == content.length, "Structure and content arrays of the tree MUST be the same size.")

    val structureBuffer = structure.asBuffer
    val contentBuffer = Buffer.ofSize[K](content.length)

    var index = 0
    var offset = 0

    while (index < structure.length) {
      val parent = ArrayTreeFunctions.parentIndex(index + offset, structureBuffer)
      val tree = f(content(index))
      if (isEmpty(tree)) {
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, contentBuffer)
        offset = offset - 1
      } else {
        val (structure, content) = toSlices(tree)
        val delta =
          ArrayTreeFunctions
            .expandValueIntoTreeDistinct(index + offset, parent, structure, content, structureBuffer, contentBuffer)
        offset = offset + delta
      }
      index = index + 1
    }

    if (ArrayTreeFunctions.hasValidTreeStructure(structureBuffer))
      fromSlices(structureBuffer.asSlice, contentBuffer.asSlice)
    else
      implicitly[Transformer[F]].empty
  }

  /** Inserts a value to a sub-tree rooted at the path.
    * @param path sequence of a node's content forming a path to a sub-tree
    * @param value value to insert
    * @param target whole tree
    * @param keepDistinct if true keeps children distinct
    * @return modified tree */
  final def insertLeafAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    value: T1,
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, content) = toSlices(target)
    val (indexes, unmatched, remaining, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, content, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val valueSequence = remaining.toVector.+:(item).:+(value)
            val newNode: F[T1] = {
              val (s, c) = TreeBuilder.linearTreeFromSequence(valueSequence).toSlices
              fromSlices(s, c)
            }
            val insertIndex = if (append) ArrayTreeFunctions.bottomIndex(index, structure) else index
            insertTreeAtIndex(insertIndex, index, newNode, target)
          case None =>
            insertLeaf(index, value, target, append, keepDistinct)
        }
    }
  }

  /** Inserts a value to a tree at a path using an extractor function.
    * @return modified tree */
  final def insertLeafAt[F[+_]: Transformer, T, T1 >: T, K](
    path: Iterable[K],
    value: T1,
    target: F[T],
    toPathItem: T => K,
    append: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (indexes, unmatched, _, _) = followPath(path, target, toPathItem, append)
    indexes.lastOption match {
      case None => Left(target)
      case Some(index) =>
        if (unmatched.isDefined) Left(target)
        else {
          Right(insertLeaf(index, value, target, append, keepDistinct))
        }
    }
  }

  /** Checks if children of the tree rooted at the index contains the given value. */
  final def hasChildValue[F[+_]: Transformer, T](index: Int, value: T, target: F[T]): Boolean = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions.firstChildHavingValue(value, index, structure.length, structure, content).isDefined
  }

  /** Prepends tree with the new head value. */
  final def prepend[F[+_]: Transformer, T, T1 >: T](value: T1, target: F[T]): F[T1] =
    transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
      structureBuffer.push(1)
      contentBuffer.push(value)
      Some(1)
    }

  /** Inserts new leaf to a tree at an index.
    * @param parentIndex index of the tree where to insert child
    * @param value value to insert
    * @param target the array tree
    * @param append whether to append (after) or prepend (before) to the existing children
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertLeaf[F[+_]: Transformer, T, T1 >: T](
    parentIndex: Int,
    value: T1,
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] =
    if (keepDistinct && hasChildValue(parentIndex, value, target)) target
    else
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        val insertIndex =
          if (append) ArrayTreeFunctions.bottomIndex(parentIndex, structureBuffer)
          else parentIndex
        ArrayTreeFunctions.insertValue(insertIndex, parentIndex, value, structureBuffer, contentBuffer).intAsSome
      }

  /** Inserts new leaves to a tree at an index.
    * @param parentIndex index of the tree where to insert children
    * @param values leaves to insert
    * @param target the array tree
    * @param append whether to append (after) or prepend (before) to the existing children
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertLeaves[F[+_]: Transformer, T, T1 >: T](
    parentIndex: Int,
    values: Iterable[T1],
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] =
    transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
      if (parentIndex < 0 && values.size > 1) None
      else {
        val toInsert = if (keepDistinct) {
          val existing = ArrayTreeFunctions.childrenIndexes(parentIndex, structureBuffer).map(contentBuffer)
          values.filterNot(value => existing.exists(_ == value))
        } else values
        val size = toInsert.size
        val insertIndex = if (append) ArrayTreeFunctions.bottomIndex(parentIndex, structureBuffer) else parentIndex
        structureBuffer.modify(parentIndex, _ + size)
        ArrayTreeFunctions
          .insertFromIteratorReverse(
            insertIndex,
            size,
            continually(0),
            toInsert.iterator,
            structureBuffer,
            contentBuffer
          )
          .nonZeroIntAsSome
      }
    }

  /** Inserts a subtree to a tree at a path.
    * @return modified tree */
  final def insertChildAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    child: F[T1],
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, content) = toSlices(target)
    val (indexes, unmatched, remaining, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, content, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) => {
            transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
              val branch = item +: remaining.toVector
              val insertIndex =
                if (append) ArrayTreeFunctions.bottomIndex(index, structureBuffer)
                else index
              structureBuffer.increment(index)
              val delta1 = ArrayTreeFunctions
                .insertFromIteratorReverse(
                  insertIndex,
                  branch.length,
                  Iterator.fill(branch.length)(1),
                  branch.iterator,
                  structureBuffer,
                  contentBuffer
                )
              val (structure, content) = toSlices(child)
              val delta2 =
                ArrayTreeFunctions.insertSlices(insertIndex, structure, content, structureBuffer, contentBuffer)
              Some(delta1 + delta2)
            }
          }

          case None =>
            if (keepDistinct) insertChildDistinct(index, child, target, append)
            else {
              val insertIndex =
                if (append) ArrayTreeFunctions.bottomIndex(index, structure)
                else index
              insertTreeAtIndex(insertIndex, index, child, target)
            }
        }
    }
  }

  /** Inserts a subtree to a tree at a path using an extractor function.
    * @return either modified tree or an existing */
  final def insertChildAt[F[+_]: Transformer, T, T1 >: T, K](
    path: Iterable[K],
    child: F[T1],
    target: F[T],
    toPathItem: T => K,
    append: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, content) = toSlices(target)
    val (indexes, unmatched, _, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, content, toPathItem, append)
    indexes.lastOption match {
      case None => Left(target)
      case Some(index) =>
        if (unmatched.isDefined) Left(target)
        else if (keepDistinct) Right(insertChildDistinct(index, child, target, append))
        else {
          val insertIndex =
            if (append) ArrayTreeFunctions.bottomIndex(index, structure)
            else index
          Right(insertTreeAtIndex(insertIndex, index, child, target))
        }
    }
  }

  /** Inserts a subtree to a tree at a path.
    * @return modified tree */
  final def insertChildrenAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    children: Iterable[F[T1]],
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (indexes, unmatched, remaining, _) = followPath(path, target, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
          val (delta1, index2) = unmatched
            .map { item =>
              val branch = item +: remaining.toVector
              val insertIndex =
                if (append) ArrayTreeFunctions.bottomIndex(index, structureBuffer)
                else index
              structureBuffer.increment(index)
              val delta = ArrayTreeFunctions
                .insertFromIteratorReverse(
                  insertIndex,
                  branch.length,
                  Iterator.fill(branch.length - 1)(1) ++ Iterator.single(0),
                  branch.iterator,
                  structureBuffer,
                  contentBuffer
                )
              (delta, insertIndex)
            }
            .getOrElse((0, index))

          val delta2 = ArrayTreeFunctions
            .insertChildren(
              index2,
              children.map(toSlices(_)),
              structureBuffer,
              contentBuffer,
              append,
              keepDistinct
            )

          Some(delta1 + delta2)
        }
    }
  }

  /** Inserts a subtree to a tree at a path.
    * @return modified tree */
  final def insertChildrenAt[F[+_]: Transformer, T, T1 >: T, K](
    path: Iterable[K],
    children: Iterable[F[T1]],
    target: F[T],
    toPathItem: T => K,
    append: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (indexes, unmatched, _, _) = followPath(path, target, toPathItem, append)
    indexes.lastOption match {
      case Some(index) if unmatched.isEmpty =>
        Right(transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
          ArrayTreeFunctions
            .insertChildren(index, children.map(toSlices(_)), structureBuffer, contentBuffer, append, keepDistinct)
            .intAsSome
        })

      case _ => Left(target)
    }
  }

  /** Prepends children with the new child without checking for duplicates. */
  final def prependChild[F[+_]: Transformer, T, T1 >: T](target: F[T], child: F[T1]): F[T1] = {
    val (structure, _) = toSlices(target)
    insertTreeAtIndex(structure.top, structure.top, child, target)
  }

  /** Appends children with the new child without checking for duplicates. */
  final def appendChild[F[+_]: Transformer, T, T1 >: T](target: F[T], child: F[T1]): F[T1] = {
    val (structure, _) = toSlices(target)
    insertTreeAtIndex(0, structure.top, child, target)
  }

  /** Inserts a subtree to a tree at an index.
    * @return modified tree */
  final def insertTreeAtIndex[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    parentIndex: Int,
    child: F[T1],
    target: F[T]
  ): F[T1] =
    if (isEmpty(child)) target
    else {
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        val (structure, content) = toSlices(child)
        if (parentIndex >= 0 && structureBuffer.nonEmpty) structureBuffer.increment(parentIndex)
        ArrayTreeFunctions.insertSlices(index, structure, content, structureBuffer, contentBuffer).intAsSome
      }
    }

  /** Inserts a subtree to a tree at an index while keeping children content distinct.
    * @return modified tree */
  final def insertChildDistinct[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    child: F[T1],
    target: F[T],
    append: Boolean
  ): F[T1] =
    if (isEmpty(child)) target
    else {
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        val (structure, content) = toSlices(child)
        (if (append)
           ArrayTreeFunctions
             .insertAfterChildDistinct(index, structure, content, structureBuffer, contentBuffer)
         else
           ArrayTreeFunctions
             .insertBeforeChildDistinct(index, structure, content, structureBuffer, contentBuffer)).nonZeroIntAsSome
      }
    }

  /** Inserts multiple children before the existing children. */
  final def insertBeforeChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    children: Iterable[F[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (children.forall(isEmpty(_))) target
    else
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        if (structureBuffer.isEmpty) {
          if (children.size == 1) {
            val (structure, content) = toSlices(children.head)
            ArrayTreeFunctions.insertSlices(0, structure, content, structureBuffer, contentBuffer).intAsSome
          } else None
        } else {
          ArrayTreeFunctions
            .insertBeforeChildren(
              structureBuffer.top,
              children.map(toSlices(_)),
              structureBuffer,
              contentBuffer,
              keepDistinct
            )
            .intAsSome
        }
      }

  /** Inserts multiple children after the existing children. */
  final def insertAfterChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    children: Iterable[F[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (children.forall(isEmpty(_))) target
    else
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        if (structureBuffer.isEmpty) {
          if (children.size == 1) {
            val (structure, content) = toSlices(children.head)
            ArrayTreeFunctions.insertSlices(0, structure, content, structureBuffer, contentBuffer).intAsSome
          } else None
        } else {
          ArrayTreeFunctions
            .insertAfterChildren(
              structureBuffer.top,
              children.map(toSlices(_)),
              structureBuffer,
              contentBuffer,
              keepDistinct
            )
            .intAsSome
        }
      }

  /** Inserts multiple children before and after the existing children. */
  final def insertChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    before: Iterable[F[T1]],
    after: Iterable[F[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (before.isEmpty && after.isEmpty) target
    else
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        if (structureBuffer.isEmpty) None
        else {
          val delta1 =
            if (before.isEmpty) 0
            else
              ArrayTreeFunctions
                .insertBeforeChildren(
                  structureBuffer.top,
                  before.map(toSlices(_)),
                  structureBuffer,
                  contentBuffer,
                  keepDistinct
                )

          val delta2 =
            if (after.isEmpty) 0
            else
              ArrayTreeFunctions
                .insertAfterChildren(
                  structureBuffer.top,
                  after.map(toSlices(_)),
                  structureBuffer,
                  contentBuffer,
                  keepDistinct
                )

          Some(delta1 + delta2)
        }
      }

  /** Inserts a branch to the tree at an index while keeping children content distinct.
    * @return modified tree */
  final def insertBranch[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    branch: Iterable[T1],
    target: F[T],
    append: Boolean
  ): F[T1] =
    if (branch.isEmpty) target
    else {
      val iterator: Iterator[T1] = branch.iterator
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        if (iterator.hasNext && contentBuffer.nonEmpty && contentBuffer(index) != iterator.next()) None
        else {
          ArrayTreeFunctions.insertBranch(iterator, index, append, structureBuffer, contentBuffer, 0).nonZeroIntAsSome
        }
      }
    }

  /** Inserts multiple branches to the tree at an index while keeping children content distinct.
    * @return modified tree */
  final def insertBranches[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    branches: Iterable[Iterable[T1]],
    target: F[T],
    append: Boolean
  ): F[T1] =
    if (branches.isEmpty) target
    else {
      transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
        branches
          .foldLeft(0) { (delta, branch) =>
            val iterator: Iterator[T1] = branch.iterator
            delta +
              (if (iterator.hasNext && contentBuffer(index + delta) != iterator.next()) 0
               else {
                 ArrayTreeFunctions
                   .insertBranch(iterator, index + delta, append, structureBuffer, contentBuffer, 0)
               })
          }
          .intAsSome
      }
    }

  /** Updates value of the node at the index.
    * @return updated tree */
  final def updateValue[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    replacement: T1,
    tree: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) tree
    else {
      transform[F, T, T1](tree) { (structureBuffer, contentBuffer) =>
        if (replacement == contentBuffer(index)) None
        else {
          contentBuffer(index) = replacement
          val delta =
            if (keepDistinct)
              ArrayTreeFunctions.ensureChildDistinct(index, structureBuffer, contentBuffer)
            else 0
          Some(delta)
        }
      }
    }

  /** Updates tree at the index.
    * @return updated tree */
  final def updateTree[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    replacement: F[T1],
    tree: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    transform[F, T, T1](tree) { (structureBuffer, contentBuffer) =>
      val parentIndex = ArrayTreeFunctions.parentIndex(index, structureBuffer)
      val (structure, content) = toSlices(replacement)
      if (structure.isEmpty) {
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, contentBuffer).intAsSome
      } else {
        val hasSameHeadValue = keepDistinct && content.last == contentBuffer(index)
        val delta1 = ArrayTreeFunctions.removeChildren(index, structureBuffer, contentBuffer)
        val indexesToTrack = IntBuffer(index + delta1)
        val delta2 =
          if (keepDistinct)
            ArrayTreeFunctions
              .insertBetweenChildrenDistinct(
                index + delta1,
                structure,
                content,
                insertAfter = false,
                structureBuffer,
                contentBuffer,
                indexesToTrack
              )
          else {
            if (parentIndex + delta1 >= 0) {
              structureBuffer.increment(parentIndex + delta1)
            }
            IndexTracker.trackShiftRight(Math.max(0, index + delta1), structure.length, indexesToTrack)
            ArrayTreeFunctions
              .insertSlices(Math.max(0, index + delta1), structure, content, structureBuffer, contentBuffer)
          }

        val delta3 = if (!hasSameHeadValue) {
          val p = if (parentIndex >= 0) parentIndex + delta1 + delta2 else parentIndex
          val i = indexesToTrack.peek
          ArrayTreeFunctions.removeValue(i, p, structureBuffer, contentBuffer)
        } else 0

        Some(delta1 + delta2 + delta3)
      }
    }

  /** Updates value of the child node holding the given value. */
  final def updateChildValue[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    replacement: T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, content, rightmost)
      .filterNot(content(_) == replacement)
      .map(updateValue(_, replacement, target, keepDistinct))
      .getOrElse(target)
  }

  /** Updates value of the node selected by the path.
    * @return either modified tree or an existing */
  final def updateValueAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    replacement: T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateValue(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates value of the node selected by the path.
    * @return either modified tree or an existing */
  final def updateValueAt[F[+_]: Transformer, K, T, T1 >: T](
    path: Iterable[K],
    replacement: T1,
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateValue(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates the child node holding the given value. */
  final def updateChild[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    replacement: F[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, content, rightmost)
      .map(updateTree(_, replacement, target, keepDistinct))
      .getOrElse(target)
  }

  /** Updates a subtree selected by the path.
    * @return either modified tree or an existing */
  final def updateTreeAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    replacement: F[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateTree(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates a subtree selected by the path.
    * @return either modified tree or an existing */
  final def updateTreeAt[F[+_]: Transformer, K, T, T1 >: T](
    path: Iterable[K],
    replacement: F[T1],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateTree(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies value of the node at the index.
    * @return modified tree */
  final def modifyValue[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    modify: T => T1,
    target: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) target
    else {
      val (_, content) = toSlices(target)
      updateValue(index, modify(content(index)), target, keepDistinct)
    }

  /** Modifies value of the child node holding the given value. */
  final def modifyChildValue[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    modify: T => T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, content, rightmost)
      .map(modifyValue(_, modify, target, keepDistinct))
      .getOrElse(target)
  }

  /** Modifies value of the node selected by the path.
    * @return either modified tree or an existing */
  final def modifyValueAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    modify: T => T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyValue(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies value of the node selected by the path.
    * @return either modified tree or an existing */
  final def modifyValueAt[F[+_]: Transformer, K, T, T1 >: T](
    path: Iterable[K],
    modify: T => T1,
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyValue(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies the tree at the index.
    * @return modified tree */
  final def modifyTree[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    modify: F[T] => F[T1],
    target: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) target
    else updateTree(index, modify(instanceAt(index, target)), target, keepDistinct)

  /** Modifies children of the tree at the index.
    * @return modified tree */
  final def modifyChildren[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    modify: Iterable[F[T]] => Iterable[F[T1]],
    target: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) target
    else updateTree(index, modifyChildren(modify, instanceAt(index, target)), target, keepDistinct)

  /** Looks for Some index of the child node holding the given value, or None.
    * @param rightmost whether to select first (false) or last (true) occurrence of the value
    * @return some first or last index if exists or none */
  private final def childHavingValue[F[+_]: Transformer, T](value: T, target: F[T], rightmost: Boolean): Option[Int] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, content, rightmost)
  }

  /** Modifies the child node holding the given value.
    * @return modified tree */
  final def modifyChild[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    modify: F[T] => F[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] =
    childHavingValue(value, target, rightmost)
      .map(modifyTree(_, modify, target, keepDistinct))
      .getOrElse(target)

  /** Modifies children of this tree.
    * @return modified tree */
  final def modifyChildren[F[+_]: Transformer, T, T1 >: T](
    modify: Iterable[F[T]] => Iterable[F[T1]],
    target: F[T]
  ): F[T1] =
    transform[F, T, T1](target) { (structureBuffer, contentBuffer) =>
      val (structure, content) = toSlices(target)
      val newChildren = modify(
        iterableFrom(
          ArrayTreeFunctions
            .childrenIndexesIterator(structureBuffer.top, structureBuffer)
            .map(instanceAt2(_, structure, content))
        )
      )
      val delta1 = ArrayTreeFunctions.removeChildren(structureBuffer.top, structureBuffer, contentBuffer)
      val delta2 = ArrayTreeFunctions
        .insertChildren(
          0,
          newChildren.map(toSlices(_)),
          structureBuffer,
          contentBuffer,
          append = true,
          keepDistinct = false
        )
      Some(delta1 + delta2)
    }

  /** Modifies a subtree selected by the path.
    * @return either modified tree or an existing */
  final def modifyTreeAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    modify: F[T] => F[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, content, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyTree(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))
  }

  /** Modifies a subtree selected by the path.
    * @return either modified tree or an existing */
  final def modifyTreeAt[F[+_]: Transformer, K, T, T1 >: T](
    path: Iterable[K],
    modify: F[T] => F[T1],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, content, toPathItem, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyTree(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))
  }

  /** Modifies children of a node selected by the path.
    * @return either modified tree or an existing */
  final def modifyChildrenAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    modify: Iterable[F[T]] => Iterable[F[T1]],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, content) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, content, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyChildren(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))
  }

  /** Modifies children of a node selected by the path.
    * @return either modified tree or an existing */
  final def modifyChildrenAt[F[+_]: Transformer, K, T, T1 >: T](
    path: Iterable[K],
    modify: Iterable[F[T]] => Iterable[F[T1]],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(modifyChildren(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Removes node at the index and merges children to parent.
    * @return updated tree */
  final def removeValue[F[+_]: Transformer, T](
    index: Int,
    parentIndexOpt: Option[Int],
    target: F[T],
    keepDistinct: Boolean
  ): F[T] =
    if (index < 0) target
    else
      transform(target) { (structureBuffer, contentBuffer) =>
        if (index == structureBuffer.top && structureBuffer(index) > 1)
          throw new RuntimeException("Cannot remove top tree node having more than a single child.")
        else {
          val parentIndex = parentIndexOpt
            .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
          ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, contentBuffer, keepDistinct).intAsSome
        }
      }

  /** Removes the node addressed by the last index, inserts children into the parent. */
  private def removeValue[F[+_]: Transformer, T](indexes: IntSlice, target: F[T], keepDistinct: Boolean): F[T] =
    if (indexes.isEmpty) target
    else {
      val (structure, _) = toSlices(target)
      if (indexes.last == structure.top) {
        if (structure.length == 1) {
          transform[F, T, T](target) {
            case (structureBuffer, contentBuffer) =>
              structureBuffer.remove(0)
              contentBuffer.remove(0)
              Some(-1)
          }
        } else if (structure.last == 1) instanceAt(structure.top - 1, target)
        else target
      } else {
        transform[F, T, T](target) {
          case (structureBuffer, contentBuffer) =>
            val index = indexes.last
            if (index == structureBuffer.top && structureBuffer(index) > 1)
              throw new RuntimeException("Cannot remove top tree node having more than a single child.")
            else {
              val parentIndex = indexes
                .get(indexes.length - 2)
                .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
              ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, contentBuffer, keepDistinct).intAsSome
            }
        }
      }
    }

  /** Removes the direct child node holding the value, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeChildValue[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T] =
    transform(target) { (structureBuffer, contentBuffer) =>
      ArrayTreeFunctions
        .childHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, contentBuffer, rightmost)
        .map { index =>
          ArrayTreeFunctions.removeValue(index, structureBuffer.top, structureBuffer, contentBuffer, keepDistinct)
        }
    }

  /** Removes the node selected by the path, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeValueAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T] =
    followEntirePath(path, target, rightmost)
      .map(removeValue(_, target, keepDistinct))
      .getOrElse(target)

  /** Removes the node selected by the path using an extractor function, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeValueAt[F[+_]: Transformer, K, T](
    path: Iterable[K],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(removeValue(_, target, keepDistinct))
      .getOrElse(target)

  /** Removes the tree at the index.
    * @return updated tree */
  final def removeTree[F[+_]: Transformer, T](
    index: Int,
    parentIndexOpt: Option[Int],
    target: F[T]
  ): F[T] =
    if (index < 0) target
    else
      transform(target) { (structureBuffer, contentBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, contentBuffer).intAsSome
      }

  /** Removes the direct child of the node, holding the value.
    * @return modified tree */
  final def removeChild[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    value: T1,
    rightmost: Boolean
  ): F[T] =
    transform(target) { (structureBuffer, contentBuffer) =>
      ArrayTreeFunctions
        .childHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, contentBuffer, rightmost)
        .map(index => ArrayTreeFunctions.removeTree(index, structureBuffer.top, structureBuffer, contentBuffer))
    }

  /** Removes the direct children of the node.
    * @return modified tree */
  final def removeChildren[F[+_]: Transformer, T](
    parentIndex: Int,
    target: F[T]
  ): F[T] =
    transform(target) { (structureBuffer, contentBuffer) =>
      ArrayTreeFunctions.removeChildren(parentIndex, structureBuffer, contentBuffer).intAsSome
    }

  /** Removes the tree selected by the path.
    * @return modified tree */
  final def removeTreeAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean
  ): F[T] =
    followEntirePath(path, target, rightmost)
      .map(indexes => removeTree(indexes.last, indexes.get(indexes.length - 2), target))
      .getOrElse(target)

  /** Removes the tree selected by the path using an extractor function.
    * @return modified tree */
  final def removeTreeAt[F[+_]: Transformer, K, T](
    path: Iterable[K],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): F[T] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes => removeTree(indexes.last, indexes.get(indexes.length - 2), target))
      .getOrElse(target)

  /** Removes children of the tree selected by the path.
    * @return modified tree */
  final def removeChildrenAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean
  ): F[T] =
    followEntirePath(path, target, rightmost)
      .map(indexes => removeChildren(indexes.last, target))
      .getOrElse(target)

  /** Removes children of the tree selected by the path using an extractor function.
    * @return modified tree */
  final def removeChildrenAt[F[+_]: Transformer, K, T](
    path: Iterable[K],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): F[T] =
    followEntirePath(path, target, toPathItem, rightmost)
      .map(indexes => removeChildren(indexes.last, target))
      .getOrElse(target)

}
