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
import com.github.arturopala.tree.internal.IntOps._
import com.github.arturopala.tree.internal.IterableOps.iterableFrom
import com.github.arturopala.tree.{Tree, TreeBuilder}

import scala.collection.Iterator.continually

/**
  * Collection of high-level operations on the linear encoding of the tree.
  *
  * @note For low-level functions look into [[ArrayTreeFunctions]].
  */
object ArrayTree {

  /** Iterates right-to-left over all leaves of the tree rooted at startIndex. */
  def leavesIterator[T](startIndex: Int, treeStructure: IntSlice, treeValues: Int => T): Iterator[T] = {
    val treeSize =
      if (startIndex == treeStructure.top) treeStructure.length
      else ArrayTreeFunctions.treeSize(startIndex, treeStructure)
    treeStructure
      .slice(startIndex - treeSize + 1, startIndex + 1)
      .reverseIndexIterator(_ == 0)
      .map(treeValues)
  }

  /** Iterates over all values of the tree rooted at startIndex.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def valuesIterator[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    depthFirst: Boolean
  ): Iterator[T] =
    (if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirst(startIndex, treeStructure)
     else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, treeStructure))
      .map(treeValues)

  /** Iterates over filtered values of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def valuesIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: T => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[T] =
    new MapFilterIterator[Int, T](
      if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirstWithLimit(startIndex, treeStructure, maxDepth)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirstWithLimit(startIndex, treeStructure, maxDepth),
      treeValues,
      pred
    )

  /** Iterates over filtered tuples of (level, value, isLeaf) of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def valuesAndLevelsIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: T => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, T, Boolean)] =
    new MapFilterIterator[(Int, Int), (Int, T, Boolean)](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexAndLevelIteratorDepthFirstWithLimit(startIndex, treeStructure, maxDepth)
      else
        ArrayTreeFunctions.nodesIndexAndLevelIteratorBreadthFirstWithLimit(startIndex, treeStructure, maxDepth),
      { case (level: Int, index: Int) => (level, treeValues(index), treeStructure(index) == 0) },
      (t: (Int, T, Boolean)) => pred(t._2)
    )

  /** Iterates over all paths of the tree. */
  final def pathsIterator[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Iterator[Iterable[T]] =
    ArrayTreeFunctions
      .pathsIndexListIterator(startIndex, treeStructure)
      .map(_.map(treeValues))

  /** Iterates over filtered tree's paths. */
  final def pathsIteratorWithFilter[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.pathsIndexListIterator(startIndex, treeStructure),
      _.map(treeValues),
      pred
    )

  /** Iterates over filtered tree's path with depth limit. */
  final def pathsIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean,
    maxDepth: Int
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.pathsIndexListIterator(startIndex, treeStructure, maxDepth),
      _.map(treeValues),
      pred
    )

  /** Iterates over all branches of the tree. */
  final def branchesIterator[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Iterator[Iterable[T]] =
    ArrayTreeFunctions
      .branchesIndexListIterator(startIndex, treeStructure)
      .map(_.map(treeValues))

  /** Iterates over filtered tree's branches. */
  final def branchesIteratorWithFilter[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, treeStructure),
      _.map(treeValues),
      pred
    )

  /** Iterates over filtered tree's branches with depth limit. */
  final def branchesIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean,
    maxDepth: Int
  ): Iterator[Iterable[T]] =
    new MapFilterIterator[IntBuffer, Iterable[T]](
      ArrayTreeFunctions.branchesIndexListIterator(startIndex, treeStructure, maxDepth),
      _.map(treeValues),
      pred
    )

  /** Iterates over all subtrees (including the tree itself).
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesIterator[F[+_]: Transformer, T](
    startIndex: Int,
    target: F[T],
    depthFirst: Boolean
  ): Iterator[Tree[T]] = {
    val (treeStructure, treeValues) = toSlices(target)
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    (if (depthFirst)
       ArrayTreeFunctions
         .nodesIndexIteratorDepthFirst(startIndex, treeStructure)
     else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, treeStructure))
      .map(treeAt2(_, treeStructure, treeValues))
  }

  /** Iterates over filtered subtrees (including the tree itself).
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithFilter[T](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean,
    depthFirst: Boolean
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    new MapFilterIterator[Int, Tree[T]](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexIteratorDepthFirst(startIndex, treeStructure)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, treeStructure),
      treeAt2(_, treeStructure, treeValues),
      pred
    )
  }

  /** Iterates over filtered subtrees (including the tree itself) with depth limit.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    new MapFilterIterator[Int, Tree[T]](
      if (depthFirst) ArrayTreeFunctions.nodesIndexIteratorDepthFirstWithLimit(startIndex, treeStructure, maxDepth)
      else ArrayTreeFunctions.nodesIndexIteratorBreadthFirstWithLimit(startIndex, treeStructure, maxDepth),
      treeAt2(_, treeStructure, treeValues),
      pred
    )
  }

  /** Iterates over filtered pairs of (level, tree) of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesAndLevelsIteratorWithLimit[T](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    pred: Tree[T] => Boolean,
    maxDepth: Int,
    depthFirst: Boolean
  ): Iterator[(Int, Tree[T])] =
    new MapFilterIterator[(Int, Int), (Int, Tree[T])](
      if (depthFirst)
        ArrayTreeFunctions.nodesIndexAndLevelIteratorDepthFirstWithLimit(startIndex, treeStructure, maxDepth)
      else
        ArrayTreeFunctions.nodesIndexAndLevelIteratorBreadthFirstWithLimit(startIndex, treeStructure, maxDepth),
      { case (level: Int, index: Int) => (level, treeAt2(index, treeStructure, treeValues)) },
      (t: (Int, Tree[T])) => pred(t._2)
    )

  /** Follows the given path of values into the tree.
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
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions.followPath(path, structure.top, structure, values, rightmost)
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
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions.followPath(path, structure.top, structure, values, toPathItem, rightmost)
  }

  /** Follows the entire path of values into the tree.
    * @return a Some of an array of travelled indexes, or None if path doesn't exist.
    */
  @`inline` final def followEntirePath[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    target: F[T],
    rightmost: Boolean
  ): Option[IntSlice] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, structure.top, structure, values, rightmost)
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
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions.followEntirePath(path, structure.top, structure, values, toPathItem, rightmost)
  }

  /** Checks if the tree starting at parentIndex contains given direct child value. */
  @`inline` final def containsChildValue[T, T1 >: T](
    value: T1,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean =
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, treeStructure)
      .exists(i => treeValues(i) == value)

  /** Checks if the tree starting at parentIndex contains given direct child. */
  @`inline` final def containsChild[T, T1 >: T](
    child: Tree[T1],
    parentIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Boolean =
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, treeStructure)
      .exists(i => treeAt2(i, treeStructure, treeValues) == child)

  /** Checks if the tree starting at parentIndex contains direct child value fulfilling the predicate. */
  @`inline` final def existsChildValue[T, T1 >: T](
    pred: T1 => Boolean,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean =
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, treeStructure)
      .exists(i => pred(treeValues(i)))

  /** Checks if the tree starting at parentIndex contains direct child fulfilling the predicate. */
  @`inline` final def existsChild[T, T1 >: T](
    pred: Tree[T1] => Boolean,
    parentIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Boolean =
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, treeStructure)
      .exists(i => pred(treeAt2(i, treeStructure, treeValues)))

  /** Checks if the tree starting at parentIndex contains given branch. */
  @`inline` final def containsBranch[T, T1 >: T](
    branch: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, fullMatch) =
      ArrayTreeFunctions.followPath(branch, startIndex, treeStructure, treeValues, rightmost = false)
    fullMatch && unmatched.isEmpty
  }

  /** Count branches starting at index fulfilling the predicate. */
  final def countBranches[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: Iterable[T] => Boolean
  ): Int =
    ArrayTreeFunctions.foldLeftBranchesIndexLists(
      startIndex,
      treeStructure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.map(treeValues).asIterable)) 1 else 0)
    )

  /** Checks if the tree starting at parentIndex contains given path (a branch prefix). */
  @`inline` final def containsPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean =
    ArrayTreeFunctions.followEntirePath(path, startIndex, treeStructure, treeValues, rightmost = false).isDefined

  /** Selects node's value accessible by path using item extractor function. */
  @`inline` final def selectValue[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[T] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues, toPathItem, rightmost)
      .map(indexes => treeValues(indexes.last))

  /** Selects tree accessible by path. */
  @`inline` final def selectTree[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    rightmost: Boolean
  ): Option[Tree[T]] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues, rightmost)
      .map(indexes => treeAt2(indexes.last, treeStructure, treeValues))

  /** Selects tree accessible by path using item extractor function. */
  @`inline` final def selectTree[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    toPathItem: T => K,
    rightmost: Boolean
  ): Option[Tree[T]] =
    ArrayTreeFunctions
      .followEntirePath(path, startIndex, treeStructure, treeValues, toPathItem, rightmost)
      .map(indexes => treeAt2(indexes.last, treeStructure, treeValues))

  // TRANSFORMATIONS

  /** Returns tree rooted at the given index. */
  final def treeAt[F[+_]: Transformer, T](index: Int, target: F[T]): Tree[T] = {
    val (treeStructure, treeValues) = toSlices(target)
    val (childStructure, childValues) = ArrayTreeFunctions.treeAt(index, treeStructure, treeValues)
    Tree.TreeTransformer.fromSlices(childStructure, childValues)
  }

  /** Returns tree rooted at the given index. */
  final def treeAt2[T](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): Tree[T] = {
    val (structure, values) = ArrayTreeFunctions.treeAt(index, treeStructure, treeValues)
    Tree.TreeTransformer.fromSlices(structure, values)
  }

  /** Returns instance rooted at the given index. */
  final def instanceAt[F[+_]: Transformer, T](index: Int, target: F[T]): F[T] = {
    val (treeStructure, treeValues) = toSlices(target)
    if (index == treeStructure.top) target
    else {
      val (childStructure, childValues) = ArrayTreeFunctions.treeAt(index, treeStructure, treeValues)
      fromSlices(childStructure, childValues)
    }
  }

  /** Transforms the tree using provided function, or returns unmodified if result in None.
    * Before modification decomposes the tree into the buffers, and assembles it again after.
    * @tparam T type of the input tree values
    * @tparam T1 type of the output tree values
    */
  private final def transform[F[+_]: Transformer, T, T1 >: T](
    target: F[T]
  )(modify: (IntBuffer, Buffer[T1]) => Option[Int]): F[T1] = {
    val (structureBuffer, valuesBuffer) = implicitly[Transformer[F]].toBuffers[T, T1](target)
    modify(structureBuffer, valuesBuffer) match {
      case None    => target
      case Some(_) => implicitly[Transformer[F]].fromBuffers(structureBuffer, valuesBuffer)
    }
  }

  /** Creates an instance from a pair of slices. */
  @`inline` private final def fromSlices[F[+_]: Transformer, T](structure: IntSlice, values: Slice[T]): F[T] =
    implicitly[Transformer[F]].fromSlices(structure, values)

  /** Outputs tree linearisation as a pair of slices. */
  @`inline` private final def toSlices[F[+_]: Transformer, T](target: F[T]): (IntSlice, Slice[T]) =
    implicitly[Transformer[F]].toSlices(target)

  /** FlatMaps the tree without checking for duplicated children. */
  final def flatMapLax[F[+_]: Transformer, T, K](
    target: F[T],
    f: T => Tree[K]
  ): F[K] = {

    val (treeStructure, treeValues) = toSlices(target)

    assert(treeStructure.length == treeValues.length, "Structure and values arrays of the tree MUST be the same size.")

    val structureBuffer = treeStructure.asBuffer
    val valuesBuffer = Buffer.ofSize[K](treeValues.length)

    var index = 0
    var offset = 0

    while (index < treeStructure.length) {
      val tree = f(treeValues(index))
      if (tree.isEmpty) {
        val parent = ArrayTreeFunctions.parentIndex(index, treeStructure) + offset
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, valuesBuffer)
        offset = offset - 1
      } else if (tree.isLeaf) {
        valuesBuffer(index + offset) = tree.head
      } else {
        val (structure, values) = tree.toSlices
        val delta =
          ArrayTreeFunctions.expandValueIntoTreeLax(index + offset, structure, values, structureBuffer, valuesBuffer)
        offset = offset + delta
      }
      index = index + 1
    }

    if (ArrayTreeFunctions.hasValidTreeStructure(structureBuffer))
      fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)
    else
      implicitly[Transformer[F]].empty
  }

  /** FlatMaps the tree while keeping children distinct. */
  final def flatMapDistinct[F[+_]: Transformer, T, K](
    target: F[T],
    f: T => Tree[K]
  ): F[K] = {

    val (treeStructure, treeValues) = toSlices(target)

    assert(treeStructure.length == treeValues.length, "Structure and values arrays of the tree MUST be the same size.")

    val structureBuffer = treeStructure.asBuffer
    val valuesBuffer = Buffer.ofSize[K](treeValues.length)

    var index = 0
    var offset = 0

    while (index < treeStructure.length) {
      val parent = ArrayTreeFunctions.parentIndex(index + offset, structureBuffer)
      val tree = f(treeValues(index))
      if (tree.isEmpty) {
        ArrayTreeFunctions.removeValue(index + offset, parent, structureBuffer, valuesBuffer)
        offset = offset - 1
      } else {
        val (structure, values) = tree.toSlices
        val delta =
          ArrayTreeFunctions
            .expandValueIntoTreeDistinct(index + offset, parent, structure, values, structureBuffer, valuesBuffer)
        offset = offset + delta
      }
      index = index + 1
    }

    if (ArrayTreeFunctions.hasValidTreeStructure(structureBuffer))
      fromSlices(structureBuffer.asSlice, valuesBuffer.asSlice)
    else
      implicitly[Transformer[F]].empty
  }

  /** Inserts a value to a sub-tree rooted at the path.
    * @param path sequence of a node's values forming a path to a sub-tree
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
    val (structure, values) = toSlices(target)
    val (indexes, unmatched, remaining, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, values, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val valueSequence = remaining.toVector.+:(item).:+(value)
            val newNode: Tree[T1] = TreeBuilder.linearTreeFromSequence(valueSequence)
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
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions.firstChildHavingValue(value, index, structure.length, structure, values).isDefined
  }

  /** Prepends tree with the new head value. */
  final def prepend[F[+_]: Transformer, T, T1 >: T](value: T1, target: F[T]): F[T1] =
    transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
      structureBuffer.push(1)
      valuesBuffer.push(value)
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
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        val insertIndex =
          if (append) ArrayTreeFunctions.bottomIndex(parentIndex, structureBuffer)
          else parentIndex
        ArrayTreeFunctions.insertValue(insertIndex, parentIndex, value, structureBuffer, valuesBuffer).intAsSome
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
    transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
      val toInsert = if (keepDistinct) {
        val existing = ArrayTreeFunctions.childrenIndexes(parentIndex, structureBuffer).map(valuesBuffer)
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
          valuesBuffer
        )
        .nonZeroIntAsSome
    }

  /** Inserts a subtree to a tree at a path.
    * @return modified tree */
  final def insertChildAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    child: Tree[T1],
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, values) = toSlices(target)
    val (indexes, unmatched, remaining, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, values, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) => {
            transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
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
                  valuesBuffer
                )
              val (structure, values) = child.toSlices
              val delta2 = ArrayTreeFunctions.insertSlice(insertIndex, structure, values, structureBuffer, valuesBuffer)
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
    child: Tree[T1],
    target: F[T],
    toPathItem: T => K,
    append: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, values) = toSlices(target)
    val (indexes, unmatched, _, _) =
      ArrayTreeFunctions.followPath(path, structure.top, structure, values, toPathItem, append)
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
    children: Iterable[Tree[T1]],
    target: F[T],
    append: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (indexes, unmatched, remaining, _) = followPath(path, target, append)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
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
                  valuesBuffer
                )
              (delta, insertIndex)
            }
            .getOrElse((0, index))

          val delta2 = ArrayTreeFunctions
            .insertChildren(
              index2,
              children.map(_.toSlices),
              structureBuffer,
              valuesBuffer,
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
    children: Iterable[Tree[T1]],
    target: F[T],
    toPathItem: T => K,
    append: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (indexes, unmatched, _, _) = followPath(path, target, toPathItem, append)
    indexes.lastOption match {
      case Some(index) if unmatched.isEmpty =>
        Right(transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
          ArrayTreeFunctions
            .insertChildren(index, children.map(_.toSlices), structureBuffer, valuesBuffer, append, keepDistinct)
            .intAsSome
        })

      case _ => Left(target)
    }
  }

  /** Prepends children with the new child without checking for duplicates. */
  final def prependChild[F[+_]: Transformer, T, T1 >: T](target: F[T], child: Tree[T1]): F[T1] = {
    val (structure, _) = toSlices(target)
    insertTreeAtIndex(structure.top, structure.top, child, target)
  }

  /** Appends children with the new child without checking for duplicates. */
  final def appendChild[F[+_]: Transformer, T, T1 >: T](target: F[T], child: Tree[T1]): F[T1] = {
    val (structure, _) = toSlices(target)
    insertTreeAtIndex(0, structure.top, child, target)
  }

  /** Inserts a subtree to a tree at an index.
    * @return modified tree */
  final def insertTreeAtIndex[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    parentIndex: Int,
    child: Tree[T1],
    target: F[T]
  ): F[T1] =
    if (child.isEmpty) target
    else {
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = child.toSlices
        if (parentIndex >= 0 && structureBuffer.nonEmpty) structureBuffer.increment(parentIndex)
        ArrayTreeFunctions.insertSlice(index, structure, values, structureBuffer, valuesBuffer).intAsSome
      }
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertChildDistinct[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    child: Tree[T1],
    target: F[T],
    append: Boolean
  ): F[T1] =
    if (child.isEmpty) target
    else {
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = child.toSlices
        (if (append)
           ArrayTreeFunctions
             .insertAfterChildDistinct(index, structure, values, structureBuffer, valuesBuffer)
         else
           ArrayTreeFunctions
             .insertBeforeChildDistinct(index, structure, values, structureBuffer, valuesBuffer)).nonZeroIntAsSome
      }
    }

  /** Inserts multiple children before the existing children. */
  final def insertBeforeChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    children: Iterable[Tree[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (children.isEmpty) target
    else
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        if (structureBuffer.isEmpty) None
        else {
          ArrayTreeFunctions
            .insertBeforeChildren(
              structureBuffer.top,
              children.map(_.toSlices),
              structureBuffer,
              valuesBuffer,
              keepDistinct
            )
            .intAsSome
        }
      }

  /** Inserts multiple children after the existing children. */
  final def insertAfterChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    children: Iterable[Tree[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (children.isEmpty) target
    else
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        if (structureBuffer.isEmpty) None
        else {
          ArrayTreeFunctions
            .insertAfterChildren(
              structureBuffer.top,
              children.map(_.toSlices),
              structureBuffer,
              valuesBuffer,
              keepDistinct
            )
            .intAsSome
        }
      }

  /** Inserts multiple children before and after the existing children. */
  final def insertChildren[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    before: Iterable[Tree[T1]],
    after: Iterable[Tree[T1]],
    keepDistinct: Boolean
  ): F[T1] =
    if (before.isEmpty && after.isEmpty) target
    else
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        if (structureBuffer.isEmpty) None
        else {
          val delta1 =
            if (before.isEmpty) 0
            else
              ArrayTreeFunctions
                .insertBeforeChildren(
                  structureBuffer.top,
                  before.map(_.toSlices),
                  structureBuffer,
                  valuesBuffer,
                  keepDistinct
                )

          val delta2 =
            if (after.isEmpty) 0
            else
              ArrayTreeFunctions
                .insertAfterChildren(
                  structureBuffer.top,
                  after.map(_.toSlices),
                  structureBuffer,
                  valuesBuffer,
                  keepDistinct
                )

          Some(delta1 + delta2)
        }
      }

  /** Inserts a branch to the tree at an index while keeping children values distinct.
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
      transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
        if (iterator.hasNext && valuesBuffer.nonEmpty && valuesBuffer(index) != iterator.next()) None
        else {
          ArrayTreeFunctions.insertBranch(iterator, index, append, structureBuffer, valuesBuffer, 0).nonZeroIntAsSome
        }
      }
    }

  /** Inserts multiple branches to the tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertBranches[T, T1 >: T](
    index: Int,
    branches: Iterable[Iterable[T1]],
    target: Tree[T],
    append: Boolean
  ): Tree[T1] =
    if (branches.isEmpty) target
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      transform[Tree, T, T1](target) { (structureBuffer, valuesBuffer) =>
        branches
          .foldLeft(0) { (delta, branch) =>
            val iterator: Iterator[T1] = branch.iterator
            delta +
              (if (iterator.hasNext && valuesBuffer(index + delta) != iterator.next()) 0
               else {
                 ArrayTreeFunctions
                   .insertBranch(iterator, index + delta, append, structureBuffer, valuesBuffer, 0)
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
      transform[F, T, T1](tree) { (structureBuffer, valuesBuffer) =>
        if (replacement == valuesBuffer(index)) None
        else {
          valuesBuffer(index) = replacement
          val delta =
            if (keepDistinct)
              ArrayTreeFunctions.ensureChildDistinct(index, structureBuffer, valuesBuffer)
            else 0
          Some(delta)
        }
      }
    }

  /** Updates tree at the index.
    * @return updated tree */
  final def updateTree[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    replacement: Tree[T1],
    tree: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    transform[F, T, T1](tree) { (structureBuffer, valuesBuffer) =>
      val parentIndex = ArrayTreeFunctions.parentIndex(index, structureBuffer)
      val (structure, values) = toSlices(replacement)
      if (structure.isEmpty) {
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer).intAsSome
      } else {
        val hasSameHeadValue = keepDistinct && values.last == valuesBuffer(index)
        val delta1 = ArrayTreeFunctions.removeChildren(index, structureBuffer, valuesBuffer)
        val indexesToTrack = IntBuffer(index + delta1)
        val delta2 =
          if (keepDistinct)
            ArrayTreeFunctions
              .insertBetweenChildrenDistinct(
                index + delta1,
                structure,
                values,
                insertAfter = false,
                structureBuffer,
                valuesBuffer,
                indexesToTrack
              )
          else {
            if (parentIndex + delta1 >= 0) {
              structureBuffer.increment(parentIndex + delta1)
            }
            IndexTracker.trackShiftRight(Math.max(0, index + delta1), structure.length, indexesToTrack)
            ArrayTreeFunctions
              .insertSlice(Math.max(0, index + delta1), structure, values, structureBuffer, valuesBuffer)
          }

        val delta3 = if (!hasSameHeadValue) {
          val p = if (parentIndex >= 0) parentIndex + delta1 + delta2 else parentIndex
          val i = indexesToTrack.peek
          ArrayTreeFunctions.removeValue(i, p, structureBuffer, valuesBuffer)
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
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, values, rightmost)
      .filterNot(values(_) == replacement)
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
    replacement: Tree[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, values, rightmost)
      .map(updateTree(_, replacement, target, keepDistinct))
      .getOrElse(target)
  }

  /** Updates a subtree selected by the path.
    * @return either modified tree or an existing */
  final def updateTreeAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    replacement: Tree[T1],
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
    replacement: Tree[T1],
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
      val (_, values) = toSlices(target)
      updateValue(index, modify(values(index)), target, keepDistinct)
    }

  /** Modifies value of the child node holding the given value. */
  final def modifyChildValue[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    modify: T => T1,
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): F[T1] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, values, rightmost)
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
    modify: Tree[T] => Tree[T1],
    target: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) target
    else updateTree(index, modify(treeAt(index, target)), target, keepDistinct)

  /** Modifies children of the tree at the index.
    * @return modified tree */
  final def modifyChildren[F[+_]: Transformer, T, T1 >: T](
    index: Int,
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    target: F[T],
    keepDistinct: Boolean
  ): F[T1] =
    if (index < 0) target
    else updateTree(index, modifyChildren(modify, treeAt(index, target)), target, keepDistinct)

  /** Looks for Some index of the child node holding the given value, or None.
    * @param rightmost whether to select first (false) or last (true) occurrence of the value
    * @return some first or last index if exists or none */
  private final def childHavingValue[F[+_]: Transformer, T](value: T, target: F[T], rightmost: Boolean): Option[Int] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .childHavingValue(value, structure.top, structure.length, structure, values, rightmost)
  }

  /** Modifies the child node holding the given value.
    * @return modified tree */
  final def modifyChild[F[+_]: Transformer, T, T1 >: T](
    value: T1,
    modify: Tree[T] => Tree[T1],
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
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    target: F[T]
  ): F[T1] =
    transform[F, T, T1](target) { (structureBuffer, valuesBuffer) =>
      val (structure, values) = toSlices(target)
      val newChildren = modify(
        iterableFrom(
          ArrayTreeFunctions
            .childrenIndexesIterator(structureBuffer.top, structureBuffer)
            .map(treeAt2(_, structure, values))
        )
      )
      val delta1 = ArrayTreeFunctions.removeChildren(structureBuffer.top, structureBuffer, valuesBuffer)
      val delta2 = ArrayTreeFunctions
        .insertChildren(
          0,
          newChildren.map(_.toSlices),
          structureBuffer,
          valuesBuffer,
          append = true,
          keepDistinct = false
        )
      Some(delta1 + delta2)
    }

  /** Modifies a subtree selected by the path.
    * @return either modified tree or an existing */
  final def modifyTreeAt[F[+_]: Transformer, T, T1 >: T](
    path: Iterable[T1],
    modify: Tree[T] => Tree[T1],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, values, rightmost)
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
    modify: Tree[T] => Tree[T1],
    target: F[T],
    toPathItem: T => K,
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, values, toPathItem, rightmost)
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
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
    target: F[T],
    rightmost: Boolean,
    keepDistinct: Boolean
  ): Either[F[T], F[T1]] = {
    val (structure, values) = toSlices(target)
    ArrayTreeFunctions
      .followEntirePath(path, structure.top, structure, values, rightmost)
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
    modify: Iterable[Tree[T]] => Iterable[Tree[T1]],
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
      transform(target) { (structureBuffer, valuesBuffer) =>
        if (index == structureBuffer.top && structureBuffer(index) > 1)
          throw new RuntimeException("Cannot remove top tree node having more than a single child.")
        else {
          val parentIndex = parentIndexOpt
            .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
          ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, valuesBuffer, keepDistinct).intAsSome
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
            case (structureBuffer, valuesBuffer) =>
              structureBuffer.remove(0)
              valuesBuffer.remove(0)
              Some(-1)
          }
        } else if (structure.last == 1) instanceAt(structure.top - 1, target)
        else target
      } else {
        transform[F, T, T](target) {
          case (structureBuffer, valuesBuffer) =>
            val index = indexes.last
            if (index == structureBuffer.top && structureBuffer(index) > 1)
              throw new RuntimeException("Cannot remove top tree node having more than a single child.")
            else {
              val parentIndex = indexes
                .get(indexes.length - 2)
                .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
              ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, valuesBuffer, keepDistinct).intAsSome
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
    transform(target) { (structureBuffer, valuesBuffer) =>
      ArrayTreeFunctions
        .childHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, valuesBuffer, rightmost)
        .map { index =>
          ArrayTreeFunctions.removeValue(index, structureBuffer.top, structureBuffer, valuesBuffer, keepDistinct)
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
      transform(target) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer).intAsSome
      }

  /** Removes the direct child of the node, holding the value.
    * @return modified tree */
  final def removeChild[F[+_]: Transformer, T, T1 >: T](
    target: F[T],
    value: T1,
    rightmost: Boolean
  ): F[T] =
    transform(target) { (structureBuffer, valuesBuffer) =>
      ArrayTreeFunctions
        .childHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, valuesBuffer, rightmost)
        .map(index => ArrayTreeFunctions.removeTree(index, structureBuffer.top, structureBuffer, valuesBuffer))
    }

  /** Removes the direct children of the node.
    * @return modified tree */
  final def removeChildren[F[+_]: Transformer, T](
    parentIndex: Int,
    target: F[T]
  ): F[T] =
    transform(target) { (structureBuffer, valuesBuffer) =>
      ArrayTreeFunctions.removeChildren(parentIndex, structureBuffer, valuesBuffer).intAsSome
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
