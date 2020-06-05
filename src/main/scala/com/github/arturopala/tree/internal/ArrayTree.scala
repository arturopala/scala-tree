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
import com.github.arturopala.tree.Tree.ArrayTree
import com.github.arturopala.tree.internal.ArrayTreeFunctions.bottomIndex
import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.internal.IntOps._

import scala.collection.Iterator.continually
import scala.reflect.ClassTag

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
  final def valuesIterator[T: ClassTag](
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
  final def valuesIteratorWithLimit[T: ClassTag](
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
  final def valuesAndLevelsIteratorWithLimit[T: ClassTag](
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

  /** Iterates over filtered tree's branches. */
  final def branchesIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Iterator[Iterable[T]] =
    ArrayTreeFunctions
      .branchesIndexListIterator(startIndex, treeStructure)
      .map(_.map(treeValues))

  /** Iterates over filtered tree's branches. */
  final def branchesIteratorWithFilter[T: ClassTag](
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
  final def branchesIteratorWithLimit[T: ClassTag](
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
  final def treesIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    depthFirst: Boolean
  ): Iterator[Tree[T]] = {
    assert(
      treeStructure.length == treeValues.length,
      "When iterating over the tree's subtrees, structure and values mst be the same size."
    )
    (if (depthFirst)
       ArrayTreeFunctions
         .nodesIndexIteratorDepthFirst(startIndex, treeStructure)
     else ArrayTreeFunctions.nodesIndexIteratorBreadthFirst(startIndex, treeStructure))
      .map(treeAt(_, treeStructure, treeValues))
  }

  /** Iterates over filtered subtrees (including the tree itself).
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithFilter[T: ClassTag](
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
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Iterates over filtered subtrees (including the tree itself) with depth limit.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesIteratorWithLimit[T: ClassTag](
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
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Iterates over filtered pairs of (level, tree) of the tree.
    * @param depthFirst if true, enumerates values depth-first,
    *                   if false, breadth-first. */
  final def treesAndLevelsIteratorWithLimit[T: ClassTag](
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
      { case (level: Int, index: Int) => (level, treeAt(index, treeStructure, treeValues)) },
      (t: (Int, Tree[T])) => pred(t._2)
    )

  /** Checks if the tree contains given direct child value. */
  @`inline` final def containsChild[T, T1 >: T](
    value: T1,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean =
    ArrayTreeFunctions
      .childrenIndexesIterator(parentIndex, treeStructure)
      .exists(i => treeValues(i) == value)

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
    pred: Iterable[T] => Boolean
  ): Int =
    ArrayTreeFunctions.foldLeftBranchesIndexLists(
      startIndex,
      treeStructure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.reverseIterator.map(treeValues).toIterable)) 1 else 0)
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
    val (structure, values) = ArrayTreeFunctions.treeAt(index, treeStructure, treeValues)
    fromSlices(structure, values)
  }

  /** Transforms the tree using provided function, or returns unmodified if result in None.
    * Before modification decomposes the tree into the buffers, and assembles it again after.
    */
  final def transform[T: ClassTag](
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
  final def flatMapLax[T: ClassTag, K: ClassTag](
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

    val trees = TreeBuilder.fromArrays(structureBuffer.toArray, valuesBuffer.toArray)
    if (trees.size == 1) trees.head else Tree.empty
  }

  /** Inserts a value to a sub-tree rooted at the path.
    * @param path sequence of a node's values forming a path to a sub-tree
    * @param value value to insert
    * @param target whole tree
    * @param keepDistinct if true keeps children distinct
    * @return modified tree */
  final def insertLeafAt[T, T1 >: T: ClassTag](
    path: Iterable[T1],
    value: T1,
    target: ArrayTree[T],
    append: Boolean,
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
            val valueSequence = remaining.toVector.+:(item).:+(value)
            val newNode: Tree[T1] = TreeBuilder.linearTreeFromSequence(valueSequence)
            val insertIndex = if (append) bottomIndex(index, target.structure) else index
            insertTree(insertIndex, index, newNode, target)
          case None =>
            insertLeaf(index, value, target, append, keepDistinct)
        }
    }
  }

  /** Inserts a value to a tree at a path using an extractor function.
    * @return modified tree */
  final def insertLeafAt[T, T1 >: T: ClassTag, K](
    path: Iterable[K],
    value: T1,
    target: ArrayTree[T],
    toPathItem: T => K,
    append: Boolean,
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
          Right(insertLeaf(index, value, target, append, keepDistinct))
        }
    }
  }

  /** Checks if children of the tree rooted at the index contains the given value. */
  final def hasChildValue[T](index: Int, value: T, tree: Tree[T]): Boolean = tree match {
    case Tree.empty          => false
    case t: Tree.NodeTree[T] => t.children.exists(_.head == value)
    case t: ArrayTree[T] =>
      ArrayTreeFunctions.firstChildHavingValue(value, index, t.size, t.structure, t.content).isDefined
  }

  final def prepend[T: ClassTag, T1 >: T: ClassTag](value: T1, tree: ArrayTree[T]): ArrayTree[T1] =
    transform[T1](tree) { (structureBuffer, valuesBuffer) =>
      structureBuffer.push(1)
      valuesBuffer.push(value)
      Some(1)
    }.asInstanceOf[ArrayTree[T1]]

  /** Inserts new leaf to a tree at an index.
    * @param parentIndex index of the tree where to insert child
    * @param value value to insert
    * @param target the array tree
    * @param append whether to append (after) or prepend (before) to the existing children
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertLeaf[T: ClassTag](
    parentIndex: Int,
    value: T,
    target: Tree[T],
    append: Boolean,
    keepDistinct: Boolean
  ): Tree[T] =
    if (target.isEmpty) Tree(value).deflated
    else {
      assert(
        parentIndex >= 0 && parentIndex < target.size,
        "Insertion index must be within target's tree range [0,length)."
      )
      if (keepDistinct && hasChildValue(parentIndex, value, target)) target
      else
        transform(target) { (structureBuffer, valuesBuffer) =>
          val insertIndex =
            if (append) bottomIndex(parentIndex, structureBuffer)
            else parentIndex
          ArrayTreeFunctions.insertValue(insertIndex, parentIndex, value, structureBuffer, valuesBuffer).intAsSome
        }
    }

  /** Inserts new leaves to a tree at an index.
    * @param parentIndex index of the tree where to insert children
    * @param values leaves to insert
    * @param target the array tree
    * @param append whether to append (after) or prepend (before) to the existing children
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertLeaves[T: ClassTag](
    parentIndex: Int,
    values: Iterable[T],
    target: Tree[T],
    append: Boolean,
    keepDistinct: Boolean
  ): Tree[T] =
    if (target.isEmpty || values.isEmpty) if (values.size == 1) Tree(values.head) else Tree.empty
    else
      transform(target) { (structureBuffer, valuesBuffer) =>
        val toInsert = if (keepDistinct) {
          val existing = ArrayTreeFunctions.childrenIndexes(parentIndex, structureBuffer).map(valuesBuffer)
          values.filterNot(value => existing.exists(_ == value))
        } else values
        val size = toInsert.size
        val insertIndex =
          if (append) bottomIndex(parentIndex, structureBuffer)
          else parentIndex
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
            val treeSequence = remaining.map(Tree.apply[T1]).toVector.+:(Tree(item)).:+(subtree)
            val newNode: Tree[T1] = TreeBuilder.fromTreeSequence(treeSequence)
            insertTree(index, index, newNode, target)
          case None =>
            if (keepDistinct) insertChildDistinct(index, subtree, target)
            else insertTree(index, index, subtree, target)
        }
    }
  }

  /** Inserts a subtree to a tree at a path using an extractor function.
    * @return either modified tree or an existing */
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
        else if (keepDistinct) Right(insertChildDistinct(index, subtree, target))
        else Right(insertTree(index, index, subtree, target))
    }
  }

  /** Inserts a subtree to a tree at an index.
    * @return modified tree */
  final def insertTree[T: ClassTag](
    index: Int,
    parentIndex: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      transform(target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = source.toSlices
        if (parentIndex >= 0) structureBuffer.increment(parentIndex)
        ArrayTreeFunctions.insertSlice(index, structure, values, structureBuffer, valuesBuffer).intAsSome
      }
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertChildDistinct[T: ClassTag](
    index: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      transform(target) { (structureBuffer, valuesBuffer) =>
        val (structure, values) = source.toSlices
        ArrayTreeFunctions
          .insertBeforeChildrenDistinct(index, structure, values, structureBuffer, valuesBuffer)
          .nonZeroIntAsSome
      }
    }

  /** Inserts multiple children before and after the existing children. */
  final def insertChildren[T: ClassTag](
    target: Tree[T],
    before: Iterable[Tree[T]],
    after: Iterable[Tree[T]],
    keepDistinct: Boolean
  ): Tree[T] =
    if (target.isEmpty) Tree.empty
    else if (before.isEmpty && after.isEmpty) target
    else
      transform(target) { (structureBuffer, valuesBuffer) =>
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

  /** Wraps the child tree recursively with a new trees and siblings. */
  final def buildFromChildAndTreeSplit[T: ClassTag](
    child: Tree[T],
    treeSplit: Iterable[(Iterable[Tree[T]], T, Iterable[Tree[T]])]
  ): Tree[T] =
    transform(child) { (structureBuffer, valuesBuffer) =>
      val delta = treeSplit.foldLeft(0) {
        case (delta, (left, value, right)) =>
          ArrayTreeFunctions.wrapWithValueAndSiblings(
            structureBuffer.top,
            value,
            left.map(_.toSlices),
            right.map(_.toSlices),
            structureBuffer,
            valuesBuffer,
            keepDistinct = false
          ) + delta
      }

      Some(delta)
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertBranch[T: ClassTag](
    index: Int,
    branch: Iterable[T],
    target: Tree[T]
  ): Tree[T] =
    if (branch.isEmpty) target
    else if (target.isEmpty) TreeBuilder.linearTreeFromSequence(branch.toSeq)
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      val iterator: Iterator[T] = branch.iterator
      transform(target) { (structureBuffer, valuesBuffer) =>
        if (iterator.hasNext && valuesBuffer(index) != iterator.next()) None
        else {
          ArrayTreeFunctions.insertBranch(iterator, index, structureBuffer, valuesBuffer, 0).nonZeroIntAsSome
        }
      }
    }

  /** Updates value of the node at the index.
    * @return updated tree */
  final def updateValue[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    replacement: T1,
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0 || replacement == tree.content(index)) tree
    else if (keepDistinct) {
      transform[T1](tree) { (structureBuffer, valuesBuffer) =>
        valuesBuffer(index) = replacement
        ArrayTreeFunctions.ensureChildDistinct(index, structureBuffer, valuesBuffer).intAsSome
      }
    } else {
      new ArrayTree(tree.structure, tree.content.update(index, replacement), tree.width, tree.height)
    }

  /** Updates tree at the index.
    * @return updated tree */
  final def updateTree[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    replacement: Tree[T1],
    tree: Tree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0 || index >= tree.size) tree
    else {
      transform[T1](tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = ArrayTreeFunctions.parentIndex(index, structureBuffer)
        if (replacement.isEmpty) {
          ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer).intAsSome
        } else {
          val hasSameHeadValue = keepDistinct && replacement.head == valuesBuffer(index)
          val delta1 = ArrayTreeFunctions.removeChildren(index, parentIndex, structureBuffer, valuesBuffer)
          val (structure, values) = replacement.toSlices[T1]
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
    }

  /** Updates value of the child node holding the given value. */
  final def updateChildValue[T: ClassTag, T1 >: T: ClassTag](
    value: T1,
    replacement: T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    ArrayTreeFunctions
      .firstChildHavingValue(value, target.size - 1, target.size, target.structure, target.content)
      .filterNot(target.content(_) == replacement)
      .map(updateValue(_, replacement, target, keepDistinct))
      .getOrElse(target)

  /** Updates value of the node selected by the path.
    * @return either modified tree or an existing */
  final def updateValueAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    replacement: T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateValue(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates value of the node selected by the path.
    * @return either modified tree or an existing */
  final def updateValueAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    replacement: T1,
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateValue(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates the child node holding the given value. */
  final def updateChild[T: ClassTag, T1 >: T: ClassTag](
    value: T1,
    replacement: Tree[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    ArrayTreeFunctions
      .firstChildHavingValue(value, target.size - 1, target.size, target.structure, target.content)
      .map(updateTree(_, replacement, target, keepDistinct))
      .getOrElse(target)

  /** Updates a subtree selected by the path.
    * @return either modified tree or an existing */
  final def updateTreeAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    replacement: Tree[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateTree(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Updates a subtree selected by the path.
    * @return either modified tree or an existing */
  final def updateTreeAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    replacement: Tree[T1],
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(indexes =>
        if (indexes.isEmpty) Left(target)
        else Right(updateTree(indexes.last, replacement, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies value of the node at the index.
    * @return modified tree */
  final def modifyValue[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    modify: T => T1,
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else updateValue(index, modify(tree.content(index)), tree, keepDistinct)

  /** Modifies value of the child node holding the given value. */
  final def modifyChildValue[T: ClassTag, T1 >: T: ClassTag](
    value: T1,
    modify: T => T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    ArrayTreeFunctions
      .firstChildHavingValue(value, target.size - 1, target.size, target.structure, target.content)
      .map(modifyValue(_, modify, target, keepDistinct))
      .getOrElse(target)

  /** Modifies value of the node selected by the path.
    * @return either modified tree or an existing */
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
        else Right(modifyValue(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies value of the node selected by the path.
    * @return either modified tree or an existing */
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
        else Right(modifyValue(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies the tree at the index.
    * @return modified tree */
  final def modifyTree[T: ClassTag, T1 >: T: ClassTag](
    index: Int,
    modify: Tree[T] => Tree[T1],
    tree: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (index < 0) tree
    else {
      val newTree = treeAt(index, tree.structure, tree.content)
      updateTree(index, modify(newTree), tree, keepDistinct)
    }

  /** Modifies the child node holding the given value. */
  final def modifyChild[T: ClassTag, T1 >: T: ClassTag](
    value: T1,
    modify: Tree[T] => Tree[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    ArrayTreeFunctions
      .firstChildHavingValue(value, target.size - 1, target.size, target.structure, target.content)
      .map(modifyTree(_, modify, target, keepDistinct))
      .getOrElse(target)

  /** Modifies a subtree selected by the path.
    * @return either modified tree or an existing */
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
        else Right(modifyTree(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

  /** Modifies a subtree selected by the path.
    * @return either modified tree or an existing */
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
        else Right(modifyTree(indexes.last, modify, target, keepDistinct))
      )
      .getOrElse(Left(target))

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
      transform(tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
        ArrayTreeFunctions.removeValue(index, parentIndex, structureBuffer, valuesBuffer, keepDistinct).intAsSome
      }

  /** Removes the node addressed by the last index, inserts children into the parent. private*/
  private def removeValue[T: ClassTag](indexes: IntSlice, target: ArrayTree[T], keepDistinct: Boolean): Tree[T] =
    if (indexes.isEmpty) target
    else if (indexes.last == target.size - 1) {
      if (target.isLeaf) Tree.empty
      else if (target.childrenCount == 1) treeAt(target.size - 2, target.structure, target.content)
      else target
    } else removeValue(indexes.last, indexes.get(indexes.length - 2), target, keepDistinct)

  /** Removes the direct child node holding the value, and inserts children into the parent.
    * @note when removing the top node, the following special rules apply:
    *       - if the tree has a single value, returns empty tree,
    *       - otherwise if the tree has a single child, returns that child,
    *       - otherwise if the tree has more children, returns the tree unmodified.
    * @return modified tree */
  final def removeChildValue[T: ClassTag, T1 >: T: ClassTag](
    value: T1,
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    transform(target) { (structureBuffer, valuesBuffer) =>
      ArrayTreeFunctions
        .firstChildHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, valuesBuffer)
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
  final def removeValueAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    target: ArrayTree[T],
    keepDistinct: Boolean
  ): Tree[T] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(removeValue(_, target, keepDistinct))
      .getOrElse(target)

  /** Removes the node selected by the path using an extractor function, and inserts children into the parent.
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
      .map(removeValue(_, target, keepDistinct))
      .getOrElse(target)

  /** Removes the tree at the index.
    * @return updated tree */
  final def removeTree[T: ClassTag](
    index: Int,
    parentIndexOpt: Option[Int],
    tree: Tree[T]
  ): Tree[T] =
    if (index < 0) tree
    else
      transform(tree) { (structureBuffer, valuesBuffer) =>
        val parentIndex = parentIndexOpt
          .getOrElse(ArrayTreeFunctions.parentIndex(index, structureBuffer))
        ArrayTreeFunctions.removeTree(index, parentIndex, structureBuffer, valuesBuffer).intAsSome
      }

  /** Removes the direct child of the node, holding the value.
    * @return modified tree */
  final def removeChild[T: ClassTag, T1 >: T: ClassTag](
    node: ArrayTree[T],
    value: T1
  ): Tree[T] =
    transform(node) { (structureBuffer, valuesBuffer) =>
      ArrayTreeFunctions
        .lastChildHavingValue(value, structureBuffer.top, structureBuffer.length, structureBuffer, valuesBuffer)
        .map(index => ArrayTreeFunctions.removeTree(index, structureBuffer.top, structureBuffer, valuesBuffer))
    }

  /** Removes the tree selected by the path.
    * @return modified tree */
  final def removeTreeAt[T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[T1],
    target: ArrayTree[T]
  ): Tree[T] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content)
      .map(indexes => removeTree(indexes.last, indexes.get(indexes.length - 2), target))
      .getOrElse(target)

  /** Removes the tree selected by the path using an extractor function.
    * @return modified tree */
  final def removeTreeAt[K, T: ClassTag, T1 >: T: ClassTag](
    path: Iterable[K],
    target: ArrayTree[T],
    toPathItem: T => K
  ): Tree[T] =
    ArrayTreeFunctions
      .followEntirePath(path, target.size - 1, target.structure, target.content, toPathItem)
      .map(indexes => removeTree(indexes.last, indexes.get(indexes.length - 2), target))
      .getOrElse(target)

}
