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

import java.util.NoSuchElementException

import com.github.arturopala.tree.{Tree, TreeBuilder}
import com.github.arturopala.tree.Tree.ArrayTree

import scala.annotation.tailrec
import scala.reflect.ClassTag

/** Collection of operations on the linear, array-based, representation of the tree. */
object ArrayTree {

  @`inline` final def identity[T, T1 >: T]: T => T1 = x => x

  /** List indexes of the children values of the parent node, if any.
    * @note tree structure as returned by [[Tree.toArrays]] */
  final def childrenIndexes(parentIndex: Int, treeStructure: Int => Int): List[Int] =
    if (parentIndex < 0) Nil
    else {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren == 0) Nil
      else {
        var result: List[Int] = List(parentIndex - 1)
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          result = i :: result
          n = n - 1
        }
        result.reverse
      }
    }

  /** Appends children indexes to the buffer, starting from the given position.
    * @return number of children appended
    */
  final def writeChildrenIndexes(
    parentIndex: Int,
    treeStructure: Int => Int,
    buffer: IntBuffer,
    position: Int
  ): Int =
    if (parentIndex < 0) 0
    else {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren == 0) 0
      else {
        var pos = position + numberOfChildren - 1
        buffer(pos) = parentIndex - 1
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          pos = pos - 1
          buffer(pos) = i
          n = n - 1
        }
        numberOfChildren - n
      }
    }

  /** Checks if children of the node contains the given value.
    * @param pred child value predicate
    * @return index of a child fulfilling predicate
    */
  final def findChildIndex[T](
    parentIndex: Int,
    pred: T => Boolean,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    var result: Option[Int] = None
    if (parentIndex >= 0) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        var value = treeValues(parentIndex - 1)
        if (pred(value)) result = Some(parentIndex - 1)
        else {
          var n = numberOfChildren - 1
          var i = parentIndex - 1
          while (n > 0 && i >= 0) {
            var a = treeStructure(i)
            while (a > 0) {
              i = i - 1
              a = a - 1 + treeStructure(i)
            }
            i = i - 1
            value = treeValues(i)
            if (pred(value)) {
              result = Some(i)
              n = -1 // escape
            } else {
              n = n - 1
            }
          }
        }
      }
    }
    result
  }

  /** Looks for Some index of the child node holding the given value, or None.
    * @return the rightmost index if many */
  final def childrenLeftmostIndexFor[T](
    value: T,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    var result: Option[Int] = None
    if (parentIndex > 0) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = Some(parentIndex - 1)
        } else {
          var n = numberOfChildren - 1
          var i = parentIndex - 1
          while (n > 0 && i >= 0) {
            var a = treeStructure(i)
            while (a > 0) {
              i = i - 1
              a = a - 1 + treeStructure(i)
            }
            i = i - 1
            if (treeValues(i) == value) {
              result = Some(i)
              n = -1
            }
            n = n - 1
          }
        }
      }
    }
    result
  }

  /** Looks for Some index of the child node holding the given value, or None.
    * @return the leftmost index if many */
  final def childrenRightmostIndexFor[T](
    value: T,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Option[Int] = {
    var result: Option[Int] = None
    if (parentIndex > 0) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = Some(parentIndex - 1)
        }
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          if (treeValues(i) == value) {
            result = Some(i)
          }
          n = n - 1
        }

      }
    }
    result
  }

  /** List indexes of the children values of the parent node holding the given value, if any.
    * @note tree structure as returned by [[com.github.arturopala.tree.Tree.toArrays]] */
  final def childrenIndexListFor[T](
    value: T,
    parentIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): List[Int] = {
    var result: List[Int] = Nil
    if (parentIndex > 0) {
      val numberOfChildren = treeStructure(parentIndex)
      if (numberOfChildren > 0) {
        if (treeValues(parentIndex - 1) == value) {
          result = (parentIndex - 1) :: result
        }
        var n = numberOfChildren - 1
        var i = parentIndex - 1
        while (n > 0 && i >= 0) {
          var a = treeStructure(i)
          while (a > 0) {
            i = i - 1
            a = a - 1 + treeStructure(i)
          }
          i = i - 1
          if (treeValues(i) == value) {
            result = i :: result
          }
          n = n - 1
        }
      }
    }
    result
  }

  /** Finds an index of the parent node of the given node.
    * @note If index > size - 1 then returns -1.  */
  final def parentIndex(index: Int, size: Int, treeStructure: Int => Int): Int = {
    var v = 1
    var i = index
    var c = true
    while (c && i < size - 1) {
      i = i + 1
      v = v - treeStructure(i)
      c = v > 0
      v = v + 1
    }
    if (c) -1 else i
  }

  /** Calculates leftmost index of the tree rooted at index. */
  final def bottomIndex(index: Int, treeStructure: Int => Int): Int =
    index - treeSize(index, treeStructure) + 1

  /** Calculates the size of the tree starting at the given index. */
  final def treeSize(index: Int, treeStructure: Int => Int): Int = {
    var i = index
    var a = treeStructure(i)

    while (a > 0 && i > 0) {
      i = i - 1
      a = a - 1 + treeStructure(i)
    }

    val size = index - i + 1

    if (a == 0) size
    else throw new IllegalArgumentException(s"Incomplete tree, size is $size, but missing at least $a node(s)")
  }

  /** Iterates over tree's node indexes, top-down, depth first. */
  final def nodeIndexIterator(startIndex: Int, treeStructure: Int => Int): Iterator[Int] = new Iterator[Int] {

    var i: Int = startIndex
    var hasNext: Boolean = i >= 0
    var a: Int = if (hasNext) treeStructure(i) else 0

    override def next(): Int =
      if (hasNext) {
        val index = i
        hasNext = a > 0 && i > 0
        if (hasNext) {
          i = i - 1
          a = a - 1 + treeStructure(i)
        }
        index
      } else throw new NoSuchElementException
  }

  /** Iterates over tree's branches as index lists, depth first. */
  final def nodeIndexIteratorWithLimit(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[Int] =
    new Iterator[Int] {

      var hasNext: Boolean = false
      var i: Int = startIndex

      val counters = new IntBuffer()
      val indexes = new IntBuffer()

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      override def next(): Int =
        if (hasNext) {
          val result = i
          seekNext(true)
          result
        } else throw new NoSuchElementException

      def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false } else {
          i = indexes.peek
          if (i < 0) { hasNext = false } else {
            hasNext = true
            val c = treeStructure(i)
            if (c == 0 || counters.length >= maxDepth - 1) {
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
            }
          }
        }
    }

  /** Iterates over tree's branches as index lists, depth first. */
  final def branchesIndexListIterator(
    startIndex: Int,
    treeStructure: Int => Int,
    maxDepth: Int = Int.MaxValue
  ): Iterator[IntSlice] =
    new Iterator[IntSlice] {

      var hasNext: Boolean = false
      var array: IntSlice = IntSlice.empty

      val counters = new IntBuffer()
      val indexes = new IntBuffer()

      if (maxDepth > 0) {
        indexes.push(startIndex)
        seekNext(false)
      }

      override def next(): IntSlice =
        if (hasNext) {
          val result = array
          seekNext(true)
          result
        } else throw new NoSuchElementException

      @tailrec
      def seekNext(check: Boolean): Unit =
        if (check && counters.isEmpty) { hasNext = false } else {
          val i = indexes.peek
          if (i < 0) { hasNext = false } else {
            val c = treeStructure(i)
            if (c == 0 || counters.length >= maxDepth - 1) {
              array = BranchIteratorUtils.readBranch(counters, indexes).push(i).toSlice
              hasNext = true
              BranchIteratorUtils.retract(counters, indexes)
            } else {
              counters.push(c)
              writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
              seekNext(false)
            }
          }
        }
    }

  private object BranchIteratorUtils {

    def readBranch(counters: IntBuffer, indexes: IntBuffer): IntBuffer = {
      val branchIndexes = new IntBuffer()
      var i = 0
      var ci = 0
      while (ci < counters.length) { // for all counters
        branchIndexes.push(indexes(i)) // read and push index at position
        val c = counters(ci) // read next child ordinal
        i = i + c // advance position to the next child
        ci = ci + 1 // increment counter ordinal
      }
      branchIndexes
    }

    def sizeBranch(counters: IntBuffer): Int = {
      var l = 0
      var i = 0
      var ci = 0
      while (ci < counters.length) {
        l = l + 1
        val c = counters(ci)
        i = i + c
        ci = ci + 1
      }
      l
    }

    def retract(counters: IntBuffer, indexes: IntBuffer): Unit =
      if (counters.length >= 1) {
        var value = counters.pop - 1
        indexes.pop
        while (value == 0 && counters.length >= 1) {
          value = counters.pop - 1
          indexes.pop
        }
        if (value != 0) counters.push(value)
      }

    def retract(counters: IntBuffer): Unit =
      if (counters.length >= 1) {
        var value = counters.pop - 1
        while (value == 0 && counters.length >= 1) {
          value = counters.pop - 1
        }
        if (value != 0) counters.push(value)
      }
  }

  /** Iterates over filtered values, top-down, depth-first. */
  final def valueIterator[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: T => Boolean,
    maxDepth: Int
  ): Iterator[T] =
    new MapFilterIterator[Int, T](
      nodeIndexIteratorWithLimit(startIndex, treeStructure, maxDepth),
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
      branchesIndexListIterator(startIndex, treeStructure, maxDepth),
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
      nodeIndexIterator(startIndex, treeStructure),
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
      nodeIndexIteratorWithLimit(startIndex, treeStructure, maxDepth),
      treeAt(_, treeStructure, treeValues),
      pred
    )
  }

  /** Returns tree rooted at the given index. */
  final def treeAt[T: ClassTag](index: Int, treeStructure: IntSlice, treeValues: Slice[T]): Tree[T] = {
    val size = treeSize(index, treeStructure)
    if (size == 0) Tree.empty
    else {
      val structure = treeStructure.slice(index - size + 1, index + 1)
      val values = treeValues.slice(index - size + 1, index + 1)
      new ArrayTree[T](structure, values, structure.count(_ == 0), calculateHeight(structure.length - 1, structure))
    }
  }

  @`inline` final val readBranchSlice: (IntBuffer, IntBuffer, Int) => IntSlice =
    (counters, indexes, i) => BranchIteratorUtils.readBranch(counters, indexes).push(i).toSlice

  /** Fold tree's branches as index lists. */
  final def foldLeftBranchesIndexLists[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, IntSlice, Int) => A,
    maxDepth: Int = Int.MaxValue
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchSlice, fold, maxDepth)

  @`inline` final val readBranchLength: (IntBuffer, IntBuffer, Int) => Int =
    (counters, _, _) => BranchIteratorUtils.sizeBranch(counters) + 1

  /** Fold tree's branch lengths. */
  final def foldLeftBranchesLengths[A](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    fold: (A, Int, Int) => A
  ): A =
    foldLeftBranches(startIndex, treeStructure, initialValue, readBranchLength, fold)

  /** Fold tree branches using provided read and fold functions.
    * @param read function to read counters and indexes buffer into result
    * @param fold function to fold value, result and branch number into accumulated value
    * @param maxDepth maximum depth of the tree to travel
    * */
  final def foldLeftBranches[A, R](
    startIndex: Int,
    treeStructure: Int => Int,
    initialValue: A,
    read: (IntBuffer, IntBuffer, Int) => R,
    fold: (A, R, Int) => A,
    maxDepth: Int = Int.MaxValue
  ): A = {

    var result = initialValue

    if (startIndex >= 0 && maxDepth > 0) {

      val counters = new IntBuffer()
      val indexes = new IntBuffer()

      var n = 0

      indexes.push(startIndex)

      while ({
        val i = indexes.peek
        if (i < 0) {
          val value = read(counters, indexes, 0)
          result = fold(result, value, n)
          BranchIteratorUtils.retract(counters, indexes)
          n = n + 1
        } else {
          val c = treeStructure(i)
          if (c == 0 || counters.length >= maxDepth - 1) {
            val newValue = read(counters, indexes, i)
            result = fold(result, newValue, n)
            BranchIteratorUtils.retract(counters, indexes)
            n = n + 1
          } else {
            counters.push(c)
            writeChildrenIndexes(i, treeStructure, indexes, indexes.length)
          }
        }
        counters.nonEmpty
      }) ()
    }

    result
  }

  /** Calculates the height of the tree, i.e. the length of the longest branch. */
  @`inline` final def calculateHeight(treeStructure: IntSlice): Int =
    calculateHeight(treeStructure.length - 1, treeStructure)

  /** Calculates the height of the tree, i.e. the length of the longest branch. */
  @`inline` final def calculateWidth(treeStructure: IntSlice): Int =
    treeStructure.count(_ == 0)

  /** Calculates the height of the tree at index, i.e. the length of the longest branch. */
  @`inline` final def calculateHeight(startIndex: Int, treeStructure: Int => Int): Int =
    foldLeftBranchesLengths(startIndex, treeStructure, 0, (a: Int, v: Int, _: Int) => Math.max(a, v))

  /** Count branches starting at index fulfilling the predicate. */
  final def countBranches[T: ClassTag](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    pred: List[T] => Boolean
  ): Int =
    foldLeftBranchesIndexLists(
      startIndex,
      treeStructure,
      0,
      (a: Int, branch: IntSlice, _: Int) => a + (if (pred(branch.reverseIterator.map(treeValues).toList)) 1 else 0)
    )

  /** Renders tree as a string iterating over the branches.*/
  final def mkStringUsingBranches[T](
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    show: T => String,
    valueSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String,
    maxDepth: Int
  ): StringBuilder = {

    def renderBranch(builder: StringBuilder, branch: IntSlice, branchIndex: Int): StringBuilder = {
      if (branchIndex > 0) builder.append(branchSeparator)
      builder.append(branchStart)
      var i = 0
      val iter = branch.iterator
      while (iter.hasNext) {
        if (i > 0) builder.append(valueSeparator)
        builder.append(show(treeValues(iter.next())))
        i = i + 1
      }
      builder.append(branchEnd)
    }

    foldLeftBranchesIndexLists(startIndex, treeStructure, new StringBuilder(), renderBranch, maxDepth)
  }

  /** Follows the given path of values into the tree.
    * @return a tuple consisting of:
    *         - an array of travelled indexes,
    *         - optionally non matching path segment,
    *         - remaining path iterator,
    *         - flag set to true if path matched an entire branch.
    *
    * @note 1. Assumes distinct values of children in each node.
    *       2. To check if the path exists within the tree it is sufficient to validate
    *          that returned non matching segment is empty.
    *       3. Generally, if the tree contains the path then the returned tuple
    *          will be (non empty indexes, None, empty iterator, true or false),
    *          but if the tree does not contain the path then the returned tuple
    *          will be (indexes (maybe empty), Some(segment), some iterator (maybe empty), true or false).
    */
  @`inline` final def followPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): (Array[Int], Option[T1], Iterator[T1], Boolean) =
    followPath(path, startIndex, treeStructure, treeValues, identity[T, T1])

  /** Follows the given path into the tree using extractor function.
    * @param toPathItem function to extract path item from the tree's node value.
    * @return a tuple consisting of:
    *         - an array of travelled indexes,
    *         - optionally non matching path segment,
    *         - remaining path iterator,
    *         - flag set to true if path matched an entire branch.
    *
    * @note 1. Assumes distinct values of children in each node.
    *       2. To check if the path exists within the tree it is sufficient to validate
    *          that returned non matching segment is empty.
    *       3. Generally, if the tree contains the path then the returned tuple
    *          will be (non empty indexes, None, empty iterator, true or false),
    *          but if the tree does not contain the path then the returned tuple
    *          will be (indexes (maybe empty), Some(segment), some iterator (maybe empty), true or false).
    */
  final def followPath[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): (Array[Int], Option[K], Iterator[K], Boolean) = {

    val indexes = new IntBuffer() // travelled indexes
    val children = new IntBuffer().push(startIndex) // children indexes to consider
    val pathIterator = path.iterator

    var pathSegment: Option[K] = None

    while (children.nonEmpty && pathIterator.hasNext) {
      pathSegment = Some(pathIterator.next())
      if (children.nonEmpty) {
        val c = children.reset + 1
        var n = 0
        while (n >= 0 && n < c) {
          val ci = children(n) // child index
          if (ci >= 0 && pathSegment.contains(toPathItem(treeValues(ci)))) {
            indexes.push(ci)
            writeChildrenIndexes(ci, treeStructure, children, 0)
            pathSegment = None
            n = -1 // force inner loop exit
          } else {
            n = n + 1
          }
        }
      }
    }

    if (pathSegment.isEmpty && pathIterator.hasNext) {
      pathSegment = Some(pathIterator.next())
    }

    (indexes.toArray, pathSegment, pathIterator, pathSegment.isEmpty && children.isEmpty)
  }

  /** Checks if the tree contains given branch. */
  @`inline` final def containsBranch[T, T1 >: T](
    branch: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, fullMatch) = followPath(branch, startIndex, treeStructure, treeValues)
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
    val (_, unmatched, _, fullMatch) = followPath(branch, startIndex, treeStructure, treeValues, toPathItem)
    fullMatch && unmatched.isEmpty
  }

  /** Checks if the tree contains given path (as a branch prefix). */
  @`inline` final def containsPath[T, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T
  ): Boolean = {
    val (_, unmatched, _, _) = followPath(path, startIndex, treeStructure, treeValues)
    unmatched.isEmpty
  }

  /** Checks if the tree contains given path (as a branch prefix). */
  @`inline` final def containsPath[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Boolean = {
    val (_, unmatched, _, _) = followPath(path, startIndex, treeStructure, treeValues, toPathItem)
    unmatched.isEmpty
  }

  /** Selects node's value accessible by path using item extractor function. */
  @`inline` final def selectValue[T, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: Int => Int,
    treeValues: Int => T,
    toPathItem: T => K
  ): Option[T] =
    followPath(path, startIndex, treeStructure, treeValues, toPathItem) match {
      case (indexes, None, _, _) if indexes.nonEmpty => Some(treeValues(indexes.last))
      case _                                         => None
    }

  /** Selects tree accessible by path. */
  @`inline` final def selectTree[T: ClassTag, T1 >: T](
    path: Iterable[T1],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T]
  ): Option[Tree[T]] =
    followPath(path, startIndex, treeStructure, treeValues) match {
      case (indexes, None, _, _) if indexes.nonEmpty =>
        val tree = treeAt[T](indexes.last, treeStructure, treeValues)
        Some(tree)

      case _ => None
    }

  /** Selects tree accessible by path using item extractor function. */
  @`inline` final def selectTree[T: ClassTag, K](
    path: Iterable[K],
    startIndex: Int,
    treeStructure: IntSlice,
    treeValues: Slice[T],
    f: T => K
  ): Option[Tree[T]] =
    followPath(path, startIndex, treeStructure, treeValues, f) match {
      case (indexes, None, _, _) if indexes.nonEmpty =>
        val tree = treeAt[T](indexes.last, treeStructure, treeValues)
        Some(tree)

      case _ => None
    }

  /** FlatMaps the tree without checking for duplicates. */
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

    def parent: Int = parentIndex(index, treeStructure.length, treeStructure) + offset

    while (index < treeStructure.length) {
      val tree = f(treeValues(index))
      val delta = insertTree(tree, index + offset, parent, structureBuffer, valuesBuffer)
      offset = offset + delta
      index = index + 1
    }

    val trees = TreeBuilder.fromArrays(structureBuffer.toArray, valuesBuffer.toArray)
    if (trees.size == 1) trees.head else Tree.empty
  }

  /** FlatMaps the tree enforcing distinct children. */
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
      val parent = {
        val p = parentIndex(index, treeStructure.length, treeStructure)
        if (p < 0) p else p + offset
      }

      val tree = f(treeValues(index))
      val delta = insertTreeDistinct(tree, index + offset, parent, structureBuffer, valuesBuffer)

      offset = offset + delta
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
    val content = target.content
    val (indexes, unmatched, remaining, _) = followPath(path, target.size - 1, structure, content)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val valueList = (item :: remaining.toList) :+ value
            val newNode: Tree[T1] = TreeBuilder.fromValueList(valueList)
            insertSubtree(index, newNode, target)
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
    val content = target.content
    val (indexes, unmatched, _, _) = followPath(path, target.size - 1, structure, content, toPathItem)
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
    case t: ArrayTree[T]     => findChildIndex(index, (v: T) => v == value, t.structure, t.content).isDefined
  }

  /** Inserts a value to a tree at an index.
    * @param index index of the root of a target sub-tree
    * @param value value to insert
    * @param target whole tree
    * @param keepDistinct if true keeps children distinct
    * @return modified tree
    */
  final def insertValue[T, T1 >: T: ClassTag](
    index: Int,
    value: T1,
    target: Tree[T],
    keepDistinct: Boolean
  ): Tree[T1] =
    if (target.isEmpty) Tree(value).deflated
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")

      if (keepDistinct && hasChildValue(index, value, target)) target
      else {

        val (structureBuffer, valuesBuffer) = target.toBuffers[T1]
        val leaf = structureBuffer(index) == 0

        structureBuffer.increment(index)
        structureBuffer.shiftRight(index, 1)
        structureBuffer.update(index, 0)
        valuesBuffer.shiftRight(index, 1)
        valuesBuffer.update(index, value)

        val newTreeStructure = structureBuffer.toSlice
        val newTreeValues = valuesBuffer.toSlice

        val width = 1 + target.width - (if (leaf) 1 else 0)

        new ArrayTree[T1](newTreeStructure, newTreeValues, width, calculateHeight(newTreeStructure))
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
    val (indexes, unmatched, remaining, _) = followPath(path, target.size - 1, target.structure, target.content)
    indexes.lastOption match {
      case None => target
      case Some(index) =>
        unmatched match {
          case Some(item) =>
            val treeList = (Tree(item) :: remaining.toList.map(Tree.apply[T1])) :+ subtree
            val newNode: Tree[T1] = TreeBuilder.fromTreeList(treeList)
            insertSubtree(index, newNode, target)
          case None =>
            if (keepDistinct) insertSubtreeDistinct(index, subtree, target)
            else insertSubtree(index, subtree, target)
        }
    }
  }

  /** Inserts a subtree to a tree at a path using an extractor function.
    * @return modified tree */
  final def insertTreeAt[T, T1 >: T: ClassTag, K](
    path: Iterable[K],
    subtree: Tree[T1],
    target: ArrayTree[T],
    toPathItem: T => K,
    keepDistinct: Boolean
  ): Either[Tree[T], Tree[T1]] = {
    val (indexes, unmatched, _, _) = followPath(path, target.size - 1, target.structure, target.content, toPathItem)
    indexes.lastOption match {
      case None => Left(target)
      case Some(index) =>
        if (unmatched.isDefined) Left(target)
        else if (keepDistinct) Right(insertSubtreeDistinct(index, subtree, target))
        else Right(insertSubtree(index, subtree, target))
    }
  }

  /** Inserts a subtree to a tree at an index.
    * @return modified tree */
  final def insertSubtree[T: ClassTag](
    index: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")

      val (structureBuffer, valuesBuffer) = target.toBuffers
      val leaf = structureBuffer(index) == 0

      val (treeStructure, treeValues) = source.toSlices

      structureBuffer.increment(index)
      insertAt(index, treeStructure, treeValues, structureBuffer, valuesBuffer)

      val newTreeStructure = structureBuffer.toSlice
      val newTreeValues = valuesBuffer.toSlice

      val width = source.width + target.width - (if (leaf) 1 else 0)

      new ArrayTree[T](newTreeStructure, newTreeValues, width, calculateHeight(newTreeStructure))
    }

  /** Inserts a subtree to a tree at an index while keeping children values distinct.
    * @return modified tree */
  final def insertSubtreeDistinct[T: ClassTag](
    index: Int,
    source: Tree[T],
    target: Tree[T]
  ): Tree[T] =
    if (source.isEmpty) target
    else if (target.isEmpty) source
    else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")

      val (structureBuffer, valuesBuffer) = target.toBuffers

      val delta = insertLeftSubtreeListDistinct(List((index, source)), structureBuffer, valuesBuffer, 0)

      if (delta == 0) {
        target // skip creating new instance if nothing changed
      } else {
        TreeBuilder.fromBuffers(structureBuffer, valuesBuffer)
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
    else if (target.isEmpty) {
      val values = Slice.of(branch.toArray.reverse)
      val structure = {
        val array = Array.fill(values.length)(1)
        array(0) = 0
        IntSlice.of(array)
      }
      new ArrayTree[T](
        structure,
        values,
        1,
        values.length
      )
    } else {
      assert(index >= 0 && index < target.size, "Insertion index must be within target's tree range [0,length).")
      val iterator: Iterator[T] = branch.iterator
      if (iterator.hasNext) {
        val value = iterator.next()
        val (structureBuffer, valuesBuffer) = target.toBuffers
        if (valuesBuffer(index) == value) {
          val delta = insertBranch(iterator, index, structureBuffer, valuesBuffer, 0)

          if (delta == 0) target
          else {
            TreeBuilder.fromBuffers(structureBuffer, valuesBuffer)
          }
        } else target
      } else target
    }

  /** Removes value at index and joins subtrees to the parent node.
    * Does not check for duplicate children values in parent. */
  final def removeNode[T: ClassTag](
    index: Int,
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Unit = {
    structureBuffer.modify(parentIndex, _ + structureBuffer(index) - 1)
    structureBuffer.shiftLeft(index + 1, 1)
    valuesBuffer.shiftLeft(index + 1, 1)
  }

  /** Inserts tree to the buffers at the given index without checking for duplicates.
    * If the tree is empty then removes the value at index, merges subtrees,
    * and updates subtree count at parent index.
    * @return buffer length change */
  final def insertTree[T: ClassTag](
    tree: Tree[T],
    index: Int,
    parentIndex: => Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (tree.size == 0) {
      removeNode(index, parentIndex, structureBuffer, valuesBuffer)
      -1
    } else {

      val offset1 = if (index < 0) {
        structureBuffer.shiftRight(0, -index)
        structureBuffer.modifyRange(0, -index, _ => 0)
        valuesBuffer.shiftRight(0, -index)
        -index
      } else 0

      val offset2 = if (tree.size == 1) {
        valuesBuffer(index + offset1) = tree.valueOption.get
        0
      } else {
        val (structure, values) = tree.toSlices
        val s = treeSize(index + offset1, structureBuffer) - 1
        structureBuffer.modify(index + offset1, _ + structure.last)
        valuesBuffer.update(index + offset1, values.last)
        insertAt(index + offset1 - s, structure.init, values.init, structureBuffer, valuesBuffer)
      }
      offset1 + offset2
    }

  /** Inserts slice's content to the buffers at an index.
    * Shifts existing buffer content right, starting from an index.
    * @return buffer length change */
  final def insertAt[T: ClassTag](
    index: Int,
    structure: IntSlice,
    values: Slice[T],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int = {
    structureBuffer.shiftRight(index, structure.length)
    structureBuffer.copyFrom(index, structure)
    valuesBuffer.insertArray(index, 0, values.length, values.toArray)
    structure.length
  }

  /** Inserts tree to the buffers at the given index checking for duplicated children.
    * Does nothing when an empty tree.
    * @return buffer length change */
  final def insertTreeDistinctUnsafe[T: ClassTag](
    tree: Tree[T],
    index: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    tree.valueOption match {
      case None => 0
      case Some(value) =>
        valuesBuffer(index) = value
        tree.children.foldLeft(0) { (offset, subtree) =>
          val bufferIndex = index + offset
          offset + (childrenLeftmostIndexFor(subtree.valueOption.get, bufferIndex, structureBuffer, valuesBuffer) match {
            case None =>
              val (structure, values) = subtree.toSlices
              val s = treeSize(bufferIndex, structureBuffer) - 1
              structureBuffer.increment(bufferIndex)
              insertAt(bufferIndex - s, structure, values, structureBuffer, valuesBuffer)

            case Some(i) => insertTreeDistinctUnsafe(subtree, i, structureBuffer, valuesBuffer)
          })
        }
    }

  /** Inserts tree to the buffers at the given index with checking for duplicated children.
    * Does nothing when an empty tree.
    * @return buffer length change */
  final def insertTreeDistinct[T: ClassTag](
    tree: Tree[T],
    index: Int,
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T]
  ): Int =
    if (tree.size == 0) {
      removeNode(index, parentIndex, structureBuffer, valuesBuffer)
      -1
    } else {

      val (i, sibling) =
        childrenRightmostIndexFor(tree.valueOption.get, parentIndex, structureBuffer, valuesBuffer)
          .map((_, true))
          .getOrElse((index, false))

      valuesBuffer(i) = tree.valueOption.get

      val delta =
        if (tree.size == 1) 0
        else
          insertRightSubtreeListDistinct(tree.children.filter(_.size > 0).map((i, _)), structureBuffer, valuesBuffer, 0)

      if (sibling && delta == 0 && parentIndex >= 0) {
        removeNode(index, parentIndex, structureBuffer, valuesBuffer)
        delta - 1
      } else delta
    }

  /** Inserts subtrees to the buffers with checking for duplicated children.
    * Does nothing when an empty tree.
    * @param queue list of (parentIndex, tree to insert)
    * @return buffer length change */
  @tailrec
  final def insertRightSubtreeListDistinct[T: ClassTag](
    queue: List[(Int, Tree[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    queue match {
      case Nil => offset
      case (parentIndex, tree) :: tail =>
        if (tree.size == 0) insertRightSubtreeListDistinct(tail, structureBuffer, valuesBuffer, offset)
        else {
          val (i, insert) =
            childrenRightmostIndexFor(tree.valueOption.get, parentIndex, structureBuffer, valuesBuffer)
              .map((_, false))
              .getOrElse((parentIndex - treeSize(parentIndex, structureBuffer) + 1, true))

          val (delta1, updatedTail) = if (insert) {
            structureBuffer.shiftRight(i, 1)
            structureBuffer.update(i, 0)
            structureBuffer.increment(parentIndex + 1)
            valuesBuffer.shiftRight(i, 1)
            valuesBuffer.update(i, tree.valueOption.get)
            (1, tail.map(shiftFrom(i, 1)))
          } else (0, tail)

          val (delta2, updatedQueue) = if (tree.size > 1) {
            if (structureBuffer(i) == 0) { // skip checking duplicates and insert remaining tree at once
              val (structure, values) = tree.toSlices
              val s = treeSize(i, structureBuffer)
              structureBuffer.modify(i, _ + structure.last)
              val d = insertAt(i - s + 1, structure.init, values.init, structureBuffer, valuesBuffer)
              (d, updatedTail.map(shiftFrom(i - d + 1, d)))
            } else
              (0, tree.children.reverse.filter(_.size > 0).map((i, _)) ::: updatedTail)
          } else
            (0, updatedTail)

          insertRightSubtreeListDistinct(updatedQueue, structureBuffer, valuesBuffer, offset + delta1 + delta2)
        }
    }

  /** Inserts subtrees as the leftmost child to the buffers with checking for duplicates.
    * Does nothing when an empty tree.
    * @param queue list of (parentIndex, tree to insert)
    * @return buffer length change */
  @tailrec
  final def insertLeftSubtreeListDistinct[T: ClassTag](
    queue: List[(Int, Tree[T])],
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    queue match {
      case Nil => offset
      case (parentIndex, tree) :: tail =>
        if (tree.size == 0) insertLeftSubtreeListDistinct(tail, structureBuffer, valuesBuffer, offset)
        else {
          val (i, insert) =
            childrenLeftmostIndexFor(tree.valueOption.get, parentIndex, structureBuffer, valuesBuffer)
              .map((_, false))
              .getOrElse((parentIndex, true))

          val (delta1, updatedTail) = if (insert) {
            structureBuffer.shiftRight(i, 1)
            structureBuffer.update(i, 0)
            structureBuffer.increment(parentIndex + 1)
            valuesBuffer.shiftRight(i, 1)
            valuesBuffer.update(i, tree.valueOption.get)
            (1, tail.map(shiftFrom(i, 1)))
          } else (0, tail)

          val (delta2, updatedQueue) = if (tree.size > 1) {
            if (structureBuffer(i) == 0) { // skip checking duplicates and insert remaining tree at once
              val (structure, values) = tree.toSlices
              val s = treeSize(i, structureBuffer)
              structureBuffer.modify(i, _ + structure.last)
              val d = insertAt(i - s + 1, structure.init, values.init, structureBuffer, valuesBuffer)
              (d, updatedTail.map(shiftFrom(i - d + 1, d)))
            } else
              (0, tree.children.reverse.filter(_.size > 0).map((i, _)) ::: updatedTail)
          } else
            (0, updatedTail)

          insertLeftSubtreeListDistinct(updatedQueue, structureBuffer, valuesBuffer, offset + delta1 + delta2)
        }
    }

  /** Inserts branch into the buffers with checking for duplicates.
    * @param branchIterator list of values
    * @return buffer length change */
  @tailrec
  final def insertBranch[T: ClassTag](
    branchIterator: Iterator[T],
    parentIndex: Int,
    structureBuffer: IntBuffer,
    valuesBuffer: Buffer[T],
    offset: Int
  ): Int =
    if (branchIterator.hasNext) {
      val value = branchIterator.next()
      childrenRightmostIndexFor(value, parentIndex, structureBuffer, valuesBuffer) match {
        case Some(index) =>
          insertBranch(branchIterator, index, structureBuffer, valuesBuffer, offset)

        case None =>
          val values = Slice.of(branchIterator.toArray.reverse :+ value)
          val structure = {
            val array = Array.fill(values.length)(1)
            array(0) = 0
            IntSlice.of(array)
          }
          if (parentIndex >= 0) {
            structureBuffer.increment(parentIndex)
            insertAt(parentIndex, structure, values, structureBuffer, valuesBuffer)
          } else {
            insertAt(0, structure, values, structureBuffer, valuesBuffer)
          }

      }
    } else offset

  /** Adds d to i if i >= b, returns i otherwise. */
  final def shiftIfGreaterOrEqualTo(i: Int, b: Int, d: Int): Int = if (i >= b) i + d else i

  /** Transforms (index,tree) by adding an offset d to the index if greater or equal to i. */
  final def shiftFrom[T](i: Int, d: Int): ((Int, Tree[T])) => (Int, Tree[T]) = {
    case (p, t) => (shiftIfGreaterOrEqualTo(p, i, d), t)
  }

}
