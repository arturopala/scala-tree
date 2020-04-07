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

import scala.collection.AbstractIterable
import scala.reflect.ClassTag

/** Lazy, immutable slice of an underlying array.
  * @tparam T type of the array's items */
abstract class ArraySlice[T] private[util] (fromIndex: Int, toIndex: Int) extends Slice[T] {

  /** Type of the underlying array items. */
  type A

  /** Underlying array. */
  val array: Array[A]

  /** Value mapping function. */
  val mapF: A => T

  /** Sliced range length. */
  final val length: Int = toIndex - fromIndex

  /** Returns value at the given index withing the range. */
  final def apply(index: Int): T = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `apply` index in the interval [0,$length), but was $index.")
    mapF(array.apply(fromIndex + index))
  }

  /** Creates a copy of the slice with modified value. */
  final def update(index: Int, value: T)(implicit tag: ClassTag[T]): Slice[T] = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `update` index in the interval [0,$length), but was $index.")
    val modified: Array[T] = toArray
    modified.update(index, value)
    Slice.of[T, T](0, length, modified, identity)
  }

  /** Lazily composes mapping function and returns new Slice.
    * Does not modify nor copy underlying array. */
  final def map[K](f: T => K): Slice[K] = Slice.of[K, A](fromIndex, toIndex, array, mapF.andThen(f))

  /** Counts values fulfilling the predicate. */
  final def count(pred: T => Boolean): Int = {
    var a = 0
    var i = fromIndex
    while (i < toIndex) {
      if (pred(mapF(array(i)))) a = a + 1
      i = i + 1
    }
    a
  }

  /** Returns true if Slice has values, otherwise false. */
  final def isEmpty: Boolean = length <= 0

  /** Returns first value in the Slice. */
  final def head: T =
    if (length > 0) mapF(array(fromIndex))
    else throw new NoSuchElementException

  /** Returns the last value in the Slice. */
  final def last: T =
    if (length > 0) mapF(array(toIndex - 1))
    else throw new NoSuchElementException

  /** Returns first value in the Slice. */
  final def headOption: Option[T] =
    if (length > 0) Some(mapF(array(fromIndex)))
    else None

  /** Returns the last value in the Slice. */
  final def lastOption: Option[T] =
    if (length > 0) Some(mapF(array(toIndex - 1)))
    else None

  /** Returns the Slice without first value. */
  final def tail: Slice[T] = drop(1)

  /** Returns the Slice without last value. */
  final def init: Slice[T] = dropRight(1)

  /** Lazily narrows Slice to provided range. */
  final def slice(from: Int, to: Int): Slice[T] = {
    val t = fit(0, to, length)
    val f = fit(0, from, t)
    if (f == 0 && t == length) this
    else
      Slice.of[T, A](fromIndex + f, fromIndex + t, array, mapF)
  }

  private def fit(lower: Int, value: Int, upper: Int): Int =
    Math.min(Math.max(lower, value), upper)

  /** Lazily narrows Slice to first N items. */
  final def take(n: Int): Slice[T] =
    Slice.of[T, A](fromIndex, Math.min(fromIndex + Math.max(0, n), toIndex), array, mapF)

  /** Lazily narrows Slice to last N items. */
  final def takeRight(n: Int): Slice[T] =
    Slice.of[T, A](Math.max(toIndex - Math.max(0, n), fromIndex), toIndex, array, mapF)

  /** Lazily narrows Slice to exclude first N items. */
  final def drop(n: Int): Slice[T] =
    Slice.of[T, A](Math.min(fromIndex + Math.max(0, n), toIndex), toIndex, array, mapF)

  /** Lazily narrows Slice to exclude last N items. */
  final def dropRight(n: Int): Slice[T] =
    Slice.of[T, A](fromIndex, Math.max(toIndex - Math.max(0, n), fromIndex), array, mapF)

  /** Returns iterator over Slice values. */
  final def iterator: Iterator[T] = new Iterator[T] {

    var i = fromIndex

    def hasNext: Boolean = i < toIndex

    def next(): T = {
      val value = mapF(array(i))
      i = i + 1
      value
    }
  }

  /** Returns iterator over Slice values in the reverse order. */
  final def reverseIterator: Iterator[T] = new Iterator[T] {

    var i = toIndex - 1

    def hasNext: Boolean = i >= fromIndex

    def next(): T = {
      val value = mapF(array(i))
      i = i - 1
      value
    }
  }

  /** Returns iterator over Slice values, fulfilling the predicate, in the reverse order. */
  final def reverseIterator(pred: T => Boolean): Iterator[T] = new Iterator[T] {

    var i = toIndex - 1

    if (i >= fromIndex) seekNext

    def hasNext: Boolean = i >= fromIndex

    def next(): T = {
      val value = mapF(array(i))
      i = i - 1
      seekNext
      value
    }

    def seekNext: Unit = {
      var v = mapF(array(i))
      while (!pred(v) && i >= fromIndex) {
        i = i - 1
        if (i > fromIndex) v = mapF(array(i))
      }
    }
  }

  /** Returns minimal copy of an underlying array, trimmed to the actual range.
    * @group Read */
  final def toArray(implicit tag: ClassTag[T]): Array[T] = {
    val newArray: Array[A] = ArrayOps.copyOf(array, length)
    Array.copy(array, fromIndex, newArray, 0, length)
    newArray.map(mapF)
  }

  /** Returns buffer with a copy of this Slice.
    * @group Read */
  final def toBuffer(implicit tag: ClassTag[T]): Buffer[T] =
    new ArrayBuffer(toArray)

  /** Returns new list of Slice values.
    * @group Read */
  final def toList: List[T] = iterator.toList

  /** Returns new iterable of Slice values.
    * @group Read */
  final def asIterable: Iterable[T] = new AbstractIterable[T] {
    override def iterator: Iterator[T] = ArraySlice.this.iterator
    override def toString(): String = ArraySlice.this.toString
  }

  final override def toString: String =
    iterator.take(Math.min(20, length)).mkString("Slice(", ",", if (length > 20) ", ... )" else ")")

  final override def equals(obj: Any): Boolean = obj match {
    case other: Slice[T] =>
      this.length == other.length &&
        Compare.sameElements(this.iterator, other.iterator)

    case _ => false
  }

  final override def hashCode(): Int = {
    var hash = 17
    hash = hash * 31 + this.length
    for (i <- fromIndex to toIndex by (length / 7)) {
      hash = hash * 31 + array(i).hashCode()
    }
    hash
  }
}