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
import scala.collection.AbstractIterable
import scala.reflect.ClassTag

/** Lazy, immutable slice of the underlying array. */
abstract class Slice[T] private (fromIndex: Int, toIndex: Int) extends (Int => T) {

  type A

  val array: Array[A]
  val mapF: A => T

  val length: Int = toIndex - fromIndex

  def apply(index: Int): T = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `apply` index in the interval [0,$length), but was $index.")
    mapF(array.apply(fromIndex + index))
  }

  def update(index: Int, value: T)(implicit tag: ClassTag[T]): Slice[T] = {
    if (index < 0 || index >= length)
      throw new IndexOutOfBoundsException(s"Expected an `update` index in the interval [0,$length), but was $index.")
    val modified: Array[T] = toArray[T]
    modified.update(index, value)
    Slice.of[T, T](0, length, modified, identity)
  }

  def map[K](f: T => K): Slice[K] = Slice.of[K, A](fromIndex, toIndex, array, mapF.andThen(f))

  def count(pred: T => Boolean): Int = {
    var a = 0
    var i = fromIndex
    while (i < toIndex) {
      if (pred(mapF(array(i)))) a = a + 1
      i = i + 1
    }
    a
  }

  def isEmpty: Boolean = length <= 0

  def slice(from: Int, to: Int): Slice[T] = {
    val t = fit(0, to, length)
    val f = fit(0, from, t)
    if (f == 0 && t == length) this
    else
      Slice.of[T, A](fromIndex + f, fromIndex + t, array, mapF)
  }

  private def fit(lower: Int, value: Int, upper: Int): Int =
    Math.min(Math.max(lower, value), upper)

  def take(n: Int): Slice[T] = {
    assert(n >= 0, "take parameter must be equal or greater to zero")
    Slice.of[T, A](fromIndex, Math.min(fromIndex + n, toIndex), array, mapF)
  }

  def takeRight(n: Int): Slice[T] = {
    assert(n >= 0, "takeRight parameter must be equal or greater to zero")
    Slice.of[T, A](Math.max(toIndex - n, fromIndex), toIndex, array, mapF)
  }

  def drop(n: Int): Slice[T] = {
    assert(n >= 0, "drop parameter must be equal or greater to zero")
    Slice.of[T, A](Math.min(fromIndex + n, toIndex), toIndex, array, mapF)
  }

  def dropRight(n: Int): Slice[T] = {
    assert(n >= 0, "dropRight parameter must be equal or greater to zero")
    Slice.of[T, A](fromIndex, Math.max(toIndex - n, fromIndex), array, mapF)
  }

  def iterator: Iterator[T] = new Iterator[T] {

    var i = fromIndex

    def hasNext: Boolean = i < toIndex

    def next(): T = {
      val value = mapF(array(i))
      i = i + 1
      value
    }
  }

  def reverseIterator: Iterator[T] = new Iterator[T] {

    var i = toIndex - 1

    def hasNext: Boolean = i >= fromIndex

    def next(): T = {
      val value = mapF(array(i))
      i = i - 1
      value
    }
  }

  def reverseIterator(pred: T => Boolean): Iterator[T] = new Iterator[T] {

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

  def toArray[T1 >: T](implicit tagT1: ClassTag[T1]): Array[T1] =
    iterator.toArray[T1]

  def toList: List[T] = iterator.toList

  def asIterable: Iterable[T] = new AbstractIterable[T] {
    override def iterator: Iterator[T] = Slice.this.iterator
    override def toString(): String = Slice.this.toString
  }

  override def toString: String =
    iterator.take(Math.min(20, length)).mkString("Slice(", ",", if (length > 20) ", ... )" else ")")

  override def equals(obj: Any): Boolean = obj match {
    case other: Slice[T] =>
      this.length == other.length &&
        Compare.sameElements(this.iterator, other.iterator)

    case _ => false
  }

  override def hashCode(): Int = {
    var hash = 17
    hash = hash * 31 + this.length
    for (i <- fromIndex to toIndex by (length / 7)) {
      hash = hash * 31 + array(i).hashCode()
    }
    hash
  }
}

object Slice {

  def apply[T: ClassTag](is: T*): Slice[T] = Slice.of(Array(is: _*))

  private[util] def of[T, K](fromIndex: Int, toIndex: Int, _array: Array[K], _mapF: K => T): Slice[T] =
    new Slice[T](fromIndex, toIndex) {
      type A = K
      val array: Array[A] = _array
      val mapF: A => T = _mapF
    }

  def of[T](array: Array[T]): Slice[T] = Slice.of[T, T](0, array.length, array, identity)

  def of[T](array: Array[T], from: Int, to: Int): Slice[T] = {
    assert(from >= 0, "When creating a Slice, parameter `from` must be greater or equal to zero.")
    assert(to <= array.length, "When creating a Slice, parameter `to` must be lower or equal to the array length.")
    assert(from <= to, "When creating a Slice, parameter `from` must be lower or equal to the parameter `to`.")
    Slice.of[T, T](from, to, array, identity)
  }

  def empty[T: ClassTag]: Slice[T] = Slice.of(Array.empty[T])

}
