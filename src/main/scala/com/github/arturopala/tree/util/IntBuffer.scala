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

/** Growable array of integers.
  * Can act as well as a stack of integers. */
final class IntBuffer(initialSize: Int = 8) {

  private var array = new Array[Int](initialSize)

  private var maxIndex = -1

  def apply(index: Int): Int = if (index >= array.length) 0 else array(index)

  def update(index: Int, value: Int): Unit = {
    ensureIndex(index)
    array.update(index, value)
    maxIndex = Math.max(index, maxIndex)
  }

  private def ensureIndex(index: Int): Unit =
    if (index >= array.length) {
      val newArray: Array[Int] = new Array(Math.max(array.length * 2, index + 1))
      java.lang.System.arraycopy(array, 0, newArray, 0, array.length)
      array = newArray
    }

  @`inline` def append(value: Int): IntBuffer = push(value)

  def store(value: Int): IntBuffer = {
    if (maxIndex < 0) maxIndex = 0
    update(maxIndex, value)
    this
  }

  def push(value: Int): IntBuffer = {
    update(length, value)
    this
  }

  def peek: Int = {
    if (maxIndex < 0) throw new NoSuchElementException
    array(maxIndex)
  }

  def pop: Int = {
    val value = peek
    maxIndex = maxIndex - 1
    value
  }

  def length: Int = maxIndex + 1

  def toArray: Array[Int] = java.util.Arrays.copyOf(array, length)

  def toSlice: IntSlice = IntSlice.of(array, 0, length)

  def isEmpty: Boolean = length == 0

  def nonEmpty: Boolean = length > 0

  def reset: Int = {
    val l = length
    maxIndex = -1
    l
  }

  override def toString: String =
    array.take(Math.min(20, length)).mkString("[", ",", if (length > 20) ", ... ]" else "]")

}
