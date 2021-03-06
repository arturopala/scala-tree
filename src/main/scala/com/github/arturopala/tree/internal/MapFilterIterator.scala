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

import java.util.NoSuchElementException

import scala.annotation.tailrec

/** Iterator over mapped items fulfilling the predicate.
  * Items itself are provided by the supplied iterator. */
final class MapFilterIterator[A, B](iterator: Iterator[A], f: A => B, pred: B => Boolean) extends Iterator[B] {

  var hasNext: Boolean = false
  var v: B = seekNext

  final override def next(): B =
    if (hasNext) {
      val value = v
      v = seekNext
      value
    } else throw new NoSuchElementException

  @tailrec
  final def seekNext: B =
    if (iterator.hasNext) {
      val i = iterator.next()
      v = f(i)
      if (pred(v)) {
        hasNext = true
        v
      } else seekNext
    } else {
      hasNext = false
      v
    }

}

/** Iterator over items fulfilling the predicate.
  * Items itself are provided by the supplied iterator. */
final class FilterIterator[A](iterator: Iterator[A], pred: A => Boolean) extends Iterator[A] {

  var hasNext: Boolean = false
  var v: A = _

  seekNext()

  final override def next(): A =
    if (hasNext) {
      val value = v
      seekNext()
      value
    } else throw new NoSuchElementException

  @tailrec
  final def seekNext(): Unit =
    if (iterator.hasNext) {
      v = iterator.next()
      if (pred(v)) {
        hasNext = true
      } else seekNext()
    } else {
      hasNext = false
    }

}
