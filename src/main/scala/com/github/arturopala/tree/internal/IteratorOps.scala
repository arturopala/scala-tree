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

/** Extensions to Iterator enabling lightweight composition. */
object IteratorOps {

  final implicit class IteratorExt[T](iterator: Iterator[T]) {

    def ++:(prepending: Iterator[T]): Iterator[T] =
      if (prepending.isEmpty) iterator
      else if (iterator.isEmpty) prepending
      else new ChainedIterator(prepending, iterator)

    def trim: Iterator[T] = iterator match {
      case chi: ChainedIterator[T] => chi.trim
      case other                   => other
    }
  }

  final class ChainedIterator[T](first: Iterator[T], second: Iterator[T]) extends Iterator[T] {

    override def hasNext: Boolean = first.hasNext || second.hasNext

    override def next(): T =
      if (first.hasNext) first.next()
      else if (second.hasNext) second.next()
      else throw new NoSuchElementException()

    def trim: Iterator[T] =
      if (first.hasNext) this
      else second
  }

}
