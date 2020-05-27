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

import com.github.arturopala.tree.Tree

/** Comparison and equality helpers */
object Compare {

  /** Checks if two trees are the same */
  @`inline` final def sameTrees[T](tree1: Tree[T], tree2: Tree[T]): Boolean = {

    val iterator1: Iterator[Tree[T]] = tree1.trees.iterator
    val iterator2: Iterator[Tree[T]] = tree2.trees.iterator

    var result: Boolean = true

    while (result && iterator1.hasNext && iterator2.hasNext) {
      val t1 = iterator1.next()
      val t2 = iterator2.next()
      result =
        result && t1.size == t2.size && t1.width == t2.width && t1.height == t2.height && t1.headOption == t2.headOption
    }

    result
  }

  /** Checks if two iterators would return same elements. */
  final def sameElements[T](iterator1: Iterator[T], iterator2: Iterator[T]): Boolean = {
    var result: Boolean = true
    while (result && iterator1.hasNext && iterator2.hasNext) {
      val t1 = iterator1.next()
      val t2 = iterator2.next()
      result = result && t1 == t2
    }
    result
  }

}
