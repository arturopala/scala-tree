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

package com.github.arturopala.tree

/** Tree folding and unfolding typeclass.
  * @tparam R result shape
  * */
trait TreeFold[R[+_]] {

  /** Empty result value */
  def empty: R[Nothing]

  /** Constructs instance from the head value and children. */
  def fold[T](head: T, children: Iterable[R[T]]): R[T]

  /** De-constructs instance into the head value and children. */
  def unfold[T](instance: R[T]): (T, Iterable[R[T]])

}

/** Known instances of [[TreeFold]]. */
object TreeFold {

  /** An instance of [[TreeFold]] typeclass for [[Tree]] */
  implicit object TreeFoldOfTree extends TreeFold[Tree] {

    override def empty: Tree[Nothing] = Tree.empty

    override def fold[T](head: T, children: Iterable[Tree[T]]): Tree[T] =
      Tree(head, children)

    override def unfold[T](instance: Tree[T]): (T, Iterable[Tree[T]]) =
      (instance.head, instance.children)
  }

}
