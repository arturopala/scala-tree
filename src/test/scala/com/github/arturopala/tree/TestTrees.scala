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

object TestTrees {

  val tree0: Tree[String] = Tree[String]()
  val tree1: Tree[String] = Tree("a")
  val tree2: Tree[String] = Tree("a", Tree("b"))
  val tree3_1: Tree[String] = Tree("a", Tree("b", Tree("c")))
  val tree3_2: Tree[String] = Tree("a", Tree("b"), Tree("c"))
  val tree4_1: Tree[String] = Tree("a", Tree("b", Tree("c", Tree("d"))))
  val tree4_2: Tree[String] = Tree("a", Tree("b", Tree("c")), Tree("d"))
  val tree4_3: Tree[String] = Tree("a", Tree("b"), Tree("c"), Tree("d"))
  val tree7: Tree[String] = Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
  val tree9: Tree[String] =
    Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i"))))

}
