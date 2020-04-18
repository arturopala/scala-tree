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

import com.github.arturopala.tree.Tree.NodeTree

object TestTrees {

  val tree0: Tree[String] = Tree[String]()

  /**
    * [a]
    */
  val tree1: NodeTree[String] = Tree("a")

  /**
    * [a,b]
    */
  val tree2: NodeTree[String] = Tree("a", Tree("b"))

  /**
    * [a,b,c]
    */
  val tree3_1: NodeTree[String] = Tree("a", Tree("b", Tree("c")))

  /**
    * [a,b]
    * [a,c]
    */
  val tree3_2: NodeTree[String] = Tree("a", Tree("b"), Tree("c"))

  /**
    * [a,b,c,d]
    */
  val tree4_1: NodeTree[String] = Tree("a", Tree("b", Tree("c", Tree("d"))))

  /**
    * [a,b,c]
    * [a,d]
    */
  val tree4_2: NodeTree[String] = Tree("a", Tree("b", Tree("c")), Tree("d"))

  /**
    * [a,b]
    * [a,c]
    * [a,d]
    */
  val tree4_3: NodeTree[String] = Tree("a", Tree("b"), Tree("c"), Tree("d"))

  /**
    * [a,b,c]
    * [a,d,e,f]
    * [a,g]
    */
  val tree7: NodeTree[String] = Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))

  /**
    * [a,b,c,d]
    * [a,e,f,g]
    * [a,e,h,i]
    */
  val tree9: NodeTree[String] =
    Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i"))))

  val allTrees: Seq[Tree[String]] =
    Seq(tree0, tree1, tree2, tree3_1, tree3_2, tree4_1, tree4_2, tree4_3, tree7, tree9)

}
