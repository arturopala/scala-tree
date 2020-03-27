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

class DeflatedTreeSpec extends TreeSpec {

  override def name = "DeflatedTree"

  val tree0 = Tree[String]().deflate
  val tree1 = Tree("a").deflate
  val tree2 = Tree("a", Tree("b")).deflate
  val tree3_1 = Tree("a", Tree("b", Tree("c"))).deflate
  val tree3_2 = Tree("a", Tree("b"), Tree("c")).deflate
  val tree4_1 = Tree("a", Tree("b", Tree("c", Tree("d")))).deflate
  val tree4_2 = Tree("a", Tree("b", Tree("c")), Tree("d")).deflate
  val tree4_3 = Tree("a", Tree("b"), Tree("c"), Tree("d")).deflate
  val tree7 = Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")).deflate
  val tree9 = Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))).deflate

  s"$name also" should {

    "return nodes in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).nodes shouldBe tree3_2.nodes
      Tree.inflate(tree7).nodes shouldBe tree7.nodes
      Tree.inflate(tree9).nodes shouldBe tree9.nodes
    }

    "return children in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).children shouldBe tree3_2.children
      Tree.inflate(tree7).children shouldBe tree7.children
      Tree.inflate(tree9).children shouldBe tree9.children
    }

    "return branches in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).branches shouldBe tree3_2.branches
      Tree.inflate(tree7).branches shouldBe tree7.branches
      Tree.inflate(tree9).branches shouldBe tree9.branches
    }
  }

}
