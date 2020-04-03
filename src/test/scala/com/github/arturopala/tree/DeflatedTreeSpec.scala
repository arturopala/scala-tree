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

  override def name = "A deflated Tree"

  val tree0: Tree[String] = TestTrees.tree0.deflated
  val tree1: Tree[String] = TestTrees.tree1.deflated
  val tree2: Tree[String] = TestTrees.tree2.deflated
  val tree3_1: Tree[String] = TestTrees.tree3_1.deflated
  val tree3_2: Tree[String] = TestTrees.tree3_2.deflated
  val tree4_1: Tree[String] = TestTrees.tree4_1.deflated
  val tree4_2: Tree[String] = TestTrees.tree4_2.deflated
  val tree4_3: Tree[String] = TestTrees.tree4_3.deflated
  val tree7: Tree[String] = TestTrees.tree7.deflated
  val tree9: Tree[String] = TestTrees.tree9.deflated

  s"$name also" should {

    "return nodes in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).values shouldBe tree3_2.values
      Tree.inflate(tree7).values shouldBe tree7.values
      Tree.inflate(tree9).values shouldBe tree9.values
    }

    "return children in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).childrenValues shouldBe tree3_2.childrenValues
      Tree.inflate(tree7).childrenValues shouldBe tree7.childrenValues
      Tree.inflate(tree9).childrenValues shouldBe tree9.childrenValues
    }

    "return branches in the same order as an inflated tree" in {
      Tree.inflate(tree3_2).branches shouldBe tree3_2.branches
      Tree.inflate(tree7).branches shouldBe tree7.branches
      Tree.inflate(tree9).branches shouldBe tree9.branches
    }
  }

}
