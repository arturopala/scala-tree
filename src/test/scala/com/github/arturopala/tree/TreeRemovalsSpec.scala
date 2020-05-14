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

class TreeRemovalsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "remove a node while merging distinct children with parent" in {
      tree0.removeValueAt(List()) shouldBe Tree.empty
      tree1.removeValueAt(List("a")) shouldBe Tree.empty
      tree1.removeValueAt(List("b")) shouldBe tree1
      tree2.removeValueAt(List("a")) shouldBe Tree("b")
      tree2.removeValueAt(List("a", "b")) shouldBe Tree("a")
      tree3_1.removeValueAt(List("a", "b", "c")) shouldBe Tree("a", Tree("b"))
      tree3_1.removeValueAt(List("a", "b")) shouldBe Tree("a", Tree("c"))
      tree3_1.removeValueAt(List("a")) shouldBe Tree("b", Tree("c"))
      tree3_1.removeValueAt(List("b")) shouldBe tree3_1
      tree3_1.removeValueAt(List("b", "c")) shouldBe tree3_1
      tree3_1.removeValueAt(List("a", "c")) shouldBe tree3_1
      tree3_1.removeValueAt(List("b")) shouldBe tree3_1
      tree3_2.removeValueAt(List("a", "b", "c")) shouldBe tree3_2
      tree3_2.removeValueAt(List("a", "b")) shouldBe Tree("a", Tree("c"))
      tree3_2.removeValueAt(List("a", "c")) shouldBe Tree("a", Tree("b"))
      tree3_2.removeValueAt(List("a", "d")) shouldBe tree3_2
      tree3_2.removeValueAt(List("a")) shouldBe tree3_2
      tree4_1.removeValueAt(List("a", "b", "c", "d", "e")) shouldBe tree4_1
      tree4_1.removeValueAt(List("a", "b", "e", "d")) shouldBe tree4_1
      tree4_1.removeValueAt(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree4_1.removeValueAt(List("a", "b", "c")) shouldBe Tree("a", Tree("b", Tree("d")))
      tree4_1.removeValueAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_1.removeValueAt(List("a")) shouldBe Tree("b", Tree("c", Tree("d")))
      tree9.removeValueAt(List("a", "e")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeValueAt(List("a", "b")) shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueAt(List("a", "b", "c")) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueAt(List("a", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("g"), Tree("h", Tree("i")))
      )
    }

  }

}
