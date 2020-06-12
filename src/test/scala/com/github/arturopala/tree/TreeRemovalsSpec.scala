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

import com.github.arturopala.tree.LaxTreeOps._

import scala.reflect.ClassTag

class TreeRemovalsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "remove distinct a child node holding a value" in {
      tree0.removeChildValue("a") shouldBe tree0
      tree0.removeChildValue("b") shouldBe tree0
      tree1.removeChildValue("a") shouldBe tree1
      tree1.removeChildValue("b") shouldBe tree1
      tree2.removeChildValue("a") shouldBe tree2
      tree2.removeChildValue("b") shouldBe tree1
      tree3_1.removeChildValue("a") shouldBe tree3_1
      tree3_1.removeChildValue("b") shouldBe Tree("a", Tree("c"))
      tree3_1.removeChildValue("c") shouldBe tree3_1
      tree3_2.removeChildValue("a") shouldBe tree3_2
      tree3_2.removeChildValue("b") shouldBe Tree("a", Tree("c"))
      tree3_2.removeChildValue("c") shouldBe Tree("a", Tree("b"))
      tree4_2.removeChildValue("a") shouldBe tree4_2
      tree4_2.removeChildValue("a") shouldBe tree4_2
      tree4_2.removeChildValue("b") shouldBe Tree("a", Tree("c"), Tree("d"))
      tree4_2.removeChildValue("d") shouldBe tree3_1
      tree7.removeChildValue("a") shouldBe tree7
      tree7.removeChildValue("b") shouldBe Tree("a", Tree("c"), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeChildValue("d") shouldBe Tree("a", Tree("b", Tree("c")), Tree("e", Tree("f")), Tree("g"))
      tree7.removeChildValue("g") shouldBe Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.removeChildValue("e") shouldBe tree7
      tree7.removeChildValue("c") shouldBe tree7
      tree9.removeChildValue("e") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeChildValue("b") shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeChildValue("a") shouldBe tree9
      // test merging down
      tree(Tree("a", Tree("b", Tree("c")), Tree("c"))).removeChildValue("b") shouldBe Tree("a", Tree("c"))
      tree(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("c"), Tree("e")))
        .removeChildValue("b") shouldBe Tree("a", Tree("c"), Tree("d"), Tree("e"))
      // test node selection when multiple duplicates
      tree(Tree("a", Tree("b"), Tree("c", Tree("d")), Tree("c", Tree("e")), Tree("c", Tree("f"))))
        .removeChildValue("c") shouldBe
        Tree("a", Tree("b"), Tree("d"), Tree("c", Tree("e")), Tree("c", Tree("f")))
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildValue("b") shouldBe
        Tree("a", Tree("c"), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "remove lax a child node holding a value" in {
      tree0.removeChildValueLax("a") shouldBe tree0
      tree0.removeChildValueLax("b") shouldBe tree0
      tree1.removeChildValueLax("a") shouldBe tree1
      tree1.removeChildValueLax("b") shouldBe tree1
      tree2.removeChildValueLax("a") shouldBe tree2
      tree2.removeChildValueLax("b") shouldBe tree1
      tree3_1.removeChildValueLax("a") shouldBe tree3_1
      tree3_1.removeChildValueLax("b") shouldBe Tree("a", Tree("c"))
      tree3_1.removeChildValueLax("c") shouldBe tree3_1
      tree3_2.removeChildValueLax("a") shouldBe tree3_2
      tree3_2.removeChildValueLax("b") shouldBe Tree("a", Tree("c"))
      tree3_2.removeChildValueLax("c") shouldBe Tree("a", Tree("b"))
      tree4_2.removeChildValueLax("a") shouldBe tree4_2
      tree4_2.removeChildValueLax("a") shouldBe tree4_2
      tree4_2.removeChildValueLax("b") shouldBe Tree("a", Tree("c"), Tree("d"))
      tree4_2.removeChildValueLax("d") shouldBe tree3_1
      tree7.removeChildValueLax("a") shouldBe tree7
      tree7.removeChildValueLax("b") shouldBe Tree("a", Tree("c"), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeChildValueLax("d") shouldBe Tree("a", Tree("b", Tree("c")), Tree("e", Tree("f")), Tree("g"))
      tree7.removeChildValueLax("g") shouldBe Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.removeChildValueLax("e") shouldBe tree7
      tree7.removeChildValueLax("c") shouldBe tree7
      tree9.removeChildValueLax("e") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeChildValueLax("b") shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeChildValueLax("a") shouldBe tree9
      tree(Tree("a", Tree("b", Tree("c")), Tree("c"))).removeChildValueLax("b") shouldBe Tree("a", Tree("c"), Tree("c"))
      tree(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("c"), Tree("e")))
        .removeChildValueLax("b") shouldBe Tree("a", Tree("c"), Tree("d"), Tree("c"), Tree("e"))
    }

    "remove a node selected by the path and insert children into the parent" in {
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
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("e"))))
        .removeValueAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"))))
        .removeValueAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")))
    }

    "remove a node selected by the path using extractor function, and insert children into the parent" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.removeValueAt(List(), codeF) shouldBe Tree.empty
      tree1.removeValueAt(List(97), codeF) shouldBe Tree.empty
      tree1.removeValueAt(List(98), codeF) shouldBe tree1
      tree2.removeValueAt(List(97), codeF) shouldBe Tree("b")
      tree2.removeValueAt(List(97, 98), codeF) shouldBe Tree("a")
      tree3_1.removeValueAt(List(97, 98, 99), codeF) shouldBe Tree("a", Tree("b"))
      tree3_1.removeValueAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c"))
      tree3_1.removeValueAt(List(97), codeF) shouldBe Tree("b", Tree("c"))
      tree3_1.removeValueAt(List(98), codeF) shouldBe tree3_1
      tree3_1.removeValueAt(List(98, 99), codeF) shouldBe tree3_1
      tree3_1.removeValueAt(List(97, 99), codeF) shouldBe tree3_1
      tree3_1.removeValueAt(List(98), codeF) shouldBe tree3_1
      tree3_2.removeValueAt(List(97, 98, 99), codeF) shouldBe tree3_2
      tree3_2.removeValueAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c"))
      tree3_2.removeValueAt(List(97, 99), codeF) shouldBe Tree("a", Tree("b"))
      tree3_2.removeValueAt(List(97, 100), codeF) shouldBe tree3_2
      tree3_2.removeValueAt(List(97), codeF) shouldBe tree3_2
      tree4_1.removeValueAt(List(97, 98, 99, 100, 101), codeF) shouldBe tree4_1
      tree4_1.removeValueAt(List(97, 98, 101, 100), codeF) shouldBe tree4_1
      tree4_1.removeValueAt(List(97, 98, 99, 100), codeF) shouldBe Tree("a", Tree("b", Tree("c")))
      tree4_1.removeValueAt(List(97, 98, 99), codeF) shouldBe Tree("a", Tree("b", Tree("d")))
      tree4_1.removeValueAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_1.removeValueAt(List(97), codeF) shouldBe Tree("b", Tree("c", Tree("d")))
      tree9.removeValueAt(List(97, 101), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeValueAt(List(97, 98), codeF) shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueAt(List(97, 98, 99), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueAt(List(97, 101, 102), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("g"), Tree("h", Tree("i")))
      )
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("e"))))
        .removeValueAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"))))
        .removeValueAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")))
    }

    "remove lax a node selected by the path and insert children into the parent" in {
      tree0.removeValueLaxAt(List()) shouldBe Tree.empty
      tree1.removeValueLaxAt(List("a")) shouldBe Tree.empty
      tree1.removeValueLaxAt(List("b")) shouldBe tree1
      tree2.removeValueLaxAt(List("a")) shouldBe Tree("b")
      tree2.removeValueLaxAt(List("a", "b")) shouldBe Tree("a")
      tree3_1.removeValueLaxAt(List("a", "b", "c")) shouldBe Tree("a", Tree("b"))
      tree3_1.removeValueLaxAt(List("a", "b")) shouldBe Tree("a", Tree("c"))
      tree3_1.removeValueLaxAt(List("a")) shouldBe Tree("b", Tree("c"))
      tree3_1.removeValueLaxAt(List("b")) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List("b", "c")) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List("a", "c")) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List("b")) shouldBe tree3_1
      tree3_2.removeValueLaxAt(List("a", "b", "c")) shouldBe tree3_2
      tree3_2.removeValueLaxAt(List("a", "b")) shouldBe Tree("a", Tree("c"))
      tree3_2.removeValueLaxAt(List("a", "c")) shouldBe Tree("a", Tree("b"))
      tree3_2.removeValueLaxAt(List("a", "d")) shouldBe tree3_2
      tree3_2.removeValueLaxAt(List("a")) shouldBe tree3_2
      tree4_1.removeValueLaxAt(List("a", "b", "c", "d", "e")) shouldBe tree4_1
      tree4_1.removeValueLaxAt(List("a", "b", "e", "d")) shouldBe tree4_1
      tree4_1.removeValueLaxAt(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree4_1.removeValueLaxAt(List("a", "b", "c")) shouldBe Tree("a", Tree("b", Tree("d")))
      tree4_1.removeValueLaxAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_1.removeValueLaxAt(List("a")) shouldBe Tree("b", Tree("c", Tree("d")))
      tree9.removeValueLaxAt(List("a", "e")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeValueLaxAt(List("a", "b")) shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueLaxAt(List("a", "b", "c")) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueLaxAt(List("a", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("g"), Tree("h", Tree("i")))
      )
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("e"))))
        .removeValueLaxAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d")), Tree("c", Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"))))
        .removeValueLaxAt(List("a", "b")) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("c", Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"), Tree("d"))))
        .removeValueLaxAt(List("a", "b")) shouldBe Tree(
        "a",
        Tree("c", Tree("d"), Tree("e")),
        Tree("c", Tree("e"), Tree("d"))
      )
    }

    "remove a node selected by the path using extractor function, and insert children into the parent" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.removeValueLaxAt(List(), codeF) shouldBe Tree.empty
      tree1.removeValueLaxAt(List(97), codeF) shouldBe Tree.empty
      tree1.removeValueLaxAt(List(98), codeF) shouldBe tree1
      tree2.removeValueLaxAt(List(97), codeF) shouldBe Tree("b")
      tree2.removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a")
      tree3_1.removeValueLaxAt(List(97, 98, 99), codeF) shouldBe Tree("a", Tree("b"))
      tree3_1.removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c"))
      tree3_1.removeValueLaxAt(List(97), codeF) shouldBe Tree("b", Tree("c"))
      tree3_1.removeValueLaxAt(List(98), codeF) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List(98, 99), codeF) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List(97, 99), codeF) shouldBe tree3_1
      tree3_1.removeValueLaxAt(List(98), codeF) shouldBe tree3_1
      tree3_2.removeValueLaxAt(List(97, 98, 99), codeF) shouldBe tree3_2
      tree3_2.removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c"))
      tree3_2.removeValueLaxAt(List(97, 99), codeF) shouldBe Tree("a", Tree("b"))
      tree3_2.removeValueLaxAt(List(97, 100), codeF) shouldBe tree3_2
      tree3_2.removeValueLaxAt(List(97), codeF) shouldBe tree3_2
      tree4_1.removeValueLaxAt(List(97, 98, 99, 100, 101), codeF) shouldBe tree4_1
      tree4_1.removeValueLaxAt(List(97, 98, 101, 100), codeF) shouldBe tree4_1
      tree4_1.removeValueLaxAt(List(97, 98, 99, 100), codeF) shouldBe Tree("a", Tree("b", Tree("c")))
      tree4_1.removeValueLaxAt(List(97, 98, 99), codeF) shouldBe Tree("a", Tree("b", Tree("d")))
      tree4_1.removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_1.removeValueLaxAt(List(97), codeF) shouldBe Tree("b", Tree("c", Tree("d")))
      tree9.removeValueLaxAt(List(97, 101), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeValueLaxAt(List(97, 98), codeF) shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueLaxAt(List(97, 98, 99), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValueLaxAt(List(97, 101, 102), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("g"), Tree("h", Tree("i")))
      )
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c", Tree("e"))))
        .removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d")), Tree("c", Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"))))
        .removeValueLaxAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c", Tree("d"), Tree("e")), Tree("c", Tree("e")))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"), Tree("e"))), Tree("c", Tree("e"), Tree("d"))))
        .removeValueLaxAt(List(97, 98), codeF) shouldBe Tree(
        "a",
        Tree("c", Tree("d"), Tree("e")),
        Tree("c", Tree("e"), Tree("d"))
      )
    }

    "remove a child tree holding a value" in {
      tree0.removeChild("a") shouldBe tree0
      tree0.removeChild("b") shouldBe tree0
      tree1.removeChild("a") shouldBe tree1
      tree1.removeChild("b") shouldBe tree1
      tree2.removeChild("a") shouldBe tree2
      tree2.removeChild("b") shouldBe tree1
      tree3_1.removeChild("b") shouldBe tree1
      tree3_2.removeChild("b") shouldBe Tree("a", Tree("c"))
      tree3_2.removeChild("c") shouldBe Tree("a", Tree("b"))
      tree4_1.removeChild("b") shouldBe tree1
      tree4_1.removeChild("c") shouldBe tree4_1
      tree4_2.removeChild("b") shouldBe Tree("a", Tree("d"))
      tree4_2.removeChild("d") shouldBe tree3_1
      tree7.removeChild("b") shouldBe
        Tree("a", Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeChild("d") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("g"))
      tree7.removeChild("g") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))).removeChild("b") shouldBe
        Tree("a", Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "remove subtree selected by the path" in {
      tree0.removeTreeAt(List()) shouldBe tree0
      tree0.removeTreeAt(List("a")) shouldBe tree0
      tree0.removeTreeAt(List("a", "b")) shouldBe tree0
      tree1.removeTreeAt(List()) shouldBe tree1
      tree1.removeTreeAt(List("a")) shouldBe Tree.empty
      tree1.removeTreeAt(List("b")) shouldBe tree1
      tree2.removeTreeAt(List()) shouldBe tree2
      tree2.removeTreeAt(List("a")) shouldBe Tree.empty
      tree2.removeTreeAt(List("a", "b")) shouldBe Tree("a")
      tree3_1.removeTreeAt(List()) shouldBe tree3_1
      tree3_1.removeTreeAt(List("a")) shouldBe Tree.empty
      tree3_1.removeTreeAt(List("a", "b")) shouldBe Tree("a")
      tree3_1.removeTreeAt(List("a", "c")) shouldBe tree3_1
      tree3_1.removeTreeAt(List("a", "a")) shouldBe tree3_1
      tree3_2.removeTreeAt(List()) shouldBe tree3_2
      tree3_2.removeTreeAt(List("a")) shouldBe Tree.empty
      tree3_2.removeTreeAt(List("a", "b")) shouldBe Tree("a", Tree("c"))
      tree3_2.removeTreeAt(List("a", "c")) shouldBe Tree("a", Tree("b"))
      tree3_2.removeTreeAt(List("a", "a")) shouldBe tree3_2
      tree4_2.removeTreeAt(List()) shouldBe tree4_2
      tree4_2.removeTreeAt(List("a")) shouldBe Tree.empty
      tree4_2.removeTreeAt(List("a", "b")) shouldBe Tree("a", Tree("d"))
      tree4_2.removeTreeAt(List("a", "c")) shouldBe tree4_2
      tree4_2.removeTreeAt(List("a", "a")) shouldBe tree4_2
      tree7.removeTreeAt(List()) shouldBe tree7
      tree7.removeTreeAt(List("a")) shouldBe Tree.empty
      tree7.removeTreeAt(List("a", "b")) shouldBe Tree("a", Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeTreeAt(List("a", "b", "c")) shouldBe Tree("a", Tree("b"), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeTreeAt(List("a", "d")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("g"))
      tree7.removeTreeAt(List("a", "g")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.removeTreeAt(List("a", "d", "e")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("g"))
      tree7.removeTreeAt(List("a", "d", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e")),
        Tree("g")
      )
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeTreeAt(List("a", "b")) shouldBe
        Tree("a", Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "remove subtree selected by the path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.removeTreeAt(List(), codeF) shouldBe tree0
      tree0.removeTreeAt(List(97), codeF) shouldBe tree0
      tree0.removeTreeAt(List(97, 98), codeF) shouldBe tree0
      tree1.removeTreeAt(List(), codeF) shouldBe tree1
      tree1.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree1.removeTreeAt(List(98), codeF) shouldBe tree1
      tree2.removeTreeAt(List(), codeF) shouldBe tree2
      tree2.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree2.removeTreeAt(List(97, 98), codeF) shouldBe Tree("a")
      tree3_1.removeTreeAt(List(), codeF) shouldBe tree3_1
      tree3_1.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree3_1.removeTreeAt(List(97, 98), codeF) shouldBe Tree("a")
      tree3_1.removeTreeAt(List(97, 99), codeF) shouldBe tree3_1
      tree3_1.removeTreeAt(List(97, 97), codeF) shouldBe tree3_1
      tree3_2.removeTreeAt(List(), codeF) shouldBe tree3_2
      tree3_2.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree3_2.removeTreeAt(List(97, 98), codeF) shouldBe Tree("a", Tree("c"))
      tree3_2.removeTreeAt(List(97, 99), codeF) shouldBe Tree("a", Tree("b"))
      tree3_2.removeTreeAt(List(97, 97), codeF) shouldBe tree3_2
      tree4_2.removeTreeAt(List(), codeF) shouldBe tree4_2
      tree4_2.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree4_2.removeTreeAt(List(97, 98), codeF) shouldBe Tree("a", Tree("d"))
      tree4_2.removeTreeAt(List(97, 99), codeF) shouldBe tree4_2
      tree4_2.removeTreeAt(List(97, 97), codeF) shouldBe tree4_2
      tree7.removeTreeAt(List(), codeF) shouldBe tree7
      tree7.removeTreeAt(List(97), codeF) shouldBe Tree.empty
      tree7.removeTreeAt(List(97, 98), codeF) shouldBe Tree("a", Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7
        .removeTreeAt(List(97, 98, 99), codeF) shouldBe Tree("a", Tree("b"), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeTreeAt(List(97, 100), codeF) shouldBe Tree("a", Tree("b", Tree("c")), Tree("g"))
      tree7
        .removeTreeAt(List(97, 103), codeF) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.removeTreeAt(List(97, 100, 101), codeF) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("g"))
      tree7.removeTreeAt(List(97, 100, 101, 102), codeF) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e")),
        Tree("g")
      )
    }

    "remove children" in {
      tree0.removeChildren() shouldBe tree0
      tree1.removeChildren() shouldBe tree1
      tree2.removeChildren() shouldBe tree1
      tree3_1.removeChildren() shouldBe tree1
      tree3_2.removeChildren() shouldBe tree1
      tree4_1.removeChildren() shouldBe tree1
      tree4_2.removeChildren() shouldBe tree1
      tree4_3.removeChildren() shouldBe tree1
      tree7.removeChildren() shouldBe tree1
      tree9.removeChildren() shouldBe tree1
      tree13.removeChildren() shouldBe tree1
    }

    "remove children from the node selected by the path" in {
      tree0.removeChildrenAt(List()) shouldBe tree0
      tree0.removeChildrenAt(List("a")) shouldBe tree0
      tree0.removeChildrenAt(List("b")) shouldBe tree0
      tree1.removeChildrenAt(List()) shouldBe tree1
      tree1.removeChildrenAt(List("a")) shouldBe tree1
      tree1.removeChildrenAt(List("b")) shouldBe tree1
      tree2.removeChildrenAt(List()) shouldBe tree2
      tree2.removeChildrenAt(List("a")) shouldBe tree1
      tree2.removeChildrenAt(List("b")) shouldBe tree2
      tree3_1.removeChildrenAt(List()) shouldBe tree3_1
      tree3_1.removeChildrenAt(List("a")) shouldBe tree1
      tree3_1.removeChildrenAt(List("a", "b")) shouldBe tree2
      tree3_1.removeChildrenAt(List("b")) shouldBe tree3_1
      tree7.removeChildrenAt(List("a", "d")) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("g"))
      tree7.removeChildrenAt(List("a")) shouldBe tree1
      tree(Tree("a", Tree("b", Tree("c"), Tree("d", Tree("e"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List("a", "b")) shouldBe
        Tree("a", Tree("b"), Tree("b", Tree("d")), Tree("b", Tree("e")))
      tree(Tree("a", Tree("b", Tree("d", Tree("e")), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List("a", "b", "d")) shouldBe
        Tree("a", Tree("b", Tree("d"), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e")))
      tree(Tree("a", Tree("b", Tree("d", Tree("e")), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List("a", "b")) shouldBe
        Tree("a", Tree("b"), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "remove children from the node selected by the path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.removeChildrenAt(List(), codeF) shouldBe tree0
      tree0.removeChildrenAt(List(97), codeF) shouldBe tree0
      tree0.removeChildrenAt(List(98), codeF) shouldBe tree0
      tree1.removeChildrenAt(List(), codeF) shouldBe tree1
      tree1.removeChildrenAt(List(97), codeF) shouldBe tree1
      tree1.removeChildrenAt(List(98), codeF) shouldBe tree1
      tree2.removeChildrenAt(List(), codeF) shouldBe tree2
      tree2.removeChildrenAt(List(97), codeF) shouldBe tree1
      tree2.removeChildrenAt(List(98), codeF) shouldBe tree2
      tree3_1.removeChildrenAt(List(), codeF) shouldBe tree3_1
      tree3_1.removeChildrenAt(List(97), codeF) shouldBe tree1
      tree3_1.removeChildrenAt(List(97, 98), codeF) shouldBe tree2
      tree3_1.removeChildrenAt(List(98), codeF) shouldBe tree3_1
      tree7.removeChildrenAt(List(97, 100), codeF) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d"), Tree("g"))
      tree7.removeChildrenAt(List(97), codeF) shouldBe tree1
      tree(Tree("a", Tree("b", Tree("c"), Tree("d", Tree("e"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List(97, 98), codeF) shouldBe
        Tree("a", Tree("b"), Tree("b", Tree("d")), Tree("b", Tree("e")))
      tree(Tree("a", Tree("b", Tree("d", Tree("e")), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List(97, 98, 100), codeF) shouldBe
        Tree("a", Tree("b", Tree("d"), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e")))
      tree(Tree("a", Tree("b", Tree("d", Tree("e")), Tree("d", Tree("f"))), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .removeChildrenAt(List(97, 98), codeF) shouldBe
        Tree("a", Tree("b"), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

  }

}
