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

    "remove a child node holding a value" in {
      tree0.removeValue("a") shouldBe tree0
      tree0.removeValue("b") shouldBe tree0
      tree1.removeValue("a") shouldBe tree1
      tree1.removeValue("b") shouldBe tree1
      tree2.removeValue("a") shouldBe tree2
      tree2.removeValue("b") shouldBe tree1
      tree3_1.removeValue("a") shouldBe tree3_1
      tree3_1.removeValue("b") shouldBe Tree("a", Tree("c"))
      tree3_1.removeValue("c") shouldBe tree3_1
      tree3_2.removeValue("a") shouldBe tree3_2
      tree3_2.removeValue("b") shouldBe Tree("a", Tree("c"))
      tree3_2.removeValue("c") shouldBe Tree("a", Tree("b"))
      tree4_2.removeValue("a") shouldBe tree4_2
      tree4_2.removeValue("a") shouldBe tree4_2
      tree4_2.removeValue("b") shouldBe Tree("a", Tree("c"), Tree("d"))
      tree4_2.removeValue("d") shouldBe tree3_1
      tree7.removeValue("a") shouldBe tree7
      tree7.removeValue("b") shouldBe Tree("a", Tree("c"), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.removeValue("d") shouldBe Tree("a", Tree("b", Tree("c")), Tree("e", Tree("f")), Tree("g"))
      tree7.removeValue("g") shouldBe Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.removeValue("e") shouldBe tree7
      tree7.removeValue("c") shouldBe tree7
      tree9.removeValue("e") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("f", Tree("g")),
        Tree("h", Tree("i"))
      )
      tree9.removeValue("b") shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )
      tree9.removeValue("a") shouldBe tree9
      tree(Tree("a", Tree("b", Tree("c")), Tree("c"))).removeValue("b") shouldBe Tree("a", Tree("c"))
      tree(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("c"), Tree("e")))
        .removeValue("b") shouldBe Tree("a", Tree("c"), Tree("d"), Tree("e"))
    }

  }

}
