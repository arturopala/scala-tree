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

class TreeUpdatesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "update head value" in {
      tree0.updateHead("b") shouldBe tree0
      tree1.updateHead("b") shouldBe Tree("b")
      tree2.updateHead("b") shouldBe Tree("b", Tree("b"))
      tree3_1.updateHead("b") shouldBe Tree("b", Tree("b", Tree("c")))
      tree3_2.updateHead("b") shouldBe Tree("b", Tree("b"), Tree("c"))
      tree4_2.updateHead("b") shouldBe Tree("b", Tree("b", Tree("c")), Tree("d"))
    }

    "update distinct child value" in {
      tree0.updateChildValue("a", "b") shouldBe tree0
      tree1.updateChildValue("a", "aa") shouldBe tree1
      tree2.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb"))
      tree3_1.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c")))
      tree3_1.updateChildValue("b", "c") shouldBe Tree("a", Tree("c", Tree("c")))
      tree3_1.updateChildValue("c", "bb") shouldBe tree3_1
      tree3_2.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb"), Tree("c"))
      tree3_2.updateChildValue("c", "cc") shouldBe Tree("a", Tree("b"), Tree("cc"))
      tree3_2.updateChildValue("c", "b") shouldBe Tree("a", Tree("b"))
      tree3_2.updateChildValue("b", "c") shouldBe Tree("a", Tree("c"))
      tree3_2.updateChildValue("d", "cc") shouldBe tree3_2
      tree4_1.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c", Tree("d"))))
      tree4_2.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c")), Tree("d"))
      tree4_2.updateChildValue("c", "bb") shouldBe tree4_2
      tree4_2.updateChildValue("d", "dd") shouldBe Tree("a", Tree("b", Tree("c")), Tree("dd"))
      tree4_2.updateChildValue("d", "b") shouldBe Tree("a", Tree("b", Tree("c")))
      tree4_2.updateChildValue("b", "d") shouldBe Tree("a", Tree("d", Tree("c")))
      tree4_3.updateChildValue("b", "bb") shouldBe Tree("a", Tree("bb"), Tree("c"), Tree("d"))
      tree4_3.updateChildValue("c", "cc") shouldBe Tree("a", Tree("b"), Tree("cc"), Tree("d"))
      tree4_3.updateChildValue("d", "dd") shouldBe Tree("a", Tree("b"), Tree("c"), Tree("dd"))
      tree4_3.updateChildValue("d", "b") shouldBe Tree("a", Tree("b"), Tree("c"))
      tree4_3.updateChildValue("c", "b") shouldBe Tree("a", Tree("b"), Tree("d"))
      tree4_3.updateChildValue("b", "d") shouldBe Tree("a", Tree("d"), Tree("c"))
      tree9.updateChildValue("e", "ee") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("ee", Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree9.updateChildValue("e", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d")), Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree9.updateChildValue("b", "e") shouldBe
        Tree("a", Tree("e", Tree("c", Tree("d")), Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("f")))))
        .updateChildValue("b", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("f"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("c", Tree("f")))))
        .updateChildValue("b", "e") shouldBe
        Tree("a", Tree("e", Tree("c", Tree("d"), Tree("f"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("c", Tree("f")))))
        .updateChildValue("e", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"), Tree("f"))))
      tree(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      ).updateChildValue("f", "b") shouldBe
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"), Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      tree(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      ).updateChildValue("b", "f") shouldBe
        Tree(
          "a",
          Tree("f", Tree("c", Tree("d"), Tree("g"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("h")))
        )
    }

    "update lax child value" in {
      tree0.updateChildValueLax("a", "b") shouldBe tree0
      tree1.updateChildValueLax("a", "aa") shouldBe tree1
      tree2.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb"))
      tree3_1.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c")))
      tree3_1.updateChildValueLax("b", "c") shouldBe Tree("a", Tree("c", Tree("c")))
      tree3_1.updateChildValueLax("c", "bb") shouldBe tree3_1
      tree3_2.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb"), Tree("c"))
      tree3_2.updateChildValueLax("c", "cc") shouldBe Tree("a", Tree("b"), Tree("cc"))
      tree3_2.updateChildValueLax("c", "b") shouldBe Tree("a", Tree("b"), Tree("b"))
      tree3_2.updateChildValueLax("b", "c") shouldBe Tree("a", Tree("c"), Tree("c"))
      tree3_2.updateChildValueLax("d", "cc") shouldBe tree3_2
      tree4_1.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c", Tree("d"))))
      tree4_2.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb", Tree("c")), Tree("d"))
      tree4_2.updateChildValueLax("c", "bb") shouldBe tree4_2
      tree4_2.updateChildValueLax("d", "dd") shouldBe Tree("a", Tree("b", Tree("c")), Tree("dd"))
      tree4_2.updateChildValueLax("d", "b") shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree4_2.updateChildValueLax("b", "d") shouldBe Tree("a", Tree("d", Tree("c")), Tree("d"))
      tree4_3.updateChildValueLax("b", "bb") shouldBe Tree("a", Tree("bb"), Tree("c"), Tree("d"))
      tree4_3.updateChildValueLax("c", "cc") shouldBe Tree("a", Tree("b"), Tree("cc"), Tree("d"))
      tree4_3.updateChildValueLax("d", "dd") shouldBe Tree("a", Tree("b"), Tree("c"), Tree("dd"))
      tree4_3.updateChildValueLax("d", "b") shouldBe Tree("a", Tree("b"), Tree("c"), Tree("b"))
      tree4_3.updateChildValueLax("c", "b") shouldBe Tree("a", Tree("b"), Tree("b"), Tree("d"))
      tree4_3.updateChildValueLax("b", "d") shouldBe Tree("a", Tree("d"), Tree("c"), Tree("d"))
      tree9.updateChildValueLax("e", "ee") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("ee", Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree9.updateChildValueLax("e", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree9.updateChildValueLax("b", "e") shouldBe
        Tree("a", Tree("e", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("f")))))
        .updateChildValueLax("b", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("f"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("c", Tree("f")))))
        .updateChildValueLax("b", "e") shouldBe
        Tree("a", Tree("e", Tree("c", Tree("d"))), Tree("e", Tree("c", Tree("f"))))
      tree(Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("c", Tree("f")))))
        .updateChildValueLax("e", "b") shouldBe
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("b", Tree("c", Tree("f"))))
      tree(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      ).updateChildValueLax("f", "b") shouldBe
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("b", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      tree(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
      ).updateChildValueLax("b", "f") shouldBe
        Tree(
          "a",
          Tree("f", Tree("c", Tree("d"))),
          Tree("b", Tree("c", Tree("e"))),
          Tree("f", Tree("c", Tree("g"))),
          Tree("f", Tree("c", Tree("h")))
        )
    }

    "update lax a value of a node selected by the path in the tree" in {
      tree0.updateValueLaxAt(List(), "x") shouldBe Left(tree0)
      tree0.updateValueLaxAt(List("a"), "x") shouldBe Left(tree0)
      tree0.updateValueLaxAt(List("b"), "x") shouldBe Left(tree0)
      tree0.updateValueLaxAt(List("a", "b"), "x") shouldBe Left(tree0)
      tree1.updateValueLaxAt(List(), "x") shouldBe Left(tree1)
      tree1.updateValueLaxAt(List("a"), "x") shouldBe Right(Tree("x"))
      tree1.updateValueLaxAt(List("b"), "x") shouldBe Left(tree1)
      tree1.updateValueLaxAt(List("a", "b"), "x") shouldBe Left(tree1)
      tree2.updateValueLaxAt(List(), "x") shouldBe Left(tree2)
      tree2.updateValueLaxAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b")))
      tree2.updateValueLaxAt(List("b"), "x") shouldBe Left(tree2)
      tree2.updateValueLaxAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x")))
      tree2.updateValueLaxAt(List("a", "c"), "x") shouldBe Left(tree2)
      tree3_1.updateValueLaxAt(List(), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b", Tree("c"))))
      tree3_1.updateValueLaxAt(List("b"), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x", Tree("c"))))
      tree3_1.updateValueLaxAt(List("a", "b"), "c") shouldBe Right(Tree("a", Tree("c", Tree("c"))))
      tree3_1.updateValueLaxAt(List("a", "c"), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x"))))
      tree3_1.updateValueLaxAt(List("a", "b", "d"), "x") shouldBe Left(tree3_1)
      tree3_2.updateValueLaxAt(List(), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b"), Tree("c")))
      tree3_2.updateValueLaxAt(List("a"), "c") shouldBe Right(Tree("c", Tree("b"), Tree("c")))
      tree3_2.updateValueLaxAt(List("b"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x"), Tree("c")))
      tree3_2.updateValueLaxAt(List("a", "b"), "c") shouldBe Right(Tree("a", Tree("c"), Tree("c")))
      tree3_2.updateValueLaxAt(List("a", "c"), "x") shouldBe Right(Tree("a", Tree("b"), Tree("x")))
      tree3_2.updateValueLaxAt(List("a", "c"), "b") shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree3_2.updateValueLaxAt(List("a", "d"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List("a", "b", "c"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List("a", "b", "d"), "x") shouldBe Left(tree3_2)
      tree4_2.updateValueLaxAt(List(), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b", Tree("c")), Tree("d")))
      tree4_2.updateValueLaxAt(List("b"), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x", Tree("c")), Tree("d")))
      tree4_2.updateValueLaxAt(List("a", "c"), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List("a", "d"), "x") shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("x")))
      tree4_2.updateValueLaxAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueLaxAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueLaxAt(List("a", "b", "d"), "x") shouldBe Left(tree4_2)
      tree7.updateValueLaxAt(List(), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a"), "x") shouldBe Right(
        Tree("x", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "b"), "x") shouldBe Right(
        Tree("a", Tree("x", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "b"), "d") shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "b", "c"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("x")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "d"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("x", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "d"), "b") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "d", "e"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("x", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "d", "e", "f"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("x"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List("a", "g"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("x"))
      )
      tree7.updateValueLaxAt(List("a", "g"), "b") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("b"))
      )
      tree7.updateValueLaxAt(List("a", "g"), "d") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("d"))
      )
      tree7.updateValueLaxAt(List("b"), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a", "c"), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a", "g", "g"), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a", "d", "g"), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a", "b", "c", "d"), "x") shouldBe Left(tree7)
      tree7.updateValueLaxAt(List("a", "b", "e"), "x") shouldBe Left(tree7)
    }

    "update lax a value of a node selected by the path in the tree using an extractor function" in {
      val e: String => Int = _.head.toInt
      tree0.updateValueLaxAt(List(), "x", e) shouldBe Left(tree0)
      tree0.updateValueLaxAt(List(97), "x", e) shouldBe Left(tree0)
      tree0.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree0)
      tree0.updateValueLaxAt(List(97, 98), "x", e) shouldBe Left(tree0)
      tree1.updateValueLaxAt(List(), "x", e) shouldBe Left(tree1)
      tree1.updateValueLaxAt(List(97), "x", e) shouldBe Right(Tree("x"))
      tree1.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree1)
      tree1.updateValueLaxAt(List(97, 98), "x", e) shouldBe Left(tree1)
      tree2.updateValueLaxAt(List(), "x", e) shouldBe Left(tree2)
      tree2.updateValueLaxAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b")))
      tree2.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree2)
      tree2.updateValueLaxAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x")))
      tree2.updateValueLaxAt(List(97, 99), "x", e) shouldBe Left(tree2)
      tree3_1.updateValueLaxAt(List(), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b", Tree("c"))))
      tree3_1.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x", Tree("c"))))
      tree3_1.updateValueLaxAt(List(97, 99), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueLaxAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x"))))
      tree3_1.updateValueLaxAt(List(97, 98, 100), "x", e) shouldBe Left(tree3_1)
      tree3_2.updateValueLaxAt(List(), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b"), Tree("c")))
      tree3_2.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x"), Tree("c")))
      tree3_2.updateValueLaxAt(List(97, 99), "x", e) shouldBe Right(Tree("a", Tree("b"), Tree("x")))
      tree3_2.updateValueLaxAt(List(97, 100), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List(97, 98, 99), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueLaxAt(List(97, 98, 100), "x", e) shouldBe Left(tree3_2)
      tree4_2.updateValueLaxAt(List(), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b", Tree("c")), Tree("d")))
      tree4_2.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x", Tree("c")), Tree("d")))
      tree4_2.updateValueLaxAt(List(97, 99), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueLaxAt(List(97, 100), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("x")))
      tree4_2.updateValueLaxAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueLaxAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueLaxAt(List(97, 98, 100), "x", e) shouldBe Left(tree4_2)
      tree7.updateValueLaxAt(List(), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97), "x", e) shouldBe Right(
        Tree("x", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 98), "x", e) shouldBe Right(
        Tree("a", Tree("x", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 98), "d", e) shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 98, 99), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("x")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 100), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("x", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 100), "b", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 100, 101), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("x", Tree("f"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 100, 101, 102), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("x"))), Tree("g"))
      )
      tree7.updateValueLaxAt(List(97, 103), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("x"))
      )
      tree7.updateValueLaxAt(List(98), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97, 99), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97, 103, 103), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97, 100, 103), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97, 98, 99, 100), "x", e) shouldBe Left(tree7)
      tree7.updateValueLaxAt(List(97, 98, 101), "x", e) shouldBe Left(tree7)
    }

    "update distinct a value of a node selected by the path in the tree" in {
      def fi(s: String): String => String = _ => s
      tree0.updateValueAt(List(), "x") shouldBe Left(tree0)
      tree0.updateValueAt(List("a"), "x") shouldBe Left(tree0)
      tree0.updateValueAt(List("b"), "x") shouldBe Left(tree0)
      tree0.updateValueAt(List("a", "b"), "x") shouldBe Left(tree0)
      tree1.updateValueAt(List(), "x") shouldBe Left(tree1)
      tree1.updateValueAt(List("a"), "x") shouldBe Right(Tree("x"))
      tree1.updateValueAt(List("b"), "x") shouldBe Left(tree1)
      tree1.updateValueAt(List("a", "b"), "x") shouldBe Left(tree1)
      tree2.updateValueAt(List(), "x") shouldBe Left(tree2)
      tree2.updateValueAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b")))
      tree2.updateValueAt(List("b"), "x") shouldBe Left(tree2)
      tree2.updateValueAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x")))
      tree2.updateValueAt(List("a", "c"), "x") shouldBe Left(tree2)
      tree3_1.updateValueAt(List(), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b", Tree("c"))))
      tree3_1.updateValueAt(List("b"), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x", Tree("c"))))
      tree3_1.updateValueAt(List("a", "c"), "x") shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x"))))
      tree3_1.updateValueAt(List("a", "b", "d"), "x") shouldBe Left(tree3_1)
      tree3_2.updateValueAt(List(), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b"), Tree("c")))
      tree3_2.updateValueAt(List("b"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x"), Tree("c")))
      tree3_2.updateValueAt(List("a", "b"), "c") shouldBe Right(Tree("a", Tree("c")))
      tree3_2.updateValueAt(List("a", "c"), "x") shouldBe Right(Tree("a", Tree("b"), Tree("x")))
      tree3_2.updateValueAt(List("a", "c"), "b") shouldBe Right(Tree("a", Tree("b")))
      tree3_2.updateValueAt(List("a", "d"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List("a", "b", "c"), "x") shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List("a", "b", "d"), "x") shouldBe Left(tree3_2)
      tree4_2.updateValueAt(List(), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List("a"), "x") shouldBe Right(Tree("x", Tree("b", Tree("c")), Tree("d")))
      tree4_2.updateValueAt(List("b"), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List("a", "b"), "x") shouldBe Right(Tree("a", Tree("x", Tree("c")), Tree("d")))
      tree4_2.updateValueAt(List("a", "b"), "d") shouldBe Right(Tree("a", Tree("d", Tree("c"))))
      tree4_2.updateValueAt(List("a", "c"), "x") shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List("a", "d"), "x") shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("x")))
      tree4_2.updateValueAt(List("a", "d"), "b") shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree4_2.updateValueAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueAt(List("a", "b", "c"), "x") shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueAt(List("a", "b", "d"), "x") shouldBe Left(tree4_2)
      tree7.updateValueAt(List(), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a"), "x") shouldBe Right(
        Tree("x", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "b"), "x") shouldBe Right(
        Tree("a", Tree("x", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "b"), "d") shouldBe Right(
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "b", "c"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("x")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "d"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("x", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "d"), "b") shouldBe Right(
        Tree("a", Tree("b", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "d"), "g") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("g", Tree("e", Tree("f"))))
      )
      tree7.updateValueAt(List("a", "d", "e"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("x", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "d", "e", "f"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("x"))), Tree("g"))
      )
      tree7.updateValueAt(List("a", "g"), "x") shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("x"))
      )
      tree7.updateValueAt(List("b"), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a", "c"), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a", "g", "g"), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a", "d", "g"), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a", "b", "c", "d"), "x") shouldBe Left(tree7)
      tree7.updateValueAt(List("a", "b", "e"), "x") shouldBe Left(tree7)
      // check if de-duplicates only the updated node
      tree3_2
        .insertLeafLax("b")
        .updateValueAt(List("a", "c"), "b") shouldBe Right(Tree("a", Tree("b"), Tree("b")))
    }

    "update distinct a value of a node selected by the path in the tree using an extractor function" in {
      val e: String => Int = _.head.toInt
      tree0.updateValueAt(List(), "x", e) shouldBe Left(tree0)
      tree0.updateValueAt(List(97), "x", e) shouldBe Left(tree0)
      tree0.updateValueAt(List(98), "x", e) shouldBe Left(tree0)
      tree0.updateValueAt(List(97, 98), "x", e) shouldBe Left(tree0)
      tree1.updateValueAt(List(), "x", e) shouldBe Left(tree1)
      tree1.updateValueAt(List(97), "x", e) shouldBe Right(Tree("x"))
      tree1.updateValueAt(List(98), "x", e) shouldBe Left(tree1)
      tree1.updateValueAt(List(97, 98), "x", e) shouldBe Left(tree1)
      tree2.updateValueAt(List(), "x", e) shouldBe Left(tree2)
      tree2.updateValueAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b")))
      tree2.updateValueAt(List(98), "x", e) shouldBe Left(tree2)
      tree2.updateValueAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x")))
      tree2.updateValueAt(List(97, 99), "x", e) shouldBe Left(tree2)
      tree3_1.updateValueAt(List(), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b", Tree("c"))))
      tree3_1.updateValueAt(List(98), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x", Tree("c"))))
      tree3_1.updateValueAt(List(97, 99), "x", e) shouldBe Left(tree3_1)
      tree3_1.updateValueAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x"))))
      tree3_1.updateValueAt(List(97, 98, 100), "x", e) shouldBe Left(tree3_1)
      tree3_2.updateValueAt(List(), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b"), Tree("c")))
      tree3_2.updateValueAt(List(98), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x"), Tree("c")))
      tree3_2.updateValueAt(List(97, 98), "c", e) shouldBe Right(Tree("a", Tree("c")))
      tree3_2.updateValueAt(List(97, 99), "x", e) shouldBe Right(Tree("a", Tree("b"), Tree("x")))
      tree3_2.updateValueAt(List(97, 99), "b", e) shouldBe Right(Tree("a", Tree("b")))
      tree3_2.updateValueAt(List(97, 100), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List(97, 98, 99), "x", e) shouldBe Left(tree3_2)
      tree3_2.updateValueAt(List(97, 98, 100), "x", e) shouldBe Left(tree3_2)
      tree4_2.updateValueAt(List(), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List(97), "x", e) shouldBe Right(Tree("x", Tree("b", Tree("c")), Tree("d")))
      tree4_2.updateValueAt(List(98), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List(97, 98), "x", e) shouldBe Right(Tree("a", Tree("x", Tree("c")), Tree("d")))
      tree4_2.updateValueAt(List(97, 98), "d", e) shouldBe Right(Tree("a", Tree("d", Tree("c"))))
      tree4_2.updateValueAt(List(97, 99), "x", e) shouldBe Left(tree4_2)
      tree4_2.updateValueAt(List(97, 100), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("x")))
      tree4_2.updateValueAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueAt(List(97, 98, 99), "x", e) shouldBe Right(Tree("a", Tree("b", Tree("x")), Tree("d")))
      tree4_2.updateValueAt(List(97, 98, 100), "x", e) shouldBe Left(tree4_2)
      tree7.updateValueAt(List(), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97), "x", e) shouldBe Right(
        Tree("x", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 98), "x", e) shouldBe Right(
        Tree("a", Tree("x", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 98), "d", e) shouldBe Right(
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 98, 99), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("x")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 100), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("x", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 100, 101), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("x", Tree("f"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 100, 101, 102), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("x"))), Tree("g"))
      )
      tree7.updateValueAt(List(97, 103), "x", e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("x"))
      )
      tree7.updateValueAt(List(98), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97, 99), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97, 103, 103), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97, 100, 103), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97, 98, 99, 100), "x", e) shouldBe Left(tree7)
      tree7.updateValueAt(List(97, 98, 101), "x", e) shouldBe Left(tree7)
    }

  }

}
