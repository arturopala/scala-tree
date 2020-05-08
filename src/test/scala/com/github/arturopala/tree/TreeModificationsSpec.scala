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

class TreeModificationsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "modify a value of a node located at the path in the tree" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      tree0.modifyValueAt(List(), f) shouldBe Left(tree0)
      tree0.modifyValueAt(List("a"), f) shouldBe Left(tree0)
      tree0.modifyValueAt(List("b"), f) shouldBe Left(tree0)
      tree0.modifyValueAt(List("a", "b"), f) shouldBe Left(tree0)
      tree1.modifyValueAt(List(), f) shouldBe Left(tree1)
      tree1.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa"))
      tree1.modifyValueAt(List("b"), f) shouldBe Left(tree1)
      tree1.modifyValueAt(List("a", "b"), f) shouldBe Left(tree1)
      tree2.modifyValueAt(List(), f) shouldBe Left(tree2)
      tree2.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueAt(List("b"), f) shouldBe Left(tree2)
      tree2.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueAt(List("a", "c"), f) shouldBe Left(tree2)
      tree3_1.modifyValueAt(List(), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueAt(List("b"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c", Tree("c"))))
      tree3_1.modifyValueAt(List("a", "c"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueAt(List("a", "b", "d"), f) shouldBe Left(tree3_1)
      tree3_2.modifyValueAt(List(), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueAt(List("a"), fi("c")) shouldBe Right(Tree("c", Tree("b"), Tree("c")))
      tree3_2.modifyValueAt(List("b"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c"), Tree("c")))
      tree3_2.modifyValueAt(List("a", "c"), f) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueAt(List("a", "c"), fi("b")) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree3_2.modifyValueAt(List("a", "d"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b", "c"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b", "d"), f) shouldBe Left(tree3_2)
      tree4_2.modifyValueAt(List(), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List("b"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List("a", "c"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a", "d"), f) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueAt(List("a", "b", "d"), f) shouldBe Left(tree4_2)
      tree7.modifyValueAt(List(), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a"), f) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "b"), f) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "b"), fi("d")) shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d", "e"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d", "e", "f"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "g"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueAt(List("a", "g"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("b"))
      )
      tree7.modifyValueAt(List("a", "g"), fi("d")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("d"))
      )
      tree7.modifyValueAt(List("b"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "c"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "g", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "d", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "b", "c", "d"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "b", "e"), f) shouldBe Left(tree7)
    }

    "modify a value of a node located at the path in the tree using an extractor function" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      val e: String => Int = _.head.toInt
      tree0.modifyValueAt(List(), f, e) shouldBe Left(tree0)
      tree0.modifyValueAt(List(97), f, e) shouldBe Left(tree0)
      tree0.modifyValueAt(List(98), f, e) shouldBe Left(tree0)
      tree0.modifyValueAt(List(97, 98), f, e) shouldBe Left(tree0)
      tree1.modifyValueAt(List(), f, e) shouldBe Left(tree1)
      tree1.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa"))
      tree1.modifyValueAt(List(98), f, e) shouldBe Left(tree1)
      tree1.modifyValueAt(List(97, 98), f, e) shouldBe Left(tree1)
      tree2.modifyValueAt(List(), f, e) shouldBe Left(tree2)
      tree2.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueAt(List(98), f, e) shouldBe Left(tree2)
      tree2.modifyValueAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueAt(List(97, 99), f, e) shouldBe Left(tree2)
      tree3_1.modifyValueAt(List(), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueAt(List(98), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueAt(List(97, 99), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueAt(List(97, 98, 100), f, e) shouldBe Left(tree3_1)
      tree3_2.modifyValueAt(List(), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueAt(List(98), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueAt(List(97, 99), f, e) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueAt(List(97, 100), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97, 98, 99), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97, 98, 100), f, e) shouldBe Left(tree3_2)
      tree4_2.modifyValueAt(List(), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List(98), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List(97, 99), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List(97, 100), f, e) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueAt(List(97, 98, 100), f, e) shouldBe Left(tree4_2)
      tree7.modifyValueAt(List(), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97), f, e) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 98), f, e) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 98), fi("d"), e) shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 98, 99), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 100), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 100), fi("b"), e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 100, 101), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 100, 101, 102), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 103), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueAt(List(98), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97, 99), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97, 103, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97, 100, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97, 98, 99, 100), f, e) shouldBe Left(tree7)
      tree7.modifyValueAt(List(97, 98, 101), f, e) shouldBe Left(tree7)
    }

    "modify distinct a value of a node located at the path in the tree" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      tree0.modifyValueDistinctAt(List(), f) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List("a"), f) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List("a", "b"), f) shouldBe Left(tree0)
      tree1.modifyValueDistinctAt(List(), f) shouldBe Left(tree1)
      tree1.modifyValueDistinctAt(List("a"), f) shouldBe Right(Tree("aa"))
      tree1.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree1)
      tree1.modifyValueDistinctAt(List("a", "b"), f) shouldBe Left(tree1)
      tree2.modifyValueDistinctAt(List(), f) shouldBe Left(tree2)
      tree2.modifyValueDistinctAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree2)
      tree2.modifyValueDistinctAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueDistinctAt(List("a", "c"), f) shouldBe Left(tree2)
      tree3_1.modifyValueDistinctAt(List(), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueDistinctAt(List("a", "c"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueDistinctAt(List("a", "b", "d"), f) shouldBe Left(tree3_1)
      tree3_2.modifyValueDistinctAt(List(), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueDistinctAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c")))
      tree3_2.modifyValueDistinctAt(List("a", "c"), f) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueDistinctAt(List("a", "c"), fi("b")) shouldBe Right(Tree("a", Tree("b")))
      tree3_2.modifyValueDistinctAt(List("a", "d"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List("a", "b", "c"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List("a", "b", "d"), f) shouldBe Left(tree3_2)
      tree4_2.modifyValueDistinctAt(List(), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List("a", "b"), fi("d")) shouldBe Right(Tree("a", Tree("d", Tree("c"))))
      tree4_2.modifyValueDistinctAt(List("a", "c"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List("a", "d"), f) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueDistinctAt(List("a", "d"), fi("b")) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree4_2.modifyValueDistinctAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List("a", "b", "d"), f) shouldBe Left(tree4_2)
      tree7.modifyValueDistinctAt(List(), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a"), f) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "b"), f) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "b"), fi("d")) shouldBe Right(
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "b", "c"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "d"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "d"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "d"), fi("g")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("g", Tree("e", Tree("f"))))
      )
      tree7.modifyValueDistinctAt(List("a", "d", "e"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "d", "e", "f"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List("a", "g"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueDistinctAt(List("b"), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a", "c"), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a", "g", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a", "d", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a", "b", "c", "d"), f) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List("a", "b", "e"), f) shouldBe Left(tree7)
    }

    "modify distinct a value of a node located at the path in the tree using an extractor function" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      val e: String => Int = _.head.toInt
      tree0.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List(97), f, e) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree0)
      tree0.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Left(tree0)
      tree1.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree1)
      tree1.modifyValueDistinctAt(List(97), f, e) shouldBe Right(Tree("aa"))
      tree1.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree1)
      tree1.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Left(tree1)
      tree2.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree2)
      tree2.modifyValueDistinctAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree2)
      tree2.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueDistinctAt(List(97, 99), f, e) shouldBe Left(tree2)
      tree3_1.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueDistinctAt(List(97, 99), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueDistinctAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueDistinctAt(List(97, 98, 100), f, e) shouldBe Left(tree3_1)
      tree3_2.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueDistinctAt(List(97, 98), fi("c"), e) shouldBe Right(Tree("a", Tree("c")))
      tree3_2.modifyValueDistinctAt(List(97, 99), f, e) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueDistinctAt(List(97, 99), fi("b"), e) shouldBe Right(Tree("a", Tree("b")))
      tree3_2.modifyValueDistinctAt(List(97, 100), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List(97, 98, 99), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueDistinctAt(List(97, 98, 100), f, e) shouldBe Left(tree3_2)
      tree4_2.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List(97, 98), fi("d"), e) shouldBe Right(Tree("a", Tree("d", Tree("c"))))
      tree4_2.modifyValueDistinctAt(List(97, 99), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueDistinctAt(List(97, 100), f, e) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueDistinctAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueDistinctAt(List(97, 98, 100), f, e) shouldBe Left(tree4_2)
      tree7.modifyValueDistinctAt(List(), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97), f, e) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 98), f, e) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 98), fi("d"), e) shouldBe Right(
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 98, 99), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 100), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 100, 101), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 100, 101, 102), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueDistinctAt(List(97, 103), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueDistinctAt(List(98), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97, 99), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97, 103, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97, 100, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97, 98, 99, 100), f, e) shouldBe Left(tree7)
      tree7.modifyValueDistinctAt(List(97, 98, 101), f, e) shouldBe Left(tree7)
    }

  }

}
