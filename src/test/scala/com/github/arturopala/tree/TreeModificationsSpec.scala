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

class TreeModificationsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "modify head value" in {
      val f = (x: String) => s"$x$x"
      tree0.modifyHead(f) shouldBe tree0
      tree1.modifyHead(f) shouldBe Tree("aa")
      tree2.modifyHead(f) shouldBe Tree("aa", Tree("b"))
      tree2.modifyHead(_ => "b") shouldBe Tree("b", Tree("b"))
      tree3_1.modifyHead(f) shouldBe Tree("aa", Tree("b", Tree("c")))
      tree3_1.modifyHead(_ => "b") shouldBe Tree("b", Tree("b", Tree("c")))
      tree3_2.modifyHead(f) shouldBe Tree("aa", Tree("b"), Tree("c"))
      tree3_2.modifyHead(_ => "b") shouldBe Tree("b", Tree("b"), Tree("c"))
      tree4_2.modifyHead(f) shouldBe Tree("aa", Tree("b", Tree("c")), Tree("d"))
      tree4_2.modifyHead(_ => "b") shouldBe Tree("b", Tree("b", Tree("c")), Tree("d"))
    }

    "modify distinct a child value" in {
      val f: String => String = s => s + s
      tree0.modifyChildValue("a", f) shouldBe tree0
      tree0.modifyChildValue("b", f) shouldBe tree0
      tree1.modifyChildValue("a", f) shouldBe tree1
      tree1.modifyChildValue("b", f) shouldBe tree1
      tree2.modifyChildValue("a", f) shouldBe tree2
      tree2.modifyChildValue("b", f) shouldBe Tree("a", Tree("bb"))
      tree3_1.modifyChildValue("a", f) shouldBe tree3_1
      tree3_1.modifyChildValue("b", f) shouldBe Tree("a", Tree("bb", Tree("c")))
      tree3_2.modifyChildValue("a", f) shouldBe tree3_2
      tree3_2.modifyChildValue("b", f) shouldBe Tree("a", Tree("bb"), Tree("c"))
      tree3_2.modifyChildValue("c", f) shouldBe Tree("a", Tree("b"), Tree("cc"))
      tree4_2.modifyChildValue("a", f) shouldBe tree4_2
      tree4_2.modifyChildValue("b", f) shouldBe Tree("a", Tree("bb", Tree("c")), Tree("d"))
      tree4_2.modifyChildValue("c", f) shouldBe tree4_2
      tree4_2.modifyChildValue("d", f) shouldBe Tree("a", Tree("b", Tree("c")), Tree("dd"))
      tree7.modifyChildValue("d", f) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValue("b", f) shouldBe
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValue("g", f) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      // test merging down
      tree3_2.modifyChildValue("c", _ => "b") shouldBe Tree("a", Tree("b"))
      tree4_2.modifyChildValue("b", _ => "d") shouldBe Tree("a", Tree("d", Tree("c")))
      tree4_2.modifyChildValue("d", _ => "b") shouldBe Tree("a", Tree("b", Tree("c")))
      tree7.modifyChildValue("d", _ => "b") shouldBe
        Tree("a", Tree("b", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValue("b", _ => "d") shouldBe
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValue("g", _ => "b") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.modifyChildValue("b", _ => "g") shouldBe
        Tree("a", Tree("g", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree7.modifyChildValue("d", _ => "g") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("g", Tree("e", Tree("f"))))
      tree7.modifyChildValue("g", _ => "d") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))))
      tree(
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f")),
          Tree("b"),
          Tree("x", Tree("g")),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      ).modifyChildValue("x", _ => "c") shouldBe
        Tree("a", Tree("c", Tree("e")), Tree("c", Tree("f"), Tree("g")), Tree("b"), Tree("d"), Tree("c", Tree("h")))
      tree(
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f", Tree("i"))),
          Tree("b"),
          Tree("x", Tree("f", Tree("j"))),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      ).modifyChildValue("x", _ => "c") shouldBe
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f", Tree("i"), Tree("j"))),
          Tree("b"),
          Tree("d"),
          Tree("c", Tree("h"))
        )
    }

    "modify lax a child value" in {
      val f: String => String = s => s + s
      tree0.modifyChildValueLax("a", f) shouldBe tree0
      tree0.modifyChildValueLax("b", f) shouldBe tree0
      tree1.modifyChildValueLax("a", f) shouldBe tree1
      tree1.modifyChildValueLax("b", f) shouldBe tree1
      tree2.modifyChildValueLax("a", f) shouldBe tree2
      tree2.modifyChildValueLax("b", f) shouldBe Tree("a", Tree("bb"))
      tree3_1.modifyChildValueLax("a", f) shouldBe tree3_1
      tree3_1.modifyChildValueLax("b", f) shouldBe Tree("a", Tree("bb", Tree("c")))
      tree3_2.modifyChildValueLax("a", f) shouldBe tree3_2
      tree3_2.modifyChildValueLax("b", f) shouldBe Tree("a", Tree("bb"), Tree("c"))
      tree3_2.modifyChildValueLax("c", f) shouldBe Tree("a", Tree("b"), Tree("cc"))
      tree4_2.modifyChildValueLax("a", f) shouldBe tree4_2
      tree4_2.modifyChildValueLax("b", f) shouldBe Tree("a", Tree("bb", Tree("c")), Tree("d"))
      tree4_2.modifyChildValueLax("c", f) shouldBe tree4_2
      tree4_2.modifyChildValueLax("d", f) shouldBe Tree("a", Tree("b", Tree("c")), Tree("dd"))
      tree7.modifyChildValueLax("d", f) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("b", f) shouldBe
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("g", f) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      // test merging down
      tree3_2.modifyChildValueLax("c", _ => "b") shouldBe Tree("a", Tree("b"), Tree("b"))
      tree4_2.modifyChildValueLax("b", _ => "d") shouldBe Tree("a", Tree("d", Tree("c")), Tree("d"))
      tree4_2.modifyChildValueLax("d", _ => "b") shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree7.modifyChildValueLax("d", _ => "b") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("b", _ => "d") shouldBe
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("g", _ => "b") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("b"))
      tree7.modifyChildValueLax("b", _ => "g") shouldBe
        Tree("a", Tree("g", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("d", _ => "g") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("g", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildValueLax("g", _ => "d") shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("d"))
      tree(
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f")),
          Tree("b"),
          Tree("x", Tree("g")),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      ).modifyChildValueLax("x", _ => "c") shouldBe
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f")),
          Tree("b"),
          Tree("c", Tree("g")),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      tree(
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f", Tree("i"))),
          Tree("b"),
          Tree("x", Tree("f", Tree("j"))),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      ).modifyChildValueLax("x", _ => "c") shouldBe
        Tree(
          "a",
          Tree("c", Tree("e")),
          Tree("c", Tree("f", Tree("i"))),
          Tree("b"),
          Tree("c", Tree("f", Tree("j"))),
          Tree("d"),
          Tree("c", Tree("h"))
        )
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .modifyChildValueLax("b", _ => "x") shouldBe
        Tree("a", Tree("x", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "modify distinct a child tree" in {
      val f: Tree[String] => Tree[String] = t => t.insertChild(Tree("x", Tree("y")))
      tree0.modifyChild("a", f) shouldBe tree0
      tree1.modifyChild("a", f) shouldBe tree1
      tree2.modifyChild("a", f) shouldBe tree2
      tree2.modifyChild("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y"))))
      tree3_1.modifyChild("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y")), Tree("c")))
      tree3_2.modifyChild("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y"))), Tree("c"))
      tree3_2.modifyChild("c", f) shouldBe Tree("a", Tree("b"), Tree("c", Tree("x", Tree("y"))))
      val f2: Tree[String] => Tree[String] = t => Tree("c", Tree("d"))
      tree3_1.modifyChild("b", f2) shouldBe Tree("a", Tree("c", Tree("d")))
      tree3_2.modifyChild("b", f2) shouldBe Tree("a", Tree("c", Tree("d")))
      tree3_2.modifyChild("c", f2) shouldBe Tree("a", Tree("b"), Tree("c", Tree("d")))
      tree4_1.modifyChild("b", f2) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_2.modifyChild("b", f2) shouldBe Tree("a", Tree("c", Tree("d")), Tree("d"))
      tree4_2.modifyChild("d", f2) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c", Tree("d")))
      tree7.modifyChild("b", _ => Tree("d", Tree("e", Tree("g")))) shouldBe
        Tree("a", Tree("d", Tree("e", Tree("g"), Tree("f"))), Tree("g"))
      tree7.modifyChild("d", _ => Tree("b", Tree("c", Tree("e")), Tree("d"))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("e")), Tree("d")), Tree("g"))
      tree7.modifyChild("g", _ => Tree("b", Tree("c", Tree("e")), Tree("d"))) shouldBe
        Tree("a", Tree("b", Tree("c", Tree("e")), Tree("d")), Tree("d", Tree("e", Tree("f"))))
      // test case when tree modification doesn't change head which already have duplicate siblings
      // as this shall not trigger merging
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .modifyChild("b", _ => Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "modify lax a child tree" in {
      val f: Tree[String] => Tree[String] = t => t.insertChild(Tree("x", Tree("y")))
      tree0.modifyChildLax("a", f) shouldBe tree0
      tree1.modifyChildLax("a", f) shouldBe tree1
      tree2.modifyChildLax("a", f) shouldBe tree2
      tree2.modifyChildLax("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y"))))
      tree3_1.modifyChildLax("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y")), Tree("c")))
      tree3_2.modifyChildLax("b", f) shouldBe Tree("a", Tree("b", Tree("x", Tree("y"))), Tree("c"))
      tree3_2.modifyChildLax("c", f) shouldBe Tree("a", Tree("b"), Tree("c", Tree("x", Tree("y"))))
      val f2: Tree[String] => Tree[String] = t => Tree("c", Tree("d"))
      tree3_1.modifyChildLax("b", f2) shouldBe Tree("a", Tree("c", Tree("d")))
      tree3_2.modifyChildLax("b", f2) shouldBe Tree("a", Tree("c", Tree("d")), Tree("c"))
      tree3_2.modifyChildLax("c", f2) shouldBe Tree("a", Tree("b"), Tree("c", Tree("d")))
      tree4_1.modifyChildLax("b", f2) shouldBe Tree("a", Tree("c", Tree("d")))
      tree4_2.modifyChildLax("b", f2) shouldBe Tree("a", Tree("c", Tree("d")), Tree("d"))
      tree4_2.modifyChildLax("d", f2) shouldBe Tree("a", Tree("b", Tree("c")), Tree("c", Tree("d")))
      tree7.modifyChildLax("b", _ => Tree("d", Tree("e", Tree("g")))) shouldBe
        Tree("a", Tree("d", Tree("e", Tree("g"))), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      tree7.modifyChildLax("d", _ => Tree("b", Tree("c", Tree("e")), Tree("d"))) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("c", Tree("e")), Tree("d")), Tree("g"))
      tree7.modifyChildLax("g", _ => Tree("b", Tree("c", Tree("e")), Tree("d"))) shouldBe
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("b", Tree("c", Tree("e")), Tree("d")))
      // test case when tree modification doesn't change head which already have duplicate siblings
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .modifyChildLax("b", _ => Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "modify distinct a value of a node selected by the path in the tree" in {
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
      tree3_1.modifyValueAt(List("a", "c"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueAt(List("a", "b", "d"), f) shouldBe Left(tree3_1)
      tree3_2.modifyValueAt(List(), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueAt(List("b"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c")))
      tree3_2.modifyValueAt(List("a", "c"), f) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueAt(List("a", "c"), fi("b")) shouldBe Right(Tree("a", Tree("b")))
      tree3_2.modifyValueAt(List("a", "d"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b", "c"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List("a", "b", "d"), f) shouldBe Left(tree3_2)
      tree4_2.modifyValueAt(List(), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List("b"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List("a", "b"), fi("d")) shouldBe Right(Tree("a", Tree("d", Tree("c"))))
      tree4_2.modifyValueAt(List("a", "c"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List("a", "d"), f) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueAt(List("a", "d"), fi("b")) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
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
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "b", "c"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List("a", "d"), fi("g")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("g", Tree("e", Tree("f"))))
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
      tree7.modifyValueAt(List("b"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "c"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "g", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "d", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "b", "c", "d"), f) shouldBe Left(tree7)
      tree7.modifyValueAt(List("a", "b", "e"), f) shouldBe Left(tree7)
      // check if de-duplicates only the modified node
      tree3_2
        .insertLeafLax("b")
        .modifyValueAt(List("a", "c"), _ => "b") shouldBe Right(Tree("a", Tree("b"), Tree("b")))
    }

    "modify lax a value of a node selected by the path in the tree" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      tree0.modifyValueLaxAt(List(), f) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List("a"), f) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List("b"), f) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List("a", "b"), f) shouldBe Left(tree0)
      tree1.modifyValueLaxAt(List(), f) shouldBe Left(tree1)
      tree1.modifyValueLaxAt(List("a"), f) shouldBe Right(Tree("aa"))
      tree1.modifyValueLaxAt(List("b"), f) shouldBe Left(tree1)
      tree1.modifyValueLaxAt(List("a", "b"), f) shouldBe Left(tree1)
      tree2.modifyValueLaxAt(List(), f) shouldBe Left(tree2)
      tree2.modifyValueLaxAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueLaxAt(List("b"), f) shouldBe Left(tree2)
      tree2.modifyValueLaxAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueLaxAt(List("a", "c"), f) shouldBe Left(tree2)
      tree3_1.modifyValueLaxAt(List(), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueLaxAt(List("b"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueLaxAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c", Tree("c"))))
      tree3_1.modifyValueLaxAt(List("a", "c"), f) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueLaxAt(List("a", "b", "d"), f) shouldBe Left(tree3_1)
      tree3_2.modifyValueLaxAt(List(), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueLaxAt(List("a"), fi("c")) shouldBe Right(Tree("c", Tree("b"), Tree("c")))
      tree3_2.modifyValueLaxAt(List("b"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueLaxAt(List("a", "b"), fi("c")) shouldBe Right(Tree("a", Tree("c"), Tree("c")))
      tree3_2.modifyValueLaxAt(List("a", "c"), f) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueLaxAt(List("a", "c"), fi("b")) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree3_2.modifyValueLaxAt(List("a", "d"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List("a", "b", "c"), f) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List("a", "b", "d"), f) shouldBe Left(tree3_2)
      tree4_2.modifyValueLaxAt(List(), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List("a"), f) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueLaxAt(List("b"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List("a", "b"), f) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueLaxAt(List("a", "c"), f) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List("a", "d"), f) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueLaxAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueLaxAt(List("a", "b", "c"), f) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueLaxAt(List("a", "b", "d"), f) shouldBe Left(tree4_2)
      tree7.modifyValueLaxAt(List(), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a"), f) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "b"), f) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "b"), fi("d")) shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "b", "c"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "d"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "d"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "d", "e"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "d", "e", "f"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List("a", "g"), f) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueLaxAt(List("a", "g"), fi("b")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("b"))
      )
      tree7.modifyValueLaxAt(List("a", "g"), fi("d")) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("d"))
      )
      tree7.modifyValueLaxAt(List("b"), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a", "c"), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a", "g", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a", "d", "g"), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a", "b", "c", "d"), f) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List("a", "b", "e"), f) shouldBe Left(tree7)
    }

    "modify distinct a value of a node selected by the path in the tree using an extractor function" in {
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
      tree3_2.modifyValueAt(List(97, 98), fi("c"), e) shouldBe Right(Tree("a", Tree("c")))
      tree3_2.modifyValueAt(List(97, 99), f, e) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueAt(List(97, 99), fi("b"), e) shouldBe Right(Tree("a", Tree("b")))
      tree3_2.modifyValueAt(List(97, 100), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97, 98, 99), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueAt(List(97, 98, 100), f, e) shouldBe Left(tree3_2)
      tree4_2.modifyValueAt(List(), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List(98), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueAt(List(97, 98), fi("d"), e) shouldBe Right(Tree("a", Tree("d", Tree("c"))))
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
        Tree("a", Tree("d", Tree("c"), Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 98, 99), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueAt(List(97, 100), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
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

    "modify lax a value of a node selected by the path in the tree using an extractor function" in {
      val f: String => String = s => s + s
      def fi(s: String): String => String = _ => s
      val e: String => Int = _.head.toInt
      tree0.modifyValueLaxAt(List(), f, e) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List(97), f, e) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree0)
      tree0.modifyValueLaxAt(List(97, 98), f, e) shouldBe Left(tree0)
      tree1.modifyValueLaxAt(List(), f, e) shouldBe Left(tree1)
      tree1.modifyValueLaxAt(List(97), f, e) shouldBe Right(Tree("aa"))
      tree1.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree1)
      tree1.modifyValueLaxAt(List(97, 98), f, e) shouldBe Left(tree1)
      tree2.modifyValueLaxAt(List(), f, e) shouldBe Left(tree2)
      tree2.modifyValueLaxAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b")))
      tree2.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree2)
      tree2.modifyValueLaxAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb")))
      tree2.modifyValueLaxAt(List(97, 99), f, e) shouldBe Left(tree2)
      tree3_1.modifyValueLaxAt(List(), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c"))))
      tree3_1.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c"))))
      tree3_1.modifyValueLaxAt(List(97, 99), f, e) shouldBe Left(tree3_1)
      tree3_1.modifyValueLaxAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc"))))
      tree3_1.modifyValueLaxAt(List(97, 98, 100), f, e) shouldBe Left(tree3_1)
      tree3_2.modifyValueLaxAt(List(), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b"), Tree("c")))
      tree3_2.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb"), Tree("c")))
      tree3_2.modifyValueLaxAt(List(97, 99), f, e) shouldBe Right(Tree("a", Tree("b"), Tree("cc")))
      tree3_2.modifyValueLaxAt(List(97, 100), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List(97, 98, 99), f, e) shouldBe Left(tree3_2)
      tree3_2.modifyValueLaxAt(List(97, 98, 100), f, e) shouldBe Left(tree3_2)
      tree4_2.modifyValueLaxAt(List(), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List(97), f, e) shouldBe Right(Tree("aa", Tree("b", Tree("c")), Tree("d")))
      tree4_2.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List(97, 98), f, e) shouldBe Right(Tree("a", Tree("bb", Tree("c")), Tree("d")))
      tree4_2.modifyValueLaxAt(List(97, 99), f, e) shouldBe Left(tree4_2)
      tree4_2.modifyValueLaxAt(List(97, 100), f, e) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("dd")))
      tree4_2.modifyValueLaxAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueLaxAt(List(97, 98, 99), f, e) shouldBe Right(Tree("a", Tree("b", Tree("cc")), Tree("d")))
      tree4_2.modifyValueLaxAt(List(97, 98, 100), f, e) shouldBe Left(tree4_2)
      tree7.modifyValueLaxAt(List(), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97), f, e) shouldBe Right(
        Tree("aa", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 98), f, e) shouldBe Right(
        Tree("a", Tree("bb", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 98), fi("d"), e) shouldBe Right(
        Tree("a", Tree("d", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 98, 99), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("cc")), Tree("d", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 100), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("dd", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 100), fi("b"), e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("e", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 100, 101), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("ee", Tree("f"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 100, 101, 102), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("ff"))), Tree("g"))
      )
      tree7.modifyValueLaxAt(List(97, 103), f, e) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("gg"))
      )
      tree7.modifyValueLaxAt(List(98), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97, 99), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97, 103, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97, 100, 103), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97, 98, 99, 100), f, e) shouldBe Left(tree7)
      tree7.modifyValueLaxAt(List(97, 98, 101), f, e) shouldBe Left(tree7)
    }

    "modify lax a subtree selected by the path" in {
      tree0.modifyTreeLaxAt(List("a", "b"), _ => tree2) shouldBe Left(tree0)
      tree1.modifyTreeLaxAt(List("a"), _ => tree2) shouldBe Right(Tree("a", Tree("b")))
      tree1.modifyTreeLaxAt(List("b"), _ => tree2) shouldBe Left(tree1)
      tree2.modifyTreeLaxAt(List("a"), _ => tree3_2) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree2.modifyTreeLaxAt(List("a", "b"), _ => tree3_2) shouldBe Right(Tree("a", Tree("a", Tree("b"), Tree("c"))))
      tree2.modifyTreeLaxAt(List("a", "c"), _ => tree3_2) shouldBe Left(tree2)
      tree3_2.modifyTreeLaxAt(List("a", "b"), _ => tree2) shouldBe Right(Tree("a", Tree("a", Tree("b")), Tree("c")))
      tree3_2.modifyTreeLaxAt(List("a", "b"), _ => tree3_2) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("c"))
      )
      tree3_2.modifyTreeLaxAt(List("a", "c"), _ => tree2) shouldBe Right(Tree("a", Tree("b"), Tree("a", Tree("b"))))
      tree3_2.modifyTreeLaxAt(List("a", "c"), _ => tree3_2) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b"), Tree("c")))
      )
      tree3_2.modifyTreeLaxAt(List("a"), _ => tree2) shouldBe Right(tree2)
      tree3_2.modifyTreeLaxAt(List("a"), _ => tree9) shouldBe Right(tree9)
      tree3_2.modifyTreeLaxAt(List("a", "a"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List("b", "a"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List("b"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List("b", "b"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List("a", "c"), _ => Tree("b", Tree("d"))) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("d")))
      )
      tree4_2.modifyTreeLaxAt(List("a", "d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("c", Tree("e"))))
      )
      tree4_2.modifyTreeLaxAt(List("a", "b", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Right(
        Tree("a", Tree("b", Tree("b", Tree("c", Tree("e")))), Tree("d"))
      )
      tree4_2.modifyTreeLaxAt(List("a", "b", "d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("a", "d", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("a", "e"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("a", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("b"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List("b", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
    }

    "modify distinct a subtree selected by the path" in {
      tree0.modifyTreeAt(List("a", "b"), _ => tree2) shouldBe Left(tree0)
      tree1.modifyTreeAt(List("a"), _ => tree2) shouldBe Right(Tree("a", Tree("b")))
      tree1.modifyTreeAt(List("a"), _ => Tree("b")) shouldBe Right(Tree("b"))
      tree1.modifyTreeAt(List("b"), _ => tree2) shouldBe Left(tree1)
      tree2.modifyTreeAt(List("a"), _ => tree3_2) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree2.modifyTreeAt(List("a", "b"), _ => tree3_2) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")))
      )
      tree2.modifyTreeAt(List("a", "c"), _ => tree3_2) shouldBe Left(tree2)
      tree3_2.modifyTreeAt(List("a", "b"), _ => tree2) shouldBe Right(
        Tree("a", Tree("a", Tree("b")), Tree("c"))
      )
      tree3_2.modifyTreeAt(List("a", "b"), _ => tree3_2) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("c"))
      )
      tree3_2.modifyTreeAt(List("a", "c"), _ => tree2) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b")))
      )
      tree3_2.modifyTreeAt(List("a", "c"), _ => tree3_2) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b"), Tree("c")))
      )
      tree3_2.modifyTreeAt(List("a"), _ => tree2) shouldBe Right(tree2)
      tree3_2.modifyTreeAt(List("a"), _ => tree9) shouldBe Right(tree9)
      tree3_2.modifyTreeAt(List("a", "a"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List("b", "a"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List("b"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List("b", "b"), _ => tree2) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List("a", "c"), _ => Tree("b", Tree("d"))) shouldBe Right(
        Tree("a", Tree("b", Tree("d")))
      )
      tree4_2.modifyTreeAt(List("a", "d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("e"))))
      )
      tree4_2.modifyTreeAt(List("a", "b", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Right(
        Tree("a", Tree("b", Tree("b", Tree("c", Tree("e")))), Tree("d"))
      )
      tree4_2.modifyTreeAt(List("a", "b", "d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("a", "d", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("a", "e"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("a", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("b"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("d"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List("b", "c"), _ => Tree("b", Tree("c", Tree("e")))) shouldBe Left(tree4_2)
      // check if de-duplicates only the modified node
      tree3_2
        .insertLeafLax("b")
        .modifyTreeAt(List("a", "c"), _ => Tree("b", Tree("c"))) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("c")))
      )
    }

    "modify distinct a subtree selected by the path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.modifyTreeAt(List(97, 98), _ => tree2, codeF) shouldBe Left(tree0)
      tree1.modifyTreeAt(List(97), _ => tree2, codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.modifyTreeAt(List(98), _ => tree2, codeF) shouldBe Left(tree1)
      tree2.modifyTreeAt(List(97), _ => tree3_2, codeF) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree2.modifyTreeAt(List(97, 98), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")))
      )
      tree2.modifyTreeAt(List(97, 99), _ => tree3_2, codeF) shouldBe Left(tree2)
      tree3_2.modifyTreeAt(List(97, 98), _ => tree2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b")), Tree("c"))
      )
      tree3_2.modifyTreeAt(List(97, 98), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("c"))
      )
      tree3_2.modifyTreeAt(List(97, 99), _ => tree2, codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b")))
      )
      tree3_2.modifyTreeAt(List(97, 99), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b"), Tree("c")))
      )
      tree3_2.modifyTreeAt(List(97), _ => tree2, codeF) shouldBe Right(tree2)
      tree3_2.modifyTreeAt(List(97), _ => tree9, codeF) shouldBe Right(tree9)
      tree3_2.modifyTreeAt(List(97, 97), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List(98, 97), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List(98), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List(98, 98), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeAt(List(97, 99), _ => Tree("b", Tree("d")), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("d")))
      )
      tree4_2.modifyTreeAt(List(97, 100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("e"))))
      )
      tree4_2.modifyTreeAt(List(97, 98, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("b", Tree("c", Tree("e")))), Tree("d"))
      )
      tree4_2.modifyTreeAt(List(97, 98, 100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(97, 100, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(97, 101), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(97, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(98), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeAt(List(98, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
    }

    "modify lax a subtree selected by the path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.modifyTreeLaxAt(List(97, 98), _ => tree2, codeF) shouldBe Left(tree0)
      tree1.modifyTreeLaxAt(List(97), _ => tree2, codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.modifyTreeLaxAt(List(98), _ => tree2, codeF) shouldBe Left(tree1)
      tree2.modifyTreeLaxAt(List(97), _ => tree3_2, codeF) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree2.modifyTreeLaxAt(List(97, 98), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")))
      )
      tree2.modifyTreeLaxAt(List(97, 99), _ => tree3_2, codeF) shouldBe Left(tree2)
      tree3_2.modifyTreeLaxAt(List(97, 98), _ => tree2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b")), Tree("c"))
      )
      tree3_2.modifyTreeLaxAt(List(97, 98), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("c"))
      )
      tree3_2.modifyTreeLaxAt(List(97, 99), _ => tree2, codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b")))
      )
      tree3_2.modifyTreeLaxAt(List(97, 99), _ => tree3_2, codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("a", Tree("b"), Tree("c")))
      )
      tree3_2.modifyTreeLaxAt(List(97), _ => tree2, codeF) shouldBe Right(tree2)
      tree3_2.modifyTreeLaxAt(List(97), _ => tree9, codeF) shouldBe Right(tree9)
      tree3_2.modifyTreeLaxAt(List(97, 97), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List(98, 97), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List(98), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List(98, 98), _ => tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.modifyTreeLaxAt(List(97, 99), _ => Tree("b", Tree("d")), codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("d")))
      )
      tree4_2.modifyTreeLaxAt(List(97, 100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("b", Tree("c", Tree("e"))))
      )
      tree4_2.modifyTreeLaxAt(List(97, 98, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("b", Tree("c", Tree("e")))), Tree("d"))
      )
      tree4_2.modifyTreeLaxAt(List(97, 98, 100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(97, 100, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(97, 101), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(97, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(98), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(100), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
      tree4_2.modifyTreeLaxAt(List(98, 99), _ => Tree("b", Tree("c", Tree("e"))), codeF) shouldBe Left(tree4_2)
    }

  }

}
