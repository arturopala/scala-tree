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

class TreeInsertionsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    def tree[T: ClassTag](t: Tree[T]): Tree[T]

    "insert distinct new value to a tree" in {
      tree0.insertValue("a") shouldBe Tree("a")
      tree1.insertValue("b") shouldBe Tree("a", Tree("b"))
      tree2.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree2.insertValue("b") shouldBe tree2
      tree3_1.insertValue("b") shouldBe tree3_1
      tree3_1.insertValue("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree4_1.insertValue("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertValue("e") shouldBe Tree(
        "a",
        Tree("e"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      tree2.insertValue("b") shouldBe tree2
      tree3_1.insertValue("b") shouldBe tree3_1
      tree3_2.insertValue("b") shouldBe tree3_2
      tree3_2.insertValue("c") shouldBe tree3_2
      tree4_1.insertValue("b") shouldBe tree4_1
      tree4_2.insertValue("b") shouldBe tree4_2
      tree4_2.insertValue("d") shouldBe tree4_2
      tree4_3.insertValue("b") shouldBe tree4_3
      tree7.insertValue("b") shouldBe tree7
      tree7.insertValue("d") shouldBe tree7
      tree7.insertValue("g") shouldBe tree7
      tree9.insertValue("b") shouldBe tree9
      tree9.insertValue("e") shouldBe tree9
    }

    "insert lax new value to a tree" in {
      tree0.insertValueLax("a") shouldBe Tree("a")
      tree1.insertValueLax("b") shouldBe Tree("a", Tree("b"))
      tree2.insertValueLax("c") shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertValueLax("d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertValueLax("b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertValueLax("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertValueLax("c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree4_1.insertValueLax("c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c", Tree("d"))))
      tree7.insertValueLax("b") shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertValueLax("d") shouldBe Tree(
        "a",
        Tree("d"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
    }

    "insert lax new value to a tree at the specified path" in {
      tree0.insertValueLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueLaxAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))))
      tree2.insertValueLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertValueLaxAt(List("a", "c"), "a") shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"))
      tree2.insertValueLaxAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree2.insertValueLaxAt(List("a", "c", "c"), "a") shouldBe Tree("a", Tree("c", Tree("c", Tree("a"))), Tree("b"))
      tree3_1
        .insertValueLaxAt(List("a", "b", "c", "d"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("d", Tree("a")))))
      tree3_1.insertValueLaxAt(List("a", "b", "c"), "a") shouldBe Tree("a", Tree("b", Tree("c", Tree("a"))))
      tree3_1.insertValueLaxAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertValueLaxAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertValueLaxAt(List("a"), "b") shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertValueLaxAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertValueLaxAt(List("a"), "c") shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2.insertValueLaxAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"), Tree("c"))
      tree3_2.insertValueLaxAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree3_2.insertValueLaxAt(List("a", "c"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("c")))
      tree3_2.insertValueLaxAt(List("a", "c", "d"), "c") shouldBe Tree("a", Tree("b"), Tree("c", Tree("d", Tree("c"))))
    }

    "insert distinct new value to a tree at the specified path" in {
      tree0.insertValueAt(List(), "a") shouldBe Tree("a")
      tree0.insertValueAt(List("a", "b"), "a") shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertValueAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree1.insertValueAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertValueAt(List("a"), "a") shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertValueAt(List("a"), "b") shouldBe Tree("a", Tree("b"))
      tree2.insertValueAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")))
      tree2.insertValueAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")))
      tree2.insertValueAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertValueAt(List("a", "b", "c"), "d") shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertValueAt(List("a", "b"), "d") shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertValueAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_2.insertValueAt(List("a"), "d") shouldBe Tree("a", Tree("d"), Tree("b"), Tree("c"))
      tree3_2.insertValueAt(List("a"), "b") shouldBe tree3_2
      tree3_2.insertValueAt(List("a"), "c") shouldBe tree3_2
      tree3_2.insertValueAt(List("a", "b"), "b") shouldBe Tree("a", Tree("b", Tree("b")), Tree("c"))
      tree3_2.insertValueAt(List("a", "b"), "c") shouldBe Tree("a", Tree("b", Tree("c")), Tree("c"))
      tree7.insertValueAt(List("a", "b"), "c") shouldBe tree7
      tree7.insertValueAt(List("a", "b"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertValueAt(List("a", "b", "c"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertValueAt(List("a", "d", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("d"), Tree("f"))),
        Tree("g")
      )
      tree7.insertValueAt(List("a", "g", "e"), "d") shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("d")))
      )
    }

    "insert lax new value to a tree at the specified path with path item extractor" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertValueLaxAt(List(97), "a", codeF) shouldBe Left(tree0)
      tree1.insertValueLaxAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertValueLaxAt(List(98), "b", codeF) shouldBe Left(tree1)
      tree1.insertValueLaxAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a")))
      tree2.insertValueLaxAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertValueLaxAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertValueLaxAt(List(98), "a", codeF) shouldBe Left(tree2)
      tree2.insertValueLaxAt(List(97, 98), "a", codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertValueLaxAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertValueLaxAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertValueLaxAt(List(97, 97), "c", codeF) shouldBe Left(tree2)
      tree2.insertValueLaxAt(List(98, 97), "c", codeF) shouldBe Left(tree2)
    }

    "insert distinct new value to a tree at the specified path with path item extractor" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertValueAt(List(), "a", codeF) shouldBe Left(tree0)
      tree0.insertValueAt(List(97, 98), "a", codeF) shouldBe Left(tree0)
      tree1.insertValueAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertValueAt(List(97, 98), "c", codeF) shouldBe Left(tree1)
      tree2.insertValueAt(List(97), "a", codeF) shouldBe Right(Tree("a", Tree("a"), Tree("b")))
      tree2.insertValueAt(List(97), "b", codeF) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertValueAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b"))))
      tree2.insertValueAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree2.insertValueAt(List(97, 98, 99), "d", codeF) shouldBe Left(tree2)
      tree3_1.insertValueAt(List(97, 98, 99), "d", codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("d"))))
      )
      tree3_1.insertValueAt(List(97, 98), "d", codeF) shouldBe Right(Tree("a", Tree("b", Tree("d"), Tree("c"))))
      tree3_1.insertValueAt(List(97), "d", codeF) shouldBe Right(Tree("a", Tree("d"), Tree("b", Tree("c"))))
      tree3_2.insertValueAt(List(97), "d", codeF) shouldBe Right(Tree("a", Tree("d"), Tree("b"), Tree("c")))
      tree3_2.insertValueAt(List(97), "b", codeF) shouldBe Right(tree3_2)
      tree3_2.insertValueAt(List(97), "c", codeF) shouldBe Right(tree3_2)
      tree3_2.insertValueAt(List(97, 98), "b", codeF) shouldBe Right(Tree("a", Tree("b", Tree("b")), Tree("c")))
      tree3_2.insertValueAt(List(97, 98), "c", codeF) shouldBe Right(Tree("a", Tree("b", Tree("c")), Tree("c")))
      tree7.insertValueAt(List(97, 98), "c", codeF) shouldBe Right(tree7)
      tree7.insertValueAt(List(97, 98), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("d"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueAt(List(97, 98, 99), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("d"))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueAt(List(97, 100, 101), "d", codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("d"), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertValueAt(List(97, 103, 101), "d", codeF) shouldBe Left(tree7)
    }

    "insert lax new subtree to a tree" in {
      tree0.insertTreeLax(Tree("a")) shouldBe Tree("a")
      tree1.insertTreeLax(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTreeLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"))
      tree3_1.insertTreeLax(Tree("d")) shouldBe Tree("a", Tree("d"), Tree("b", Tree("c")))
      tree3_1.insertTreeLax(Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTreeLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b", Tree("c")))
      tree3_2.insertTreeLax(Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))

      tree1.insertTreeLax(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree1.insertTreeLax(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")))
      tree1.insertTreeLax(Tree("a", Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")))

      tree2.insertTreeLax(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")), Tree("b"))
      tree2.insertTreeLax(tree2) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree3_1.insertTreeLax(tree3_1) shouldBe Tree("a", Tree("a", Tree("b", Tree("c"))), Tree("b", Tree("c")))
      tree3_2.insertTreeLax(tree3_2) shouldBe Tree("a", Tree("a", Tree("b"), Tree("c")), Tree("b"), Tree("c"))
      tree4_1.insertTreeLax(tree4_1) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_2.insertTreeLax(tree4_2) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("b", Tree("c")),
        Tree("d")
      )
      tree4_3.insertTreeLax(tree4_3) shouldBe Tree(
        "a",
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree7.insertTreeLax(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeLax(tree7)
        .insertTreeLax(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )

      allTrees.foldLeft(tree0)((acc, tree) => tree.insertTreeLax(acc)) shouldBe Tree(
        "a",
        Tree(
          "a",
          Tree(
            "a",
            Tree(
              "a",
              Tree(
                "a",
                Tree("a", Tree("a", Tree("a", Tree("a"), Tree("b")), Tree("b", Tree("c"))), Tree("b"), Tree("c")),
                Tree("b", Tree("c", Tree("d")))
              ),
              Tree("b", Tree("c")),
              Tree("d")
            ),
            Tree("b"),
            Tree("c"),
            Tree("d")
          ),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        ),
        Tree("b", Tree("c", Tree("d"))),
        Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))
      )

      allTrees.foldLeft(tree0)((acc, tree) => acc.insertTreeLax(tree)) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("e", Tree("f", Tree("g")), Tree("h", Tree("i")))),
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("a", Tree("b"), Tree("c"), Tree("d")),
        Tree("a", Tree("b", Tree("c")), Tree("d")),
        Tree("a", Tree("b", Tree("c", Tree("d")))),
        Tree("a", Tree("b"), Tree("c")),
        Tree("a", Tree("b", Tree("c"))),
        Tree("a", Tree("b"))
      )

      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .insertTreeLax(Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x")), Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "insert distinct new subtree to a tree" in {
      tree0.insertTree(Tree.empty) shouldBe Tree.empty
      tree0.insertTree(Tree("a")) shouldBe Tree("a")
      tree0.insertTree(tree9) shouldBe tree9
      tree1.insertTree(Tree("a")) shouldBe Tree("a", Tree("a"))
      tree1.insertTree(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTree(Tree.empty) shouldBe tree1
      tree2.insertTree(Tree("a")) shouldBe Tree("a", Tree("a"), Tree("b"))
      tree2.insertTree(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("a", Tree("b")), Tree("b"))
      tree2.insertTree(Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTree(Tree("b", Tree("c"))) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTree(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d"), Tree("c")))
      tree3_1.insertTree(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e"), Tree("c"))
      )
      tree3_2.insertTree(Tree("b", Tree("d"))) shouldBe Tree("a", Tree("b", Tree("d")), Tree("c"))
      tree3_2.insertTree(Tree("b", Tree("d"), Tree("e"))) shouldBe Tree(
        "a",
        Tree("b", Tree("d"), Tree("e")),
        Tree("c")
      )
      tree4_2.insertTree(Tree("b", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d")
      )
      tree4_2.insertTree(Tree("d", Tree("c", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d")))
      )
      tree7.insertTree(Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("c", Tree("d"), Tree("e"), Tree("f")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTree(tree7)
        .insertTree(tree7) shouldBe Tree(
        "a",
        Tree("a", Tree("b", Tree("c")), Tree("d", Tree("e", Tree("f"))), Tree("g")),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))))
        .insertTree(Tree("b", Tree("x"))) shouldBe
        Tree("a", Tree("b", Tree("x"), Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e")))
    }

    "insert lax new subtree to a tree at the specified path" in {
      tree0.insertTreeLaxAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeLaxAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeLaxAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"))
      tree2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")))
      tree3_1.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertTreeLaxAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c"), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b"), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("c"), Tree("b"), Tree("c"))
      tree3_2
        .insertTreeLaxAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("c", Tree("a")), Tree("b"), Tree("c"))
      tree3_2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d")
      )
      tree4_2.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertTreeLaxAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertTreeLaxAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeLaxAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeLaxAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertTreeLaxAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")), Tree("f"))),
        Tree("g")
      )
    }

    "insert lax new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertTreeLaxAt(List(), Tree.empty, codeF) shouldBe Right(Tree.empty)
      tree0.insertTreeLaxAt(List(97), Tree.empty, codeF) shouldBe Left(Tree.empty)
      tree0.insertTreeLaxAt(List(), Tree("b"), codeF) shouldBe Right(Tree("b"))
      tree0.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Left(Tree.empty)
      tree1.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree1)

      tree2.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b")))
      tree2.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree2)
      tree2.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a", Tree("b")))))
      tree2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree2)
      tree2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree2)

      tree3_1.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b", Tree("c"))))
      tree3_1.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_1)
      tree3_1.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"), Tree("c"))))
      tree3_1.insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("b"), Tree("c")))
      tree3_2.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a")), Tree("c")))
      tree3_2.insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree3_2)

      tree4_2.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree("a", Tree("b"), Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree4_2)
      tree4_2.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertTreeLaxAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b"),
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(98), Tree("b"), codeF) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertTreeLaxAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 98, 101), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 103, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeLaxAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 100, 101), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeLaxAt(List(97, 103), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g", Tree("a", Tree("b"), Tree("c")))
        )
      )
    }

    "insert distinct new subtree to a tree at the specified path" in {
      tree0.insertTreeAt(List(), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeAt(List("a"), Tree.empty) shouldBe Tree.empty
      tree0.insertTreeAt(List(), Tree("b")) shouldBe Tree("b")
      tree0.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree1.insertTreeAt(List("b"), Tree("b")) shouldBe tree1
      tree1.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree1.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree1
        .insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree1.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"))
      tree2.insertTreeAt(List("b"), Tree("b")) shouldBe tree2
      tree2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")))
      tree2.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))))
      tree2
        .insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_1.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTreeAt(List("b"), Tree("b")) shouldBe tree3_1
      tree3_1.insertTreeAt(List("a", "b"), Tree("c")) shouldBe Tree("a", Tree("b", Tree("c")))
      tree3_1.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")))
      tree3_1.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      tree3_1
        .insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      tree3_1.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
      )

      tree3_2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2.insertTreeAt(List("a"), Tree("c")) shouldBe Tree("a", Tree("b"), Tree("c"))
      tree3_2
        .insertTreeAt(List("a"), Tree("c", Tree("a"))) shouldBe Tree("a", Tree("b"), Tree("c", Tree("a")))
      tree3_2.insertTreeAt(List("b"), Tree("b")) shouldBe tree3_2
      tree3_2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a")), Tree("c"))
      tree3_2.insertTreeAt(List("a", "b"), tree2) shouldBe Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      tree3_2.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("c")
      )
      tree3_2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("c")
      )

      tree4_2.insertTreeAt(List("a"), Tree("b")) shouldBe Tree("a", Tree("b", Tree("c")), Tree("d"))
      tree4_2.insertTreeAt(List("b"), Tree("b")) shouldBe tree4_2
      tree4_2.insertTreeAt(List("a", "b"), tree1) shouldBe Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      tree4_2
        .insertTreeAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d")
      )
      tree4_2.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d")
      )
      tree4_2.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d")
      )

      tree7.insertTreeAt(List("a"), Tree("b")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("b"), Tree("b")) shouldBe tree7
      tree7.insertTreeAt(List("a", "b"), tree1) shouldBe Tree(
        "a",
        Tree("b", Tree("a"), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7
        .insertTreeAt(List("a", "b"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("a", Tree("b")), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "c"), tree2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "c"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "e"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("a", Tree("b"), Tree("c"))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "b", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))), Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertTreeAt(List("a", "g", "e", "f"), tree3_2) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("e", Tree("f", Tree("a", Tree("b"), Tree("c")))))
      )
      tree7.insertTreeAt(List("a", "d", "e"), Tree("f", Tree("i"))) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f", Tree("i")))),
        Tree("g")
      )
    }

    "insert distinct new subtree to a tree at the specified path using an extractor function" in {
      val codeF: String => Int = s => s.head.toInt
      tree0.insertTreeAt(List(), Tree.empty, codeF) shouldBe Right(Tree.empty)
      tree0.insertTreeAt(List(97), Tree.empty, codeF) shouldBe Left(Tree.empty)
      tree0.insertTreeAt(List(), Tree("b"), codeF) shouldBe Right(Tree("b"))
      tree0.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Left(Tree.empty)
      tree1.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree1.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree1)
      tree1.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree1)

      tree2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b")))
      tree2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree2)
      tree2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a"))))
      tree2.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(Tree("a", Tree("b", Tree("a", Tree("b")))))
      tree2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree2)
      tree2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree2)

      tree3_1.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b", Tree("c"))))
      tree3_1.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_1)
      tree3_1.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")))
      )
      tree3_1.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b")), Tree("c")))
      )
      tree3_1
        .insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c", Tree("a", Tree("b")))))
      )
      tree3_1.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c"))))
        )
      )

      tree3_2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(Tree("a", Tree("b"), Tree("c")))
      tree3_2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a")), Tree("c"))
      )
      tree3_2.insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a", Tree("b"))), Tree("c"))
      )
      tree3_2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Left(tree3_2)
      tree3_2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Left(tree3_2)

      tree4_2.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("c")), Tree("d"))
      )
      tree4_2.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree4_2)
      tree4_2.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree("a", Tree("b", Tree("a"), Tree("c")), Tree("d"))
      )
      tree4_2
        .insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d")
        )
      )
      tree4_2.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d")
        )
      )
      tree4_2.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d")
        )
      )

      tree7.insertTreeAt(List(97), Tree("b"), codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(98), Tree("b"), codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98), tree1, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a"), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7
        .insertTreeAt(List(97, 98), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("a", Tree("b")), Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 99), tree2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 98, 101), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 103, 101, 102), tree3_2, codeF) shouldBe Left(tree7)
      tree7.insertTreeAt(List(97, 98, 99), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c", Tree("a", Tree("b"), Tree("c")))),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 100, 101), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("a", Tree("b"), Tree("c")), Tree("f"))),
          Tree("g")
        )
      )
      tree7.insertTreeAt(List(97, 103), tree3_2, codeF) shouldBe Right(
        Tree(
          "a",
          Tree("b", Tree("c")),
          Tree("d", Tree("e", Tree("f"))),
          Tree("g", Tree("a", Tree("b"), Tree("c")))
        )
      )
    }

    "insert new branch to a tree" in {
      tree0.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree1.insertBranch(List("a", "c", "c", "d")) shouldBe Tree("a", Tree("c", Tree("c", Tree("d"))))
      tree1.insertBranch(List("b", "c", "d")) shouldBe Tree("a")
      tree2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree2.insertBranch(List("a", "c", "c", "d")) shouldBe Tree("a", Tree("c", Tree("c", Tree("d"))), Tree("b"))
      tree2
        .insertBranch(List("a", "b", "c", "d"))
        .insertBranch(List("a", "b", "e", "f")) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f")), Tree("c", Tree("d")))
      )
      tree3_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree3_1.insertBranch(List("a", "b", "e", "f")) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c")))
      tree3_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("c"))
      tree4_1.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))))
      tree4_2.insertBranch(List("a", "b", "c", "d")) shouldBe Tree("a", Tree("b", Tree("c", Tree("d"))), Tree("d"))
      tree4_3.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("c"),
        Tree("d")
      )
      tree7.insertBranch(List("a", "b", "c", "d")) shouldBe Tree(
        "a",
        Tree("b", Tree("c", Tree("d"))),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "g", "h", "i")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("f"))),
        Tree("g", Tree("h", Tree("i")))
      )
      tree7.insertBranch(List("a", "d", "g", "h")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("g", Tree("h")), Tree("e", Tree("f"))),
        Tree("g")
      )
      tree7.insertBranch(List("a", "d", "e", "g")) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("d", Tree("e", Tree("g"), Tree("f"))),
        Tree("g")
      )
    }

  }

}
