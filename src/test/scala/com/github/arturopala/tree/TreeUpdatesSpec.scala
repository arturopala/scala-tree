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

  }

}
