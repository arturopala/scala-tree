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

import com.github.arturopala.tree.internal.NodeTree._

class NodeTreeSpec extends AnyWordSpecCompat {

  def all[T]: T => Boolean = _ => true
  def none[T]: T => Boolean = _ => false
  val even: String => Boolean = s => s.head.toInt % 2 == 0
  val odd: String => Boolean = s => s.head.toInt  % 2 != 0

  s"NodeTree" should {

    "insert new child distinct between siblings" in {
      insertDistinctBetweenSiblings(List(), Tree("a"), List(), false) shouldBe
        (List(Tree("a")), List())
      insertDistinctBetweenSiblings(List(Tree("a")), Tree("b"), List(Tree("c")), false) shouldBe
        (List(Tree("a"), Tree("b")), List(Tree("c")))
      insertDistinctBetweenSiblings(List(Tree("a", Tree("b"))), Tree("a", Tree("c")), List(Tree("a", Tree("d"))), false) shouldBe
        (List(Tree("a", Tree("b"), Tree("c"))), List(Tree("a", Tree("d"))))
      insertDistinctBetweenSiblings(List(), Tree("c"), List(Tree("c"), Tree("e")), false) shouldBe
        (List(Tree("c")), List(Tree("e")))
    }

    "insert children distinct" in {
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c"), Tree("c"), Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct("a", List(Tree("b")), List(Tree("c"), Tree("e"), Tree("c")), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("e"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("c"), Tree("d")),
        List(Tree("c"), Tree("e")),
        List(Tree("c"), Tree("f")),
        false
      ) shouldBe
        Tree("a", Tree("c"), Tree("d"), Tree("e"), Tree("c"), Tree("f"))
      insertChildrenDistinct(
        "a",
        List(Tree("c"), Tree("e")),
        List(Tree("c"), Tree("d")),
        List(Tree("c"), Tree("f")),
        false
      ) shouldBe
        Tree("a", Tree("c"), Tree("e"), Tree("d"), Tree("c"), Tree("f"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("y"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("y"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("c"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b"), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("a"), Tree("z")),
        List(Tree("b"), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"), Tree("x"), Tree("a"), Tree("z"), Tree("b"), Tree("c"), Tree("d"))
      insertChildrenDistinct(
        "a",
        List(Tree("b", Tree("e")), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("b", Tree("f")), Tree("z")),
        List(Tree("b", Tree("g")), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree(
          "a",
          Tree("b", Tree("e"), Tree("f")),
          Tree("c"),
          Tree("d"),
          Tree("x"),
          Tree("z"),
          Tree("b", Tree("g")),
          Tree("c"),
          Tree("d")
        )
      insertChildrenDistinct(
        "a",
        List(Tree("b", Tree("e")), Tree("c"), Tree("d")),
        List(Tree("x"), Tree("b", Tree("f")), Tree("b", Tree("g"))),
        List(Tree("b", Tree("h")), Tree("c"), Tree("d")),
        false
      ) shouldBe
        Tree(
          "a",
          Tree("b", Tree("e"), Tree("f"), Tree("g")),
          Tree("c"),
          Tree("d"),
          Tree("x"),
          Tree("b", Tree("h")),
          Tree("c"),
          Tree("d")
        )
    }

    "insert child distinct" in {
      insertChildDistinct("a", List(Tree("b")), Tree("c"), List(Tree("d")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      insertChildDistinct("a", List(Tree("b")), Tree("c"), List(Tree("c")), false) shouldBe
        Tree("a", Tree("b"), Tree("c"))
      insertChildDistinct("a", List(Tree("c")), Tree("c"), List(Tree("d")), false) shouldBe
        Tree("a", Tree("c"), Tree("d"))
      insertChildDistinct("a", List(Tree("b")), Tree("c", Tree("e")), List(Tree("d"), Tree("c", Tree("f"))), false) shouldBe
        Tree("a", Tree("b"), Tree("c", Tree("e"), Tree("f")), Tree("d"))
      insertChildDistinct(
        "a",
        List(Tree("b")),
        Tree("c", Tree("e")),
        List(Tree("d"), Tree("c", Tree("f")), Tree("c", Tree("g"))),
        false
      ) shouldBe
        Tree("a", Tree("b"), Tree("c", Tree("e"), Tree("f")), Tree("d"), Tree("c", Tree("g")))
      insertChildDistinct(
        "a",
        List(Tree("c", Tree("e")), Tree("b")),
        Tree("c", Tree("f")),
        List(Tree("d"), Tree("c", Tree("g"))),
        false
      ) shouldBe
        Tree("a", Tree("c", Tree("e"), Tree("f")), Tree("b"), Tree("d"), Tree("c", Tree("g")))
      insertChildDistinct(
        "a",
        List(Tree("c", Tree("e")), Tree("c", Tree("f")), Tree("b")),
        Tree("c", Tree("g")),
        List(Tree("d"), Tree("c", Tree("h"))),
        false
      ) shouldBe
        Tree("a", Tree("c", Tree("e")), Tree("c", Tree("f"), Tree("g")), Tree("b"), Tree("d"), Tree("c", Tree("h")))
    }

    "ensure child is distinct" in {
      ensureChildDistinct(Tree("a"), 0, false) shouldBe Tree("a")
      ensureChildDistinct(Tree("a"), 1, false) shouldBe Tree("a")
      ensureChildDistinct(Tree("a", Tree("b")), 0, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b")), 1, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 0, false) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("c")), 1, false) shouldBe Tree("a", Tree("b"), Tree("c"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 0, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 1, false) shouldBe Tree("a", Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b")), 2, false) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b"), Tree("b"), Tree("b")), 2, false) shouldBe Tree("a", Tree("b"), Tree("b"))
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 2, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c")),
        Tree("b", Tree("d"), Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 1, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d")),
        Tree("b", Tree("e"))
      )
      ensureChildDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")), Tree("b", Tree("e"))), 0, false) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d")),
        Tree("b", Tree("e"))
      )
    }

    "make tree distinct" in {
      makeTreeDistinct(Tree("a")) shouldBe Tree("a")
      makeTreeDistinct(Tree("a", Tree("b"))) shouldBe Tree("a", Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")))) shouldBe Tree("a", Tree("b", Tree("c"), Tree("d")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")))) shouldBe Tree("a", Tree("b", Tree("c"), Tree("d")))
      makeTreeDistinct(Tree("a", Tree("b"), Tree("b"))) shouldBe Tree("a", Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("d")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d"))
      )
      makeTreeDistinct(Tree("a", Tree("b", Tree("c")), Tree("b", Tree("c")))) shouldBe Tree("a", Tree("b", Tree("c")))
      makeTreeDistinct(Tree("a", Tree("b", Tree("c"), Tree("d")), Tree("b", Tree("c"), Tree("e")))) shouldBe Tree(
        "a",
        Tree("b", Tree("c"), Tree("d"), Tree("e"))
      )
      makeTreeDistinct(Tree("a", Tree("b", Tree("e"), Tree("c")), Tree("b", Tree("c"), Tree("e")))) shouldBe Tree(
        "a",
        Tree("b", Tree("e"), Tree("c"))
      )
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("g")))
        )
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("j")), Tree("e", Tree("g")))
        )
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("i"), Tree("j"))))
      makeTreeDistinct(Tree("a", Tree("c"), Tree("b"), Tree("c"))) shouldBe Tree("a", Tree("c"), Tree("b"))
      makeTreeDistinct(Tree("a", Tree("b"), Tree("c"), Tree("b"))) shouldBe Tree("a", Tree("b"), Tree("c"))
      makeTreeDistinct(
        Tree("a", Tree("b", Tree("c", Tree("d", Tree("e", Tree("f"), Tree("g"), Tree("f"), Tree("g"))))))
      ) shouldBe Tree("a", Tree("b", Tree("c", Tree("d", Tree("e", Tree("f"), Tree("g"))))))
    }

    "make tree distinct up to specified level" in {
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 0
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 1
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 2
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))),
          Tree("b", Tree("c", Tree("i")), Tree("e", Tree("f")))
        ),
        maxLookupLevel = 3
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f")), Tree("c", Tree("i"))))
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
          Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
        ),
        maxLookupLevel = 1
      ) shouldBe Tree(
        "a",
        Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
        Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
      )
      makeTreeDistinct(
        Tree(
          "a",
          Tree("b", Tree("e", Tree("f")), Tree("e", Tree("g"))),
          Tree("c", Tree("d", Tree("i")), Tree("d", Tree("j")))
        ),
        maxLookupLevel = 2
      ) shouldBe Tree("a", Tree("b", Tree("e", Tree("f"), Tree("g"))), Tree("c", Tree("d", Tree("i"), Tree("j"))))
    }

    "build tree from triples list" in {
      buildTreeFromPartials(List(), Nil) shouldBe Nil
      buildTreeFromPartials(List((0, "a", Nil)), Nil) shouldBe List(Tree("a"))
      buildTreeFromPartials(List((0, "a", List(Tree("b")))), Nil) shouldBe List(Tree("a", Tree("b")))
      buildTreeFromPartials(List((0, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"))
      )
      buildTreeFromPartials(List((0, "d", Nil), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d"))
      )
      buildTreeFromPartials(List((0, "d", List(Tree("e", Tree("f")))), (1, "a", List(Tree("b"), Tree("c")))), Nil) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f"))))
      )
      buildTreeFromPartials(
        List((0, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"))))
      buildTreeFromPartials(
        List((0, "h", Nil), (1, "d", List(Tree("e", Tree("f")), Tree("g"))), (1, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g"), Tree("h"))))
      buildTreeFromPartials(
        List((0, "h", Nil), (0, "d", List(Tree("e", Tree("f")), Tree("g"))), (2, "a", List(Tree("b"), Tree("c")))),
        Nil
      ) shouldBe List(Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g")), Tree("h")))
      buildTreeFromPartials(
        List(
          (0, "h", List(Tree("i"), Tree("j"))),
          (0, "d", List(Tree("e", Tree("f")), Tree("g"))),
          (2, "a", List(Tree("b"), Tree("c")))
        ),
        Nil
      ) shouldBe List(
        Tree("a", Tree("b"), Tree("c"), Tree("d", Tree("e", Tree("f")), Tree("g")), Tree("h", Tree("i"), Tree("j")))
      )
    }
  }
}
