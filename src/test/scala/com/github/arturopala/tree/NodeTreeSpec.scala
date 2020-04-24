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

import com.github.arturopala.tree.util.NodeTree._

class NodeTreeSpec extends AnyWordSpecCompat {

  import TestTrees._

  def all[T]: T => Boolean = _ => true
  def none[T]: T => Boolean = _ => false
  val even: String => Boolean = s => s.head.toInt % 2 == 0
  val odd: String => Boolean = s => s.head.toInt  % 2 != 0

  s"NodeTree" should {

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

    "merge distinct the tree with new subtrees" in {
      mergeDistinct(Vector(), Nil) shouldBe Tree.empty
      mergeDistinct(Vector((Tree("a"), Nil)), Nil) shouldBe Tree("a")
      mergeDistinct(Vector((Tree("a", Tree("b")), Nil)), Nil) shouldBe Tree("a", Tree("b"))
      mergeDistinct(Vector((Tree("a"), List(Tree("b")))), Nil) shouldBe Tree("a", Tree("b"))
      mergeDistinct(Vector((Tree("a", Tree("c")), List(Tree("b")))), Nil) shouldBe Tree("a", Tree("b"), Tree("c"))
      mergeDistinct(Vector((Tree("a", Tree("c")), List(Tree("c")))), Nil) shouldBe Tree("a", Tree("c"))
      mergeDistinct(Vector((Tree("a", Tree("c")), List(Tree("c", Tree("d"))))), Nil) shouldBe Tree(
        "a",
        Tree("c", Tree("d"))
      )
      mergeDistinct(Vector((Tree("a", Tree("c"), Tree("d")), List(Tree("c", Tree("d"))))), Nil) shouldBe Tree(
        "a",
        Tree("c", Tree("d")),
        Tree("d")
      )
      mergeDistinct(
        Vector((Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c"), Tree("d")), List(Tree("c", Tree("d"))))),
        Nil
      ) shouldBe Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("d")), Tree("d"))
      mergeDistinct(
        Vector((Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c"), Tree("d")), List(Tree("e", Tree("g"))))),
        Nil
      ) shouldBe Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c"), Tree("d"))
      mergeDistinct(
        Vector((Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c"), Tree("d")), List(Tree("e", Tree("h"))))),
        Nil
      ) shouldBe Tree("a", Tree("e", Tree("h"), Tree("f"), Tree("g")), Tree("c"), Tree("d"))
      mergeDistinct(
        Vector(
          (Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("h")), Tree("d")), List(Tree("c", Tree("d"))))
        ),
        Nil
      ) shouldBe Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("d"), Tree("h")), Tree("d"))
      mergeDistinct(
        Vector(
          (Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("d")), Tree("d")), List(Tree("c", Tree("d"))))
        ),
        Nil
      ) shouldBe Tree("a", Tree("e", Tree("f"), Tree("g")), Tree("c", Tree("d")), Tree("d"))
      mergeDistinct(Vector((Tree("a", Tree("b", Tree("c"))), List(Tree("b"), Tree("c"), Tree("d")))), Nil) shouldBe Tree(
        "a",
        Tree("c"),
        Tree("d"),
        Tree("b", Tree("c"))
      )
      mergeDistinct(Vector((Tree("a", Tree("b"), Tree("c"), Tree("d")), List(Tree("b"), Tree("c"), Tree("d")))), Nil) shouldBe Tree(
        "a",
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      mergeDistinct(
        Vector(
          (
            Tree("a", Tree("b"), Tree("c"), Tree("d")),
            List(Tree("b", Tree("e")), Tree("c", Tree("f")), Tree("d", Tree("g")))
          )
        ),
        Nil
      ) shouldBe Tree("a", Tree("b", Tree("e")), Tree("c", Tree("f")), Tree("d", Tree("g")))
    }

    "stream all values" in {
      valueStream(all, tree1).toList shouldBe List("a")
      valueStream(all, tree2).toList shouldBe List("a", "b")
      valueStream(all, tree3_1).toList shouldBe List("a", "b", "c")
      valueStream(all, tree3_2).toList shouldBe List("a", "b", "c")
      valueStream(all, tree4_1).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree4_2).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree4_3).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree7).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      valueStream(all, tree9).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")
    }

    "stream filtered nodes" in {
      valueStream(all, tree1).toList shouldBe List("a")
      valueStream(all, tree2).toList shouldBe List("a", "b")
      valueStream(all, tree3_1).toList shouldBe List("a", "b", "c")
      valueStream(all, tree3_2).toList shouldBe List("a", "b", "c")
      valueStream(all, tree4_1).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree4_2).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree4_3).toList shouldBe List("a", "b", "c", "d")
      valueStream(all, tree7).toList shouldBe List("a", "b", "c", "d", "e", "f", "g")
      valueStream(all, tree9).toList shouldBe List("a", "b", "c", "d", "e", "f", "g", "h", "i")

      valueStream(none, tree1).toList shouldBe Nil
      valueStream(none, tree2).toList shouldBe Nil
      valueStream(none, tree3_1).toList shouldBe Nil
      valueStream(none, tree3_2).toList shouldBe Nil
      valueStream(none, tree4_1).toList shouldBe Nil
      valueStream(none, tree4_2).toList shouldBe Nil
      valueStream(none, tree4_3).toList shouldBe Nil
      valueStream(none, tree7).toList shouldBe Nil
      valueStream(none, tree9).toList shouldBe Nil

      valueStream(even, tree1).toList shouldBe Nil
      valueStream(even, tree2).toList shouldBe List("b")
      valueStream(even, tree3_1).toList shouldBe List("b")
      valueStream(even, tree3_2).toList shouldBe List("b")
      valueStream(even, tree4_1).toList shouldBe List("b", "d")
      valueStream(even, tree4_2).toList shouldBe List("b", "d")
      valueStream(even, tree4_3).toList shouldBe List("b", "d")
      valueStream(even, tree7).toList shouldBe List("b", "d", "f")
      valueStream(even, tree9).toList shouldBe List("b", "d", "f", "h")

      valueStream(odd, tree1).toList shouldBe List("a")
      valueStream(odd, tree2).toList shouldBe List("a")
      valueStream(odd, tree3_1).toList shouldBe List("a", "c")
      valueStream(odd, tree3_2).toList shouldBe List("a", "c")
      valueStream(odd, tree4_1).toList shouldBe List("a", "c")
      valueStream(odd, tree4_2).toList shouldBe List("a", "c")
      valueStream(odd, tree4_3).toList shouldBe List("a", "c")
      valueStream(odd, tree7).toList shouldBe List("a", "c", "e", "g")
      valueStream(odd, tree9).toList shouldBe List("a", "c", "e", "g", "i")
    }

    "stream all branches" in {
      branchStream(all, tree1) shouldBe List(List("a"))
      branchStream(all, tree2) shouldBe List(List("a", "b"))
      branchStream(all, tree3_1) shouldBe List(List("a", "b", "c"))
      branchStream(all, tree3_2) shouldBe List(List("a", "b"), List("a", "c"))
      branchStream(all, tree4_1) shouldBe List(List("a", "b", "c", "d"))
      branchStream(all, tree4_2) shouldBe List(List("a", "b", "c"), List("a", "d"))
      branchStream(all, tree4_3) shouldBe List(List("a", "b"), List("a", "c"), List("a", "d"))
      branchStream(all, tree7) shouldBe List(List("a", "b", "c"), List("a", "d", "e", "f"), List("a", "g"))
      branchStream(all, tree9) shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
    }

    "stream filtered branches" in {
      branchStream(all, tree1).map(_.toList).toList shouldBe List(List("a"))
      branchStream(all, tree2).map(_.toList).toList shouldBe List(List("a", "b"))
      branchStream(all, tree3_1).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      branchStream(all, tree3_2).map(_.toList).toList shouldBe List(List("a", "b"), List("a", "c"))
      branchStream(all, tree4_1).map(_.toList).toList shouldBe List(List("a", "b", "c", "d"))
      branchStream(all, tree4_2).map(_.toList).toList shouldBe List(List("a", "b", "c"), List("a", "d"))
      branchStream(all, tree4_3).map(_.toList).toList shouldBe List(
        List("a", "b"),
        List("a", "c"),
        List("a", "d")
      )
      branchStream(all, tree7).map(_.toList).toList shouldBe List(
        List("a", "b", "c"),
        List("a", "d", "e", "f"),
        List("a", "g")
      )
      branchStream(all, tree9).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )

      branchStream(none, tree1).toList shouldBe Nil
      branchStream(none, tree2).toList shouldBe Nil
      branchStream(none, tree3_1).toList shouldBe Nil
      branchStream(none, tree3_2).toList shouldBe Nil
      branchStream(none, tree4_1).toList shouldBe Nil
      branchStream(none, tree4_2).toList shouldBe Nil
      branchStream(none, tree4_3).toList shouldBe Nil
      branchStream(none, tree7).toList shouldBe Nil
      branchStream(none, tree9).toList shouldBe Nil

      branchStream[String](_.size > 1, tree1).toList shouldBe Nil
      branchStream[String](_.size > 1, tree2).map(_.toList).toList shouldBe List(List("a", "b"))
      branchStream[String](_.last == "c", tree3_1).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      branchStream[String](_.last == "c", tree3_2).map(_.toList).toList shouldBe List(List("a", "c"))
      branchStream[String](_.last == "d", tree4_2).map(_.toList).toList shouldBe List(List("a", "d"))
      branchStream[String](_.size > 2, tree4_2).map(_.toList).toList shouldBe List(List("a", "b", "c"))
      branchStream[String](_.size > 3, tree7).map(_.toList).toList shouldBe List(List("a", "d", "e", "f"))
      branchStream[String](_.size > 3, tree9).map(_.toList).toList shouldBe List(
        List("a", "b", "c", "d"),
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      branchStream[String](_.toList.contains("e"), tree9).map(_.toList).toList shouldBe List(
        List("a", "e", "f", "g"),
        List("a", "e", "h", "i")
      )
      branchStream[String](_.toList.contains("h"), tree9).map(_.toList).toList shouldBe List(
        List("a", "e", "h", "i")
      )
      branchStream[String](_.size < 3, tree9).map(_.toList).toList shouldBe Nil
    }

    "stream all subtrees" in {
      treeStream(all, tree1).toList shouldBe List(tree1)
      treeStream(all, tree2).toList shouldBe List(tree2, Tree("b"))
      treeStream(all, tree3_1).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      treeStream(all, tree3_2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      treeStream(all, tree4_1).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      treeStream(all, tree4_2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      treeStream(all, tree4_3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      treeStream(all, tree7).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      treeStream(all, tree9).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin
    }

    "stream filtered subtrees" in {
      treeStream(all, tree1).toList shouldBe List(tree1)
      treeStream(all, tree2).toList shouldBe List(tree2, Tree("b"))
      treeStream(all, tree3_1).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      treeStream(all, tree3_2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      treeStream(all, tree4_1).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      treeStream(all, tree4_2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      treeStream(all, tree4_3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      treeStream(all, tree7).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      treeStream(all, tree9).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      treeStream(none, tree1).toList shouldBe Nil
      treeStream(none, tree2).toList shouldBe Nil
      treeStream(none, tree3_1).toList shouldBe Nil
      treeStream(none, tree3_2).toList shouldBe Nil
      treeStream(none, tree4_1).toList shouldBe Nil
      treeStream(none, tree4_2).toList shouldBe Nil
      treeStream(none, tree4_3).toList shouldBe Nil
      treeStream(none, tree7).toList shouldBe Nil
      treeStream(none, tree9).toList shouldBe Nil

      treeStream[String](_.size > 0, tree1).toList shouldBe List(tree1)
      treeStream[String](_.size > 0, tree2).toList shouldBe List(tree2, Tree("b"))
      treeStream[String](_.size < 2, tree3_2).toList shouldBe List(Tree("b"), Tree("c"))
      treeStream[String](_.height == 2, tree7).toList shouldBe List(Tree("b", Tree("c")), Tree("e", Tree("f")))
      treeStream[String](_.height == 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      treeStream[String](_.height > 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      treeStream[String](_.width >= 2, tree9).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }
  }

  "MyTest" should {

    "use ScalaTest matchers" in {
      (1 + 1) shouldBe 2
      (1 + 1) should not be 3
      Option(1 + 1) shouldBe Some(2)
      (0 until 10) shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
      an[NotImplementedError] shouldBe thrownBy(???)
    }

  }
}
