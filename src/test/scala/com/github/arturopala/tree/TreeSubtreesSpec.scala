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

import com.github.arturopala.tree.TreeOptions.TraversingMode.{TopDownBreadthFirst, TopDownDepthFirst}

class TreeSubtreesSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "iterate over trees traversing top-down and depth-first" in {
      tree0.trees(TopDownDepthFirst) shouldBe Nil
      tree1.trees(TopDownDepthFirst) shouldBe List(tree1)
      tree2.trees(TopDownDepthFirst) shouldBe List(tree2, Tree("b"))
      tree3_1.trees(TopDownDepthFirst) shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.trees(TopDownDepthFirst) shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.trees(TopDownDepthFirst) shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.trees(TopDownDepthFirst) shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("c"), Tree("d"))
      tree4_3.trees(TopDownDepthFirst) shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.trees(TopDownDepthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.trees(TopDownDepthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
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

    "iterate over trees traversing traversing top-down and breadth-first" in {
      tree0.trees(TopDownBreadthFirst) shouldBe Nil
      tree1.trees(TopDownBreadthFirst) shouldBe List(tree1)
      tree2.trees(TopDownBreadthFirst) shouldBe List(tree2, Tree("b"))
      tree3_1.trees(TopDownBreadthFirst) shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.trees(TopDownBreadthFirst) shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.trees(TopDownBreadthFirst) shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.trees(TopDownBreadthFirst) shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("d"), Tree("c"))
      tree4_3.trees(TopDownBreadthFirst) shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.trees(TopDownBreadthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]
          |[c]
          |[e,f]
          |[f]""".stripMargin
      tree9.trees(TopDownBreadthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]
          |[c,d]
          |[f,g]
          |[h,i]
          |[d]
          |[g]
          |[i]""".stripMargin
    }

    "iterate over filtered trees traversing top-down and depth-first" in {
      tree0.treesWithFilter(all, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(tree1)
      tree2.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      tree4_3.treesWithFilter(all, TopDownDepthFirst).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treesWithFilter(all, TopDownDepthFirst).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treesWithFilter(all, TopDownDepthFirst).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree2.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_1.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_2.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_1.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_2.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_3.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree7.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree9.treesWithFilter(none, TopDownDepthFirst).toList shouldBe Nil

      tree0.treesWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe List(tree1)
      tree2.treesWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treesWithFilter(_.size < 2, TopDownDepthFirst).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treesWithFilter(_.height == 2, TopDownDepthFirst).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("e", Tree("f"))
      )
      tree9.treesWithFilter(_.height == 2, TopDownDepthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treesWithFilter(_.height > 2, TopDownDepthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treesWithFilter(_.width >= 2, TopDownDepthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }

    "iterate over filtered trees traversing top-down and breadth-first" in {
      tree0.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(tree1)
      tree2.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("d"),
        Tree("c")
      )
      tree4_3.treesWithFilter(all, TopDownBreadthFirst).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treesWithFilter(all, TopDownBreadthFirst).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]
          |[c]
          |[e,f]
          |[f]""".stripMargin
      tree9.treesWithFilter(all, TopDownBreadthFirst).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]
          |[c,d]
          |[f,g]
          |[h,i]
          |[d]
          |[g]
          |[i]""".stripMargin

      tree0.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree2.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_1.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_2.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_1.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_2.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_3.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree7.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree9.treesWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil

      tree0.treesWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe List(tree1)
      tree2.treesWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe List(tree2, Tree("b"))
      tree3_2.treesWithFilter(_.size < 2, TopDownBreadthFirst).toList shouldBe List(Tree("b"), Tree("c"))
      tree7.treesWithFilter(_.height == 2, TopDownBreadthFirst).toList shouldBe List(
        Tree("b", Tree("c")),
        Tree("e", Tree("f"))
      )
      tree9.treesWithFilter(_.height == 2, TopDownBreadthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treesWithFilter(_.height > 2, TopDownBreadthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treesWithFilter(_.width >= 2, TopDownBreadthFirst).map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[e,f,g],[e,h,i]""".stripMargin
    }

    "iterate over filtered trees with depth limit traversing top-down and depth-first" in {
      tree0.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree0.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree1)
      tree1.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree1)
      tree2.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree2.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree2)
      tree2.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree2, Tree("b"))
      tree2.treesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree3_1.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree3_1)
      tree3_1.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree3_1, Tree("b", Tree("c")))
      tree3_1.treesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List(tree3_1, Tree("b", Tree("c")), Tree("c"))
      tree3_2.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree3_2.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree3_2)
      tree3_2.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_1.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree4_1)
      tree4_1.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_1.treesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d"))
      )
      tree4_1.treesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_2.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree4_2)
      tree4_2.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree4_2, Tree("b", Tree("c")), Tree("d"))
      tree4_2.treesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownDepthFirst, 4).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("c"),
        Tree("d")
      )
      tree4_3.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree4_3.treesWithFilter(all, TopDownDepthFirst, 1).toList shouldBe List(tree4_3)
      tree4_3.treesWithFilter(all, TopDownDepthFirst, 2).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree4_3.treesWithFilter(all, TopDownDepthFirst, 3).toList shouldBe List(tree4_3, Tree("b"), Tree("c"), Tree("d"))
      tree7.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree7.treesWithFilter(all, TopDownDepthFirst, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]""".stripMargin
      tree7.treesWithFilter(all, TopDownDepthFirst, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]""".stripMargin
      tree7.treesWithFilter(all, TopDownDepthFirst, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[g]""".stripMargin
      tree7.treesWithFilter(all, TopDownDepthFirst, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[c]
          |[d,e,f]
          |[e,f]
          |[f]
          |[g]""".stripMargin
      tree9.treesWithFilter(all, TopDownDepthFirst, 0).toList shouldBe Nil
      tree9.treesWithFilter(all, TopDownDepthFirst, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownDepthFirst, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownDepthFirst, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownDepthFirst, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[c,d]
          |[d]
          |[e,f,g],[e,h,i]
          |[f,g]
          |[g]
          |[h,i]
          |[i]""".stripMargin

      tree0.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree1.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree2.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree3_1.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree3_2.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree4_1.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree4_2.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree4_3.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree7.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
      tree9.treesWithFilter(none, TopDownDepthFirst, 10).toList shouldBe Nil
    }

    "iterate over filtered trees with depth limit traversing top-down and breadth-first" in {
      tree0.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree0.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree1.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree1)
      tree1.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(tree1)
      tree2.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree2.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree2)
      tree2.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(tree2, Tree("b"))
      tree2.treesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List(tree2, Tree("b"))
      tree3_1.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree3_1.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree3_1)
      tree3_1.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(tree3_1, Tree("b", Tree("c")))
      tree3_1.treesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List(
        tree3_1,
        Tree("b", Tree("c")),
        Tree("c")
      )
      tree3_2.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree3_2.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree3_2)
      tree3_2.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(tree3_2, Tree("b"), Tree("c"))
      tree4_1.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_1.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree4_1)
      tree4_1.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d")))
      )
      tree4_1.treesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d"))
      )
      tree4_1.treesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List(
        tree4_1,
        Tree("b", Tree("c", Tree("d"))),
        Tree("c", Tree("d")),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_2.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree4_2)
      tree4_2.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("d")
      )
      tree4_2.treesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("d"),
        Tree("c")
      )
      tree4_2.treesWithFilter(all, TopDownBreadthFirst, 4).toList shouldBe List(
        tree4_2,
        Tree("b", Tree("c")),
        Tree("d"),
        Tree("c")
      )
      tree4_3.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree4_3.treesWithFilter(all, TopDownBreadthFirst, 1).toList shouldBe List(tree4_3)
      tree4_3.treesWithFilter(all, TopDownBreadthFirst, 2).toList shouldBe List(
        tree4_3,
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree4_3.treesWithFilter(all, TopDownBreadthFirst, 3).toList shouldBe List(
        tree4_3,
        Tree("b"),
        Tree("c"),
        Tree("d")
      )
      tree7.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree7.treesWithFilter(all, TopDownBreadthFirst, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]""".stripMargin
      tree7.treesWithFilter(all, TopDownBreadthFirst, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]""".stripMargin
      tree7.treesWithFilter(all, TopDownBreadthFirst, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]
          |[c]
          |[e,f]""".stripMargin
      tree7.treesWithFilter(all, TopDownBreadthFirst, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c],[a,d,e,f],[a,g]
          |[b,c]
          |[d,e,f]
          |[g]
          |[c]
          |[e,f]
          |[f]""".stripMargin
      tree9.treesWithFilter(all, TopDownBreadthFirst, 0).toList shouldBe Nil
      tree9.treesWithFilter(all, TopDownBreadthFirst, 1).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownBreadthFirst, 2).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownBreadthFirst, 3).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]
          |[c,d]
          |[f,g]
          |[h,i]""".stripMargin
      tree9.treesWithFilter(all, TopDownBreadthFirst, 4).toList.map(_.showAsArrays()).mkString("\n") shouldBe
        """[a,b,c,d],[a,e,f,g],[a,e,h,i]
          |[b,c,d]
          |[e,f,g],[e,h,i]
          |[c,d]
          |[f,g]
          |[h,i]
          |[d]
          |[g]
          |[i]""".stripMargin

      tree0.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree1.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree2.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree3_1.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree3_2.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree4_1.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree4_2.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree4_3.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree7.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
      tree9.treesWithFilter(none, TopDownBreadthFirst, 10).toList shouldBe Nil
    }

    "iterate over filtered trees and levels traversing top-down and depth-first" in {
      tree0.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List((1, tree1))
      tree2.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List((1, tree2), (2, Tree("b")))
      tree3_1.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, tree3_1),
        (2, Tree("b", Tree("c"))),
        (3, Tree("c"))
      )
      tree3_2.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, tree3_2),
        (2, Tree("b")),
        (2, Tree("c"))
      )
      tree4_1.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, tree4_1),
        (2, Tree("b", Tree("c", Tree("d")))),
        (3, Tree("c", Tree("d"))),
        (4, Tree("d"))
      )
      tree4_2.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, tree4_2),
        (2, Tree("b", Tree("c"))),
        (3, Tree("c")),
        (2, Tree("d"))
      )
      tree4_3.treesAndLevelsWithFilter(all, TopDownDepthFirst).toList shouldBe List(
        (1, tree4_3),
        (2, Tree("b")),
        (2, Tree("c")),
        (2, Tree("d"))
      )
      tree7
        .treesAndLevelsWithFilter(all, TopDownDepthFirst)
        .toList
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c],[a,d,e,f],[a,g]
          |2: [b,c]
          |3: [c]
          |2: [d,e,f]
          |3: [e,f]
          |4: [f]
          |2: [g]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(all, TopDownDepthFirst)
        .toList
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [b,c,d]
          |3: [c,d]
          |4: [d]
          |2: [e,f,g],[e,h,i]
          |3: [f,g]
          |4: [g]
          |3: [h,i]
          |4: [i]""".stripMargin

      tree0.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree2.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_1.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree3_2.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_1.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_2.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree4_3.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree7.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil
      tree9.treesAndLevelsWithFilter(none, TopDownDepthFirst).toList shouldBe Nil

      tree0.treesAndLevelsWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe List((1, tree1))
      tree2.treesAndLevelsWithFilter(_.size > 0, TopDownDepthFirst).toList shouldBe List((1, tree2), (2, Tree("b")))
      tree3_2.treesAndLevelsWithFilter(_.size < 2, TopDownDepthFirst).toList shouldBe List(
        (2, Tree("b")),
        (2, Tree("c"))
      )
      tree7.treesAndLevelsWithFilter(_.height == 2, TopDownDepthFirst).toList shouldBe List(
        (2, Tree("b", Tree("c"))),
        (3, Tree("e", Tree("f")))
      )
      tree9
        .treesAndLevelsWithFilter(_.height == 2, TopDownDepthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """3: [c,d]
          |3: [f,g]
          |3: [h,i]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(_.height > 2, TopDownDepthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [b,c,d]
          |2: [e,f,g],[e,h,i]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(_.width >= 2, TopDownDepthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [e,f,g],[e,h,i]""".stripMargin
    }

    "iterate over filtered trees and levels traversing top-down and breadth-first" in {
      tree0.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List((1, tree1))
      tree2.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List((1, tree2), (2, Tree("b")))
      tree3_1.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, tree3_1),
        (2, Tree("b", Tree("c"))),
        (3, Tree("c"))
      )
      tree3_2.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, tree3_2),
        (2, Tree("b")),
        (2, Tree("c"))
      )
      tree4_1.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, tree4_1),
        (2, Tree("b", Tree("c", Tree("d")))),
        (3, Tree("c", Tree("d"))),
        (4, Tree("d"))
      )
      tree4_2.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, tree4_2),
        (2, Tree("b", Tree("c"))),
        (2, Tree("d")),
        (3, Tree("c"))
      )
      tree4_3.treesAndLevelsWithFilter(all, TopDownBreadthFirst).toList shouldBe List(
        (1, tree4_3),
        (2, Tree("b")),
        (2, Tree("c")),
        (2, Tree("d"))
      )
      tree7
        .treesAndLevelsWithFilter(all, TopDownBreadthFirst)
        .toList
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c],[a,d,e,f],[a,g]
          |2: [b,c]
          |2: [d,e,f]
          |2: [g]
          |3: [c]
          |3: [e,f]
          |4: [f]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(all, TopDownBreadthFirst)
        .toList
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [b,c,d]
          |2: [e,f,g],[e,h,i]
          |3: [c,d]
          |3: [f,g]
          |3: [h,i]
          |4: [d]
          |4: [g]
          |4: [i]""".stripMargin

      tree0.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree2.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_1.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree3_2.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_1.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_2.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree4_3.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree7.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil
      tree9.treesAndLevelsWithFilter(none, TopDownBreadthFirst).toList shouldBe Nil

      tree0.treesAndLevelsWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe Nil
      tree1.treesAndLevelsWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe List((1, tree1))
      tree2.treesAndLevelsWithFilter(_.size > 0, TopDownBreadthFirst).toList shouldBe List((1, tree2), (2, Tree("b")))
      tree3_2.treesAndLevelsWithFilter(_.size < 2, TopDownBreadthFirst).toList shouldBe List(
        (2, Tree("b")),
        (2, Tree("c"))
      )
      tree7.treesAndLevelsWithFilter(_.height == 2, TopDownBreadthFirst).toList shouldBe List(
        (2, Tree("b", Tree("c"))),
        (3, Tree("e", Tree("f")))
      )
      tree9
        .treesAndLevelsWithFilter(_.height == 2, TopDownBreadthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """3: [c,d]
          |3: [f,g]
          |3: [h,i]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(_.height > 2, TopDownBreadthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [b,c,d]
          |2: [e,f,g],[e,h,i]""".stripMargin
      tree9
        .treesAndLevelsWithFilter(_.width >= 2, TopDownBreadthFirst)
        .map { case (l, t) => s"$l: ${t.showAsArrays()}" }
        .mkString("\n") shouldBe
        """1: [a,b,c,d],[a,e,f,g],[a,e,h,i]
          |2: [e,f,g],[e,h,i]""".stripMargin
    }

  }

}
