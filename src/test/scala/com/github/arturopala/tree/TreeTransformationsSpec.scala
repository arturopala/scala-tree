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

class TreeTransformationsSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "map all nodes" in {
      val f: String => String = _ + "0"
      tree0.map(f).showAsGraph() shouldBe ""
      tree1.map(f).showAsGraph() shouldBe "a0"
      tree2.map(f).showAsGraph() shouldBe """a0 > b0"""
      tree3_1.map(f).showAsGraph() shouldBe """a0 > b0 > c0"""
      tree3_2.map(f).showAsGraph() shouldBe
        """a0 > b0
          |a0 > c0""".stripMargin
      tree7.map(f).showAsGraph() shouldBe
        """a0 > b0 > c0
          |a0 > d0 > e0 > f0
          |a0 > g0""".stripMargin
    }

    "flatMap all nodes" in {
      val f: String => Tree[String] = x => Tree(x, Tree(x + "0"))
      tree0.flatMap(f).showAsGraph() shouldBe ""
      tree1.flatMap(f).showAsGraph() shouldBe "a > a0"
      tree2.flatMap(f).showAsGraph() shouldBe
        """a > b > b0
          |a > a0""".stripMargin
      tree3_1.flatMap(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > a0""".stripMargin
      tree3_2.flatMap(f).showAsGraph() shouldBe
        """a > b > b0
          |a > c > c0
          |a > a0""".stripMargin
      tree7.flatMap(f).showAsGraph() shouldBe
        """a > b > c > c0
          |a > b > b0
          |a > d > e > f > f0
          |a > d > e > e0
          |a > d > d0
          |a > g > g0
          |a > a0""".stripMargin
    }

    "transform a tree using for-comprehension" in {
      (for {
        n       <- tree9
        subtree <- Tree(n, Tree(n + n), Tree(n + n + n))
      } yield subtree).showAsGraph() shouldBe
        """a > b > c > d > dd
          |a > b > c > d > ddd
          |a > b > c > cc
          |a > b > c > ccc
          |a > b > bb
          |a > b > bbb
          |a > e > f > g > gg
          |a > e > f > g > ggg
          |a > e > f > ff
          |a > e > f > fff
          |a > e > h > i > ii
          |a > e > h > i > iii
          |a > e > h > hh
          |a > e > h > hhh
          |a > e > ee
          |a > e > eee
          |a > aa
          |a > aaa""".stripMargin
    }

  }

}
