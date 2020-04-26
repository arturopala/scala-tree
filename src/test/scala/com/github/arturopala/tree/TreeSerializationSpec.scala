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

class TreeSerializationSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "serialize a tree to a list of (numberOfChildren, value) pairs" in {
      tree0.toPairsIterator shouldBe Iterator.empty
      TreeBuilder.fromPairsIterator(Iterator.empty) shouldBe List(Tree.empty)
      tree1.toPairsIterator.toList shouldBe List((0, "a"))
      tree2.toPairsIterator.toList shouldBe List((0, "b"), (1, "a"))
      tree3_1.toPairsIterator.toList shouldBe List((0, "c"), (1, "b"), (1, "a"))
      tree3_2.toPairsIterator.toList shouldBe List((0, "c"), (0, "b"), (2, "a"))
      tree4_1.toPairsIterator.toList shouldBe List((0, "d"), (1, "c"), (1, "b"), (1, "a"))
      tree4_2.toPairsIterator.toList shouldBe List((0, "d"), (0, "c"), (1, "b"), (2, "a"))
      tree4_3.toPairsIterator.toList shouldBe List((0, "d"), (0, "c"), (0, "b"), (3, "a"))
    }

    "serialize a tree to a pair of arrays and deserialize it back using fromArrays" in {
      val (structure0, values0) = tree0.toArrays
      structure0.length shouldBe 0
      values0.length shouldBe 0
      TreeBuilder.fromArrays(structure0, values0) shouldBe List(tree0)

      val (structure1, values1) = tree1.toArrays
      structure1.length shouldBe 1
      values1.length shouldBe 1
      TreeBuilder.fromArrays(structure1, values1) shouldBe List(tree1)

      val (structure2, values2) = tree2.toArrays
      structure2.length shouldBe 2
      structure2 shouldBe Array(0, 1)
      values2.length shouldBe 2
      values2 shouldBe Array("b", "a")
      TreeBuilder.fromArrays(structure2, values2) shouldBe List(tree2)

      val (structure3, values3) = tree3_1.toArrays
      structure3.length shouldBe 3
      structure3 shouldBe Array(0, 1, 1)
      values3.length shouldBe 3
      values3 shouldBe Array("c", "b", "a")
      TreeBuilder.fromArrays(structure3, values3) shouldBe List(tree3_1)

      val (structure3_2, values3_2) = tree3_2.toArrays
      structure3_2.length shouldBe 3
      structure3_2 shouldBe Array(0, 0, 2)
      values3_2.length shouldBe 3
      values3_2 shouldBe Array("c", "b", "a")
      TreeBuilder.fromArrays(structure3_2, values3_2) shouldBe List(tree3_2)

      val (structure4, values4) = tree4_1.toArrays
      structure4.length shouldBe 4
      structure4 shouldBe Array(0, 1, 1, 1)
      values4.length shouldBe 4
      values4 shouldBe Array("d", "c", "b", "a")
      TreeBuilder.fromArrays(structure4, values4) shouldBe List(tree4_1)

      val (structure4_2, values4_2) = tree4_2.toArrays
      structure4_2.length shouldBe 4
      structure4_2 shouldBe Array(0, 0, 1, 2)
      values4_2.length shouldBe 4
      values4_2 shouldBe Array("d", "c", "b", "a")
      TreeBuilder.fromArrays(structure4_2, values4_2) shouldBe List(tree4_2)

      val (structure7, values7) = tree7.toArrays
      structure7.length shouldBe 7
      structure7 shouldBe Array(0, 0, 1, 1, 0, 1, 3)
      values7.length shouldBe 7
      values7 shouldBe Array("g", "f", "e", "d", "c", "b", "a")
      TreeBuilder.fromArrays(structure7, values7) shouldBe List(tree7)
    }

    "serialize a tree to a structure array" in {
      tree0.toStructureArray shouldBe Array.empty[Int]
      tree1.toStructureArray shouldBe Array(0)
      tree2.toStructureArray shouldBe Array(0, 1)
      tree3_1.toStructureArray shouldBe Array(0, 1, 1)
      tree3_2.toStructureArray shouldBe Array(0, 0, 2)
      tree4_1.toStructureArray shouldBe Array(0, 1, 1, 1)
      tree4_2.toStructureArray shouldBe Array(0, 0, 1, 2)
      tree4_3.toStructureArray shouldBe Array(0, 0, 0, 3)
      tree7.toStructureArray shouldBe Array(0, 0, 1, 1, 0, 1, 3)
      tree9.toStructureArray shouldBe Array(0, 1, 0, 1, 2, 0, 1, 1, 2)
    }

  }

}
