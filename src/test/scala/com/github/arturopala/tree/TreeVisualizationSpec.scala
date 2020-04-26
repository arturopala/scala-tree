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

class TreeVisualizationSpec extends FunSuite {

  test(Inflated, new Spec with InflatedTestTrees)
  test(Deflated, new Spec with DeflatedTestTrees)

  sealed trait Spec extends AnyWordSpecCompat with TestTrees {

    "visualize the branches of the tree" in {
      tree0.showAsArrays() shouldBe ""
      tree1.showAsArrays() shouldBe "[a]"
      tree2.showAsArrays() shouldBe "[a,b]"
      tree3_1.showAsArrays() shouldBe "[a,b,c]"
      tree3_2.showAsArrays() shouldBe "[a,b],[a,c]"
      tree4_1.showAsArrays() shouldBe "[a,b,c,d]"
      tree4_2.showAsArrays() shouldBe "[a,b,c],[a,d]"
      tree4_3.showAsArrays() shouldBe "[a,b],[a,c],[a,d]"
      tree7.showAsArrays() shouldBe "[a,b,c],[a,d,e,f],[a,g]"
      tree9.showAsArrays() shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
    }

    "visualize the branches of the tree limiting the depth" in {
      def mkStringWithMaxDepth(n: Int) = tree9.mkStringUsingBranches(_.toString, ",", ",", "[", "]", n)
      mkStringWithMaxDepth(0) shouldBe ""
      mkStringWithMaxDepth(1) shouldBe "[a]"
      mkStringWithMaxDepth(2) shouldBe "[a,b],[a,e]"
      mkStringWithMaxDepth(3) shouldBe "[a,b,c],[a,e,f],[a,e,h]"
      mkStringWithMaxDepth(4) shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
      mkStringWithMaxDepth(5) shouldBe "[a,b,c,d],[a,e,f,g],[a,e,h,i]"
    }

  }

}
