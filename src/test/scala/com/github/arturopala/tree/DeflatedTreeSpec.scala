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

class DeflatedTreeSpec extends AnyWordSpecCompat with DeflatedTestTrees {

  "DeflatedTree" should {

    "return nodes in the same order as an inflated tree" in {
      TestTrees.tree3_2.values shouldBe tree3_2.values
      TestTrees.tree7.values shouldBe tree7.values
      TestTrees.tree9.values shouldBe tree9.values
    }

    "return children in the same order as an inflated tree" in {
      TestTrees.tree3_2.childrenValues shouldBe tree3_2.childrenValues
      TestTrees.tree7.childrenValues shouldBe tree7.childrenValues
      TestTrees.tree9.childrenValues shouldBe tree9.childrenValues
    }

    "return branches in the same order as an inflated tree" in {
      TestTrees.tree3_2.branches shouldBe tree3_2.branches
      TestTrees.tree7.branches shouldBe tree7.branches
      TestTrees.tree9.branches shouldBe tree9.branches
    }
  }

}
