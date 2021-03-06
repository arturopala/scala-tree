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

/**
  * Common functions to render a tree.
  */
object TreeFormat {

  final def showAsArrays[F[+_], T](tree: TreeLike[F, T], separator: String): String =
    tree.mkStringFromBranches(_.toString, ",", separator, "[", "]")

  final def showAsGraph[F[+_], T](tree: TreeLike[F, T], separator: String): String =
    tree.mkStringFromBranches(_.toString, " > ", separator, "", "")

  final def showAsPaths[F[+_], T](tree: TreeLike[F, T], separator: String): String =
    tree.mkStringFromBranches(_.toString, "/", separator, "", "")

}
