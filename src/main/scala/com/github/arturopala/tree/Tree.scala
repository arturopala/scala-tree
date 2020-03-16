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

import scala.annotation.tailrec

/** General purpose, covariant, immutable, linked tree-like data structure with rich API.
  *
  * Either an empty or a concrete node.
  * Each concrete node holds a value and links to the other subtrees.
  *
  * @groupprio properties 0
  * @groupname properties Properties
  * @groupdesc properties Tree properties, accessible in O(1).
  * @groupprio nodes 10
  * @groupname nodes Nodes
  * @groupprio sub-trees 20
  * @groupname sub-trees Trees
  * @groupprio branches 30
  * @groupname branches Branches
  * @groupprio modifications 40
  * @groupname modifications Modify
  * @groupprio transformations 50
  * @groupname transformations Transform
  * @groupprio paths 60
  * @groupname paths Path-based operations
  * @groupprio serialization 90
  * @groupname serialization Serialize
  * @groupprio visualization 100
  * @groupname visualization Visualize
  */
sealed trait Tree[+T] {

  /** The number of the concrete nodes in the tree.
    * @group properties */
  val size: Int

  /** The number of leafs (concrete nodes without subtrees),
    * same as a number of distinct branches starting at the root of the tree.
    * @group properties */
  val leafsSize: Int

  /** Returns true if this is a node without subtrees, otherwise false.
    * @group properties */
  val isLeaf: Boolean

  // NODES

  /** List of all the node's values in the tree, presented depth first.
    * @group nodes */
  def nodes(): List[T]

  /** List of all the node's values in the tree, presented depth first.
    * @note Uses unsafe nested recursions, result same as [[nodes()]].
    * @group nodes */
  def nodesUnsafe: List[T]

  /** List of filtered node's values in the tree, presented depth first.
    * @param filter return true to include the value in the result, false otherwise.
    * @group nodes */
  def nodes(filter: T => Boolean): List[T]

  /** Lazy stream of the node's values in the tree, presented depth first
    * @group nodes */
  def nodeStream: Stream[T]

  /** Filtered lazy stream of the node's values in the tree, presented depth first.
    * @param filter return true to include the value in the result, false otherwise.
    * @group nodes */
  def nodeStream(filter: T => Boolean): Stream[T]

  // SUB-TREES

  /** Returns direct children of the tree.
    * @group sub-trees */
  def children: List[Tree[T]]

  /** List all the possible subtrees of this tree inclusive.
    * @group sub-trees */
  def trees(): List[Tree[T]]

  /** List all the possible subtrees of this tree inclusive.
    * @note Uses unsafe nested recursions, result same as [[trees()]].
    * @group sub-trees */
  def treesUnsafe: List[Tree[T]]

  /** List filtered subtrees of this tree inclusive.
    * @param filter return true to include the tree in the result, false otherwise.
    * @group sub-trees */
  def trees(filter: Tree[T] => Boolean): List[Tree[T]]

  // BRANCHES

  /** List all the branches of this tree starting at the root.
    * @note Uses unsafe nested recursions, result same as [[branches()]].
    * @group branches */
  def branches(): List[List[T]]

  /** List all the branches of this tree starting at the root.
    * @group branches */
  def branchesUnsafe: List[List[T]]

  /** List filtered branches of this tree starting at the root.
    * @param filter return true to include the branch in the result, false otherwise.
    * @note An argument to the filter function is a REVERSED branch.
    * @group branches
    */
  def branches(filter: List[T] => Boolean): List[List[T]]

  /** Lazy stream of all the branches of this tree starting at the root.
    * @group branches */
  def branchStream: Stream[List[T]]

  /** Filtered lazy stream of all the branches of this tree starting at the root.
    * @param filter return true to include the branch in the result, false otherwise.
    * @group branches */
  def branchStream(filter: List[T] => Boolean): Stream[List[T]]

  /** Returns the number of distinct branches accepted by the filter, starting at the root of the tree.
    * @param filter return true to count the branch, false otherwise.
    * @group branches */
  def countBranches(filter: List[T] => Boolean): Int

  // MODIFICATION

  /** Inserts a new node holding the value and returns updated tree.
    * @group modifications */
  def insert[T1 >: T](value: T1): Tree[T1]

  /** Inserts a new sub-tree and returns updated tree.
    * @group modifications */
  def insert[T1 >: T](subtree: Tree[T1]): Tree[T1]

  /** Inserts a new branch of values and returns updated tree.
    * @param branch list of values forming a path from the root to the leaf.
    * @note New branch must start with the existing root element of tree, otherwise the tree will stay intact.
    * @group modifications */
  def insert[T1 >: T](branch: List[T1]): Tree[T1]

  // TRANSFORMATION

  /** Maps all nodes of the tree using provided function and returns a new tree.
    * @group transformations */
  def map[K](f: T => K): Tree[K]

  /** Maps all nodes of the tree using provided function and returns a new tree.
    * @note Uses nested recursions.
    * @group transformations */
  def mapUnsafe[K](f: T => K): Tree[K]

  /** Flat-maps all nodes of the tree using provided function and returns a new tree.
    * @group transformations */
  def flatMap[K](f: T => Tree[K])(implicit strategy: Tree.FlatMapStrategy = Tree.FlatMapStrategy.JoinSubtrees): Tree[K]

  // PATH-BASED OPERATIONS

  /** Selects a sub-tree starting at the given path, if any.
    * @param path list of values forming a path from the root to the node.
    * @group paths */
  def select[T1 >: T](path: List[T1]): Option[Tree[T]]

  /** Checks if the given path is a valid prefix of any branch of this tree.
    * @param path list of values forming a path from the root to the node.
    * @group paths */
  def contains[T1 >: T](path: List[T1]): Boolean

  // SERIALIZATION

  /** Outputs tree in the linear format as a list of pairs (numberOfChildren, value).
    * @note It is possible to build a tree back using [[Tree.Builder.fromValueList]] method.
    *
    *       Properties of the generated list:
    *       - every children's node value comes before parent's node value
    *       - children node's values are listed in the reverse order
    *       - sum of the numberOfChildren is the size of the tree minus 1
    *
    * @group serialization */
  def toValueList: List[(Int, T)]

  /** Outputs tree in the linear format as a list of pairs (numberOfChildren, value).
    * @note It is possible to build a tree back using [[Tree.Builder.fromTreeList]] method.
    *
    *       Properties of the generated list:
    *       - every children's node comes before parent's node value
    *       - children nodes are listed in the reverse order
    *       - sum of the numberOfChildren is the size of the tree minus 1
    *
    * @group serialization */
  def toTreeList: List[(Int, Tree[T])]

  // VISUALIZATION

  /** Makes a String representation of the tree.
    * @param show function to render a node value
    * @param nodeSeparator string to separate nodes
    * @param branchStart string to add at the start of each branch
    * @param branchEnd string to add at the end of each branch
    * @param branchSeparator string to separate branches
    * @group visualization
    */
  def mkString(
    show: T => String,
    nodeSeparator: String,
    branchSeparator: String,
    branchStart: String,
    branchEnd: String
  ): String
}

/** Tree companion object.
  * Hosts factory methods, builders, flatMap strategies, helpers and visualisation templates.
  */
object Tree {

  /** Creates an empty Tree, same as [[Tree.empty]]. */
  def apply[T](): Tree[T] = empty

  /** Creates a single leaf tree, same as [[Node.apply(value:T)]]. */
  def apply[T](value: T): Node[T] = Node(value)

  /** Creates a tree node from the value and subtrees */
  def apply[T](value: T, subtree: Node[T], others: Node[T]*): Node[T] = Node(value, subtree :: others.toList)

  /** Creates a tree node from the value and list of subtrees */
  def apply[T](value: T, subtrees: List[Node[T]]): Node[T] = Node(value, subtrees)

  /** Concrete node of the Tree, consisting of a value and a list of subtrees (possibly empty). */
  final case class Node[+T] private (value: T, subtrees: List[Node[T]]) extends Tree[T] {

    /** The number of the concrete nodes in the tree.
      * @group properties */
    override val size: Int = 1 + subtrees.map(_.size).sum

    /** The number of leafs (concrete nodes without subtrees),
      * same as a number of distinct branches starting at the root of the tree.
      * @group properties */
    override val leafsSize: Int = Math.max(1, subtrees.map(_.leafsSize).sum)

    /** Returns true if this is a node without subtrees, otherwise false.
      * @group properties */
    override val isLeaf: Boolean = subtrees.isEmpty

    // NODES

    /** List of filtered node's values in the tree, presented depth first.
      * @group nodes */
    override def nodes(): List[T] = nodes(_ => true)

    /** List of all the node's values in the tree, presented depth first.
      * @note Uses unsafe nested recursions.
      * @group nodes */
    override def nodesUnsafe: List[T] = value :: subtrees.flatMap(_.nodesUnsafe)

    /** List of filtered node's values in the tree, presented depth first.
      * @group nodes */
    override def nodes(filter: T => Boolean): List[T] = NodeOps.nodes(filter, Nil, List(this))

    /** Lazy stream of the node's values in the tree, presented depth first
      * @group nodes */
    override def nodeStream: Stream[T] = nodeStream(_ => true)

    /** Filtered lazy stream of the node's values in the tree, presented depth first.
      * @group nodes */
    override def nodeStream(filter: T => Boolean): Stream[T] = NodeOps.nodeStream(filter, this, Nil)

    // SUB-TREES

    /** Returns direct children of the tree.
      * @group sub-trees */
    def children: List[Tree[T]] = subtrees

    /** List all the possible subtrees of this tree inclusive.
      * @group sub-trees */
    override def trees(): List[Tree[T]] = trees(_ => true)

    /** List all the possible subtrees of this tree inclusive.
      * @note Uses unsafe nested recursions.
      * @group sub-trees */
    override def treesUnsafe: List[Tree[T]] = this :: subtrees.flatMap(_.treesUnsafe)

    /** List filtered subtrees of this tree inclusive.
      * @group sub-trees */
    override def trees(filter: Tree[T] => Boolean): List[Tree[T]] = NodeOps.trees(filter, Nil, List(this))

    // BRANCHES

    /** List all the branches of this tree starting at the root.
      * @group branches */
    override def branches(): List[List[T]] = branches(_ => true)

    /** List all the branches of this tree starting at the root.
      * @group branches */
    override def branchesUnsafe: List[List[T]] = subtrees match {
      case Nil => List(List(value))
      case _ =>
        subtrees.flatMap(_.branchesUnsafe).map(value :: _)
    }

    /** List filtered branches of this tree starting at the root.
      * @note An argument to the filter function is a REVERSED branch.
      * @group branches
      */
    override def branches(filter: List[T] => Boolean): List[List[T]] =
      NodeOps.branches(filter, Nil, subtrees.map((List(value), _)))

    /** Lazy stream of all the branches of this tree starting at the root.
      * @group branches */
    override def branchStream: Stream[List[T]] = branchStream(_ => true)

    /** Filtered lazy stream of all the branches of this tree starting at the root.
      * @group branches */
    override def branchStream(filter: List[T] => Boolean): Stream[List[T]] =
      NodeOps.branchStream(filter, this, Nil, Nil)

    /** Returns the number of distinct branches accepted by the filter, starting at the root of the tree.
      * @group branches */
    override def countBranches(filter: List[T] => Boolean): Int = subtrees match {
      case Nil => 1
      case _   => NodeOps.countBranches(filter, 0, subtrees.map((List(value), _)))
    }

    // MODIFICATION

    /** Inserts a new node holding the value and returns updated tree.
      * @group modifications */
    override def insert[T1 >: T](newValue: T1): Tree[T1] = Node(value, Node(newValue) :: subtrees)

    /** Inserts a new sub-tree and returns updated tree.
      * @group modifications */
    override def insert[T1 >: T](subtree: Tree[T1]): Tree[T1] = subtree match {
      case `empty`        => this
      case node: Node[T1] => Node(value, node :: subtrees)
    }

    /** Inserts a new branch of values and returns updated tree
      * @note New branch must start with the existing root element of tree, otherwise the tree will stay intact.
      * @group modifications */
    override def insert[T1 >: T](branch: List[T1]): Tree[T1] =
      branch match {
        case `value` :: xs => NodeOps.insert(this, xs)
        case _             => this
      }

    // TRANSFORMATION

    /** Maps all nodes of the tree using provided function and returns a new tree.
      * @group transformations */
    override def map[K](f: T => K): Tree[K] = {
      val list: List[(Int, K)] = NodeOps.listMap(f, List((subtrees.size, f(value))), subtrees)
      Builder.fromValueList(list, Nil).headOption.getOrElse(empty)
    }

    /** Maps all nodes of the tree using provided function and returns a new tree.
      * @note Uses nested recursions.
      * @group transformations */
    override def mapUnsafe[K](f: T => K): Tree[K] = {
      def mapNodeUnsafe(n: Node[T]): Node[K] = Node(f(n.value), n.subtrees.map(mapNodeUnsafe))
      mapNodeUnsafe(this)
    }

    /** Flat-maps all nodes of the tree using provided function and returns a new tree.
      * @group transformations */
    override def flatMap[K](
      f: T => Tree[K]
    )(implicit strategy: FlatMapStrategy = FlatMapStrategy.JoinSubtrees): Tree[K] = {
      val list: List[(Int, Tree[K])] = NodeOps.listFlatMap(f, List((subtrees.size, f(value))), subtrees)
      Builder.fromTreeList(list, Nil, 0, strategy).headOption.getOrElse(empty)
    }

    // PATH-BASED OPERATIONS

    /** Selects a sub-tree starting at the given path, if any.
      * @group paths */
    override def select[T1 >: T](path: List[T1]): Option[Tree[T]] = NodeOps.select(this, path)

    /** Checks if the given path is a valid prefix of any branch of this tree.
      * @group paths */
    override def contains[T1 >: T](path: List[T1]): Boolean = NodeOps.contains(this, path)

    // SERIALIZATION

    /** Outputs tree in the linear format as a list of pairs (numberOfChildren, value).
      * @note It is possible to build a tree back using [[Tree.Builder.fromValueList]] method.
      * @group serialization */
    def toValueList: List[(Int, T)] = NodeOps.toValueList(List((subtrees.size, value)), subtrees)

    /** Outputs tree in the linear format as a list of pairs (numberOfChildren, value).
      * @note It is possible to build a tree back using [[Tree.Builder.fromTreeList]] method.
      * @group serialization */
    def toTreeList: List[(Int, Tree[T])] = NodeOps.toTreeList(List((subtrees.size, Node(value))), subtrees)

    // VISUALIZATION

    /** Makes a String representation of the tree.
      * @param show function to render a node value
      * @param nodeSeparator string to separate nodes
      * @param branchStart string to add at the start of each branch
      * @param branchEnd string to add at the end of each branch
      * @param branchSeparator string to separate branches
      * @group visualization
      */
    override def mkString(
      show: T => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String
    ): String = {
      val string = show(value)
      subtrees match {
        case Nil => branchStart + string + branchEnd
        case _ =>
          NodeOps
            .mkString(
              show,
              nodeSeparator,
              branchSeparator,
              branchEnd,
              new StringBuilder().append(branchStart).append(string),
              subtrees.map((branchStart + string, _)),
              newBranch = false
            )
            .mkString
      }
    }

  }

  /** Node companion object. */
  object Node {

    /** Creates a new leaf node */
    def apply[T](value: T): Node[T] = Node(value, Nil)
  }

  /** An empty Tree. */
  final case object empty extends Tree[Nothing] {

    override val size: Int = 0
    override val leafsSize: Int = 0
    override val isLeaf: Boolean = false
    override def nodes(): List[Nothing] = Nil
    override def nodesUnsafe: List[Nothing] = Nil
    override def nodes(filter: Nothing => Boolean): List[Nothing] = Nil
    override def nodeStream: Stream[Nothing] = Stream.empty
    override def nodeStream(filter: Nothing => Boolean): Stream[Nothing] = Stream.empty
    override def select[T1](path: List[T1]): Option[Tree[Nothing]] = if (path.isEmpty) Some(empty) else None
    override def contains[T1](path: List[T1]): Boolean = path.isEmpty
    override def children: List[Tree[Nothing]] = Nil
    override def trees(): List[Tree[Nothing]] = List(empty)
    override def treesUnsafe: List[Tree[Nothing]] = List(empty)
    override def trees(filter: Tree[Nothing] => Boolean): List[Tree[Nothing]] = List(empty)
    override def branches(): List[List[Nothing]] = Nil
    override def branchesUnsafe: List[List[Nothing]] = Nil
    override def branches(filter: List[Nothing] => Boolean): List[List[Nothing]] = Nil
    override def branchStream: Stream[List[Nothing]] = Stream.empty
    override def branchStream(filter: List[Nothing] => Boolean): Stream[List[Nothing]] = Stream.empty
    override def countBranches(filter: List[Nothing] => Boolean): Int = 0
    override def insert[T1](value: T1): Tree[T1] = Node(value)
    override def insert[T1](subtree: Tree[T1]): Tree[T1] = subtree
    override def insert[T1](branch: List[T1]): Tree[T1] = branch match {
      case x :: xs => NodeOps.insert(Node(x), xs)
      case _       => empty
    }
    override def map[K](f: Nothing => K): Tree[K] = empty
    override def mapUnsafe[K](f: Nothing => K): Tree[K] = empty
    override def flatMap[K](f: Nothing => Tree[K])(implicit strategy: FlatMapStrategy): Tree[K] = empty
    override def toValueList: List[(Int, Nothing)] = Nil
    override def toTreeList: List[(Int, Tree[Nothing])] = Nil
    override def mkString(
      show: Nothing => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchStart: String,
      branchEnd: String
    ): String = ""
  }

  /** Static operations on the concrete nodes of the tree. */
  final object NodeOps {

    @tailrec
    def nodes[T](filter: T => Boolean, result: List[T], remaining: List[Node[T]]): List[T] =
      remaining match {
        case Nil => result.reverse
        case Node(value, subtrees) :: xs =>
          if (filter(value)) nodes(filter, value :: result, subtrees ::: xs)
          else nodes(filter, result, subtrees ::: xs)
      }

    def nodeStream[T](filter: T => Boolean, node: Node[T], remaining: List[Node[T]]): Stream[T] = {
      def continue: Stream[T] = node.subtrees match {
        case x :: xs =>
          nodeStream(filter, x, xs ::: remaining)

        case Nil =>
          remaining match {
            case y :: ys => nodeStream(filter, y, ys)
            case Nil     => Stream.empty
          }
      }
      if (filter(node.value)) Stream.cons(node.value, continue) else continue
    }

    @tailrec
    def select[T, T1 >: T](node: Node[T], path: List[T1]): Option[Node[T]] =
      if (path.isEmpty || (path.nonEmpty && path.head != node.value)) None
      else if (path.tail.isEmpty) {
        if (path.head == node.value) Some(node) else None
      } else {
        val nextOpt = node.subtrees
          .collectFirst {
            case nextNode if nextNode.value == path.tail.head => nextNode
          }
        if (nextOpt.isEmpty) None
        else select(nextOpt.get, path.tail)
      }

    @tailrec
    def contains[T, T1 <: T](node: Node[T], path: List[T1]): Boolean =
      if (path.isEmpty || (path.nonEmpty && path.head != node.value)) false
      else if (path.tail.isEmpty) path.head == node.value
      else {
        val nextOpt = node.subtrees
          .collectFirst {
            case nextNode if nextNode.value == path.tail.head => nextNode
          }
        if (nextOpt.isEmpty) false
        else contains(nextOpt.get, path.tail)
      }

    @tailrec
    def trees[T](filter: Tree[T] => Boolean, result: List[Node[T]], remaining: List[Node[T]]): List[Node[T]] =
      remaining match {
        case Nil => result.reverse
        case (node @ Node(_, subtrees)) :: xs =>
          if (filter(node)) trees(filter, node :: result, subtrees ::: xs)
          else trees(filter, result, subtrees ::: xs)
      }
    @tailrec
    def branches[T](
      filter: List[T] => Boolean,
      result: List[List[T]],
      remaining: List[(List[T], Node[T])]
    ): List[List[T]] =
      remaining match {
        case Nil => result.reverse
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = value :: acc
          subtrees match {
            case Nil if filter(branch) => branches(filter, branch.reverse :: result, xs)
            case _                     => branches(filter, result, subtrees.map((branch, _)) ::: xs)
          }
      }

    def branchStream[T](
      filter: List[T] => Boolean,
      tree: Tree[T],
      acc: List[T],
      remaining: List[(List[T], Node[T])]
    ): Stream[List[T]] =
      tree match {
        case `empty` => Stream.empty

        case Node(value, subtrees) =>
          subtrees match {
            case x :: Nil =>
              branchStream(filter, x, value :: acc, remaining)

            case x :: xs =>
              branchStream(filter, x, value :: acc, (acc, Node(value, xs)) :: remaining)

            case Nil =>
              val branch = value :: acc
              def continue: Stream[List[T]] = remaining match {
                case (acc2, y) :: ys => branchStream(filter, y, acc2, ys)
                case Nil             => Stream.empty
              }
              if (filter(branch)) Stream.cons(branch.reverse, continue)
              else continue
          }
      }

    @tailrec
    def countBranches[T](filter: List[T] => Boolean, result: Int, remaining: List[(List[T], Node[T])]): Int =
      remaining match {
        case Nil => result
        case (acc, Node(value, subtrees)) :: xs =>
          val branch = value :: acc
          subtrees match {
            case Nil if filter(branch) => countBranches(filter, 1 + result, xs)
            case _                     => countBranches(filter, result, subtrees.map((branch, _)) ::: xs)
          }
      }

    def insert[T, T1 >: T](tree: Node[T], branch: List[T1]): Node[T1] =
      branch match {
        case x :: xs =>
          tree.subtrees.partition(_.value == x) match {

            case (Nil, bs) =>
              val c = insert(Node(x), xs)
              Node(tree.value, c :: bs)

            case (as, bs) =>
              as match {

                case a :: Nil =>
                  val c = insert(a, xs)
                  Node(tree.value, c :: bs)

                case _ =>
                  val cs = as.map(insert(_, xs))
                  Node(tree.value, cs ::: bs)
              }
          }

        case Nil => tree
      }

    @tailrec
    def toValueList[T](result: List[(Int, T)], remaining: List[Node[T]]): List[(Int, T)] =
      remaining match {
        case Nil => result
        case Node(value, subtrees) :: xs =>
          subtrees match {
            case Nil => toValueList((subtrees.size, value) :: result, subtrees ::: xs)
            case _   => toValueList((subtrees.size, value) :: result, subtrees ::: xs)
          }
      }

    @tailrec
    def toTreeList[T](
      result: List[(Int, Tree[T])],
      remaining: List[Node[T]]
    ): List[(Int, Tree[T])] =
      remaining match {
        case Nil => result
        case Node(value, subtrees) :: xs =>
          subtrees match {
            case Nil => toTreeList((subtrees.size, Node(value)) :: result, subtrees ::: xs)
            case _   => toTreeList((subtrees.size, Node(value)) :: result, subtrees ::: xs)
          }
      }

    @tailrec
    def listMap[T, K](f: T => K, result: List[(Int, K)], remaining: List[Node[T]]): List[(Int, K)] =
      remaining match {
        case Nil => result
        case Node(value, subtrees) :: xs =>
          subtrees match {
            case Nil => listMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
            case _   => listMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
          }
      }

    @tailrec
    def listFlatMap[T, K](
      f: T => Tree[K],
      result: List[(Int, Tree[K])],
      remaining: List[Node[T]]
    ): List[(Int, Tree[K])] =
      remaining match {
        case Nil => result
        case Node(value, subtrees) :: xs =>
          subtrees match {
            case Nil => listFlatMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
            case _   => listFlatMap(f, (subtrees.size, f(value)) :: result, subtrees ::: xs)
          }
      }

    @tailrec
    def mkString[T](
      show: T => String,
      nodeSeparator: String,
      branchSeparator: String,
      branchEnd: String,
      result: StringBuilder,
      remaining: List[(String, Node[T])],
      newBranch: Boolean
    ): StringBuilder =
      remaining match {
        case Nil => result
        case (prefix, Node(value, subtrees)) :: xs =>
          val string = show(value)
          val builder = (if (newBranch) result.append(branchSeparator).append(prefix) else result)
            .append(nodeSeparator)
            .append(string)
          subtrees match {
            case Nil =>
              mkString(show, nodeSeparator, branchSeparator, branchEnd, builder.append(branchEnd), xs, newBranch = true)
            case _ =>
              mkString(
                show,
                nodeSeparator,
                branchSeparator,
                branchEnd,
                builder,
                subtrees.map((prefix + nodeSeparator + string, _)) ::: xs,
                newBranch = false
              )
          }
      }

  }

  /** There are multiple ways to flatten the tree after expanding a node.
    * As we don't want to be constrained by an arbitrary choice,
    * there is a possibility to create and/or use your own strategy when doing a flatMap.
    */
  trait FlatMapStrategy {

    /** When a value of a node expands into a new Node,
      * we need a way to deal with the existing subtrees. */
    def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T]

    /** When a value of a node expands into an Empty tree,
      * we need to decide either to keep or remove existing subtrees. */
    def keepOrphanedSubtrees: Boolean
  }

  object FlatMapStrategy {

    /** Default strategy is to preserve all existing subtrees. */
    object JoinSubtrees extends FlatMapStrategy {

      /** Concatenates new and existing subtrees of an expanded node. */
      override def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T] =
        Node(newNode.value, existingSubtrees ::: newNode.subtrees)

      /** Joins orphaned subtrees to the parent node. */
      override def keepOrphanedSubtrees: Boolean = true
    }

    /** A strategy to replace existing subtrees with the new ones. */
    object Replace extends FlatMapStrategy {

      /** Replaces old subtrees with the new ones. */
      override def merge[T](newNode: Node[T], existingSubtrees: List[Node[T]]): Node[T] =
        newNode

      /** Removes orphaned subtrees completely. */
      override def keepOrphanedSubtrees: Boolean = false
    }

  }

  /** Useful methods to construct the tree. */
  object Builder {

    /** Builds a tree from a list of pairs (numberOfChildren, value), where:
      *   - `value` is the value of a new node, and
      *   - `numberOfChildren` is a number of preceding elements in the list
      *                      to become direct subtrees of the node.
      * @note - Values of subtrees must always precede the value of a parent node, and appear in the reverse order.
      *       - The sum of all numberOfChildren values must be the size of the list minus one.
      */
    @tailrec
    final def fromValueList[K](list: List[(Int, K)], result: List[Node[K]] = Nil): List[Tree[K]] = list match {
      case Nil => result
      case (size, value) :: xs =>
        fromValueList(xs, Node(value, result.take(size)) :: result.drop(size))
    }

    /** Builds a tree from a list of pairs (numberOfChildren, node), where:
      *   - `node` is a new node, and
      *   - `numberOfChildren` is a number of preceding elements in the list
      *                      to become direct subtrees of the current node.
      *   - `strategy` defines how to merge nodes and what to do with orphaned subtrees.
      * @note - Nodes of subtrees must always precede the parent node, and appear in the reverse order.
      *       - The sum of all numberOfChildren values must be the size of the list minus one.
      */
    @tailrec
    final def fromTreeList[K](
      list: List[(Int, Tree[K])],
      result: List[Node[K]] = Nil,
      offset: Int = 0,
      strategy: FlatMapStrategy = FlatMapStrategy.JoinSubtrees
    ): List[Tree[K]] =
      list match {
        case Nil => result
        case (size, tree) :: xs =>
          tree match {
            case `empty` =>
              val offset = if (strategy.keepOrphanedSubtrees) size else -1
              fromTreeList(xs, result.drop(size - offset), offset, strategy)
            case node: Node[K] =>
              val merged = strategy.merge(node, result.take(size))
              fromTreeList(xs, merged :: result.drop(size), 0, strategy)
          }
      }
  }

  /** Collection of common visualization templates. */
  object Show {

    def showAsArrays[T <: Any](tree: Tree[T]): String =
      tree.mkString(_.toString, ",", "\n", "[", "]")

    def showAsGraph[T <: Any](tree: Tree[T]): String =
      tree.mkString(_.toString, " > ", "\n", "", "")

    def showAsPaths[T <: Any](tree: Tree[T]): String =
      tree.mkString(_.toString, "/", "\n", "", "")

  }

}
