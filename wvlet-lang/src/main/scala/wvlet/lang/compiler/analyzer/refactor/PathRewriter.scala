/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wvlet.lang.compiler.analyzer.refactor

import wvlet.lang.model.plan.*
import wvlet.log.LogSupport

/**
  * Utility for replacing nodes in a LogicalPlan tree at specified paths.
  * 
  * A path is represented as a List[Int], where each integer is the index of the child
  * to follow from the current node. For example, path [0, 1, 2] means:
  * - Go to child 0 of root
  * - Go to child 1 of that node
  * - Go to child 2 of that node (this is the target)
  */
object PathRewriter extends LogSupport:

  /**
    * Replace a single node at the specified path
    * 
    * @param root The root of the tree
    * @param path The path to the node to replace (empty means replace root)
    * @param replacement The new node to insert
    * @return A new tree with the replacement applied
    */
  def replaceAtPath(root: LogicalPlan, path: List[Int], replacement: LogicalPlan): LogicalPlan =
    path match
      case Nil =>
        // Replace root itself
        replacement
      case head :: tail =>
        // Navigate to child and recurse
        val children = root.children
        if head < 0 || head >= children.size then
          warn(s"Invalid path index $head for node with ${children.size} children: ${root.getClass.getSimpleName}")
          return root
        
        val updatedChild = replaceAtPath(children(head), tail, replacement)
        replaceChild(root, head, updatedChild)

  /**
    * Replace multiple nodes at specified paths.
    * 
    * IMPORTANT: Paths are processed in order of decreasing length (deepest first)
    * to avoid path invalidation during replacement.
    * 
    * @param root The root of the tree
    * @param replacements List of (path, replacement) pairs
    * @return A new tree with all replacements applied
    */
  def replaceMany(root: LogicalPlan, replacements: List[(List[Int], LogicalPlan)]): LogicalPlan =
    // Sort by path length descending (deepest first)
    val sorted = replacements.sortBy(-_._1.length)
    
    sorted.foldLeft(root) { case (currentRoot, (path, replacement)) =>
      replaceAtPath(currentRoot, path, replacement)
    }

  /**
    * Replace the child at the specified index in a LogicalPlan node.
    * 
    * This handles the various Relation types that SubtreeCollector considers refactorable.
    */
  private def replaceChild(node: LogicalPlan, childIndex: Int, newChild: LogicalPlan): LogicalPlan =
    val children = node.children
    if childIndex < 0 || childIndex >= children.size then
      warn(s"Invalid child index $childIndex for ${node.getClass.getSimpleName}")
      return node

    // Build new children list
    val newChildren = children.updated(childIndex, newChild)

    // Reconstruct the node with new children
    node match
      // === Unary relations ===
      case f: Filter =>
        f.copy(child = newChildren.head.asInstanceOf[Relation])
      case p: Project =>
        p.copy(child = newChildren.head.asInstanceOf[Relation])
      case g: GroupBy =>
        g.copy(child = newChildren.head.asInstanceOf[Relation])
      case a: Agg =>
        a.copy(child = newChildren.head.asInstanceOf[Relation])
      case s: Sort =>
        s.copy(child = newChildren.head.asInstanceOf[Relation])
      case d: Distinct =>
        d.copy(child = newChildren.head.asInstanceOf[Project])
      case l: Limit =>
        l.copy(child = newChildren.head.asInstanceOf[Relation])
      case o: Offset =>
        o.copy(child = newChildren.head.asInstanceOf[Relation])
      case a: AliasedRelation =>
        a.copy(child = newChildren.head.asInstanceOf[Relation])
      case n: NamedRelation =>
        n.copy(child = newChildren.head.asInstanceOf[Relation])
      case p: Pivot =>
        p.copy(child = newChildren.head.asInstanceOf[Relation])
      case s: Sample =>
        s.copy(child = newChildren.head.asInstanceOf[Relation])
      case d: Dedup =>
        d.copy(child = newChildren.head.asInstanceOf[Relation])
        
      // === Binary relations ===
      case j: Join =>
        childIndex match
          case 0 => j.copy(left = newChildren(0).asInstanceOf[Relation])
          case 1 => j.copy(right = newChildren(1).asInstanceOf[Relation])
          case _ => 
            warn(s"Unexpected child index $childIndex for Join")
            node
            
      // === Set operations ===
      case u: Union =>
        childIndex match
          case 0 => u.copy(left = newChildren(0).asInstanceOf[Relation])
          case 1 => u.copy(right = newChildren(1).asInstanceOf[Relation])
          case _ =>
            warn(s"Unexpected child index $childIndex for Union")
            node
      case i: Intersect =>
        childIndex match
          case 0 => i.copy(left = newChildren(0).asInstanceOf[Relation])
          case 1 => i.copy(right = newChildren(1).asInstanceOf[Relation])
          case _ =>
            warn(s"Unexpected child index $childIndex for Intersect")
            node
      case e: Except =>
        e.copy(left = newChildren(0).asInstanceOf[Relation], right = newChildren(1).asInstanceOf[Relation])
      case c: Concat =>
        childIndex match
          case 0 => c.copy(left = newChildren(0).asInstanceOf[Relation])
          case 1 => c.copy(right = newChildren(1).asInstanceOf[Relation])
          case _ =>
            warn(s"Unexpected child index $childIndex for Concat")
            node
        
      // === Query wrappers ===
      case q: Query =>
        q.copy(body = newChildren.head.asInstanceOf[Relation])
      case w: WithQuery =>
        // WithQuery.children returns empty list by design (queryDefs are prerequisites, not children)
        // So we need special handling
        w.copy(queryBody = newChild.asInstanceOf[Relation])
        
      // === Save operations ===
      case a: AppendTo =>
        a.copy(child = newChildren.head.asInstanceOf[Relation])
      case d: Delete =>
        d.copy(child = newChildren.head.asInstanceOf[Relation])
      case s: SaveTo =>
        s.copy(child = newChildren.head.asInstanceOf[Relation])
        
      // === Package/top-level ===
      case p: PackageDef =>
        p.copy(statements = newChildren)
        
      // === Other relations that might appear ===
      case c: Count =>
        c.copy(child = newChildren.head.asInstanceOf[Relation])
      case b: BracedRelation =>
        b.copy(child = newChildren.head.asInstanceOf[Relation])
      case t: TestRelation =>
        t.copy(child = newChildren.head.asInstanceOf[Relation])
      case l: Lateral =>
        l.copy(query = newChildren.head.asInstanceOf[Relation])
      case u: Unpivot =>
        u.copy(child = newChildren.head.asInstanceOf[Relation])
      case d: Debug =>
        childIndex match
          case 0 => d.copy(child = newChildren(0).asInstanceOf[Relation])
          case 1 => d.copy(debugExpr = newChildren(1).asInstanceOf[Relation])
          case _ => node
          
      // === Fallback using reflection (less safe but covers edge cases) ===
      case other =>
        tryReflectiveCopy(other, childIndex, newChild)

  /**
    * Try to use reflection to copy a node with an updated child.
    * This is a fallback for node types not explicitly handled above.
    */
  private def tryReflectiveCopy(node: LogicalPlan, childIndex: Int, newChild: LogicalPlan): LogicalPlan =
    try
      val productClass = node.getClass
      val copyMethod = productClass.getMethods.find(_.getName == "copy")
      
      copyMethod match
        case Some(method) =>
          // Get current field values using Product interface
          // All LogicalPlan nodes extend Product, so this will always match
          val p = node.asInstanceOf[Product]
          val currentValues = (0 until p.productArity).map(p.productElement).toArray
          
          // Find which field corresponds to the child at childIndex
          // This is tricky because children might not align with product fields
          // For now, we'll just warn and return the original
          warn(s"Reflective copy for ${node.getClass.getSimpleName} not fully implemented, returning original")
          node
        case None =>
          warn(s"No copy method found for ${node.getClass.getSimpleName}")
          node
    catch
      case e: Exception =>
        warn(s"Failed reflective copy for ${node.getClass.getSimpleName}: ${e.getMessage}")
        node

end PathRewriter
