/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package avl

/**
 * An ordered map class, implemented with AVL trees.
 */
class TreeMap[Key, Value](implicit ord: Ordering[Key]) {
  /*
   * The root of the tree.
   * If the tree is empty, then this should be null.
   * In reality, this would be a private member but we keep it public
   * for the sake of the testing and grading processes.
   */
  var root: Node[Key, Value] = null

  /*
   * In order to have constant time size queries,
   * we keep count of how many keys the tree currently has.
   * Remember to update this when you insert/remove keys.
   */
  private var _nofKeys = 0
  /** The size of the tree */
  def size: Int = _nofKeys
  /** Is the tree empty? */
  def isEmpty = (_nofKeys == 0)

  //helper metods
  def rightRot(node: Node[Key, Value]): Node[Key, Value] = {
        var y = node.left ; var x = y.right 
  
        substWith(node, y)
        y.right = node
        node.left = x
        node.updateHeight()
        y.updateHeight()
        return y 
  }
    
  def leftRot(node: Node[Key, Value]): Node[Key, Value] = {
      var y = node.right ; var x = y.left
      substWith(node, y)
      y.left = node
      node.right = x
      node.updateHeight()
      y.updateHeight()
      return y 
  }
  
  
  def rebalance(root: Node[Key, Value]){
    var node = root ; var key = node.key ;var balance = node.balance 
    while (node != null){
      key = node.key ; balance = node.balance ; node.updateHeight
      
      if (balance < -1){
        if(node.left.balance > 0){
          leftRot(node.left) 
          rightRot(node)
        }else if (node.left.balance < 0) {
          rightRot(node)
        }else  rightRot(node) }
      
      if (balance > 1){
        if(node.right.balance < 0){
          rightRot(node.right) 
          leftRot(node)
        }else if (node.right.balance > 0) {
          leftRot(node)
        }else leftRot(node)  }
  
      node = node.parent
    }  
  }
  
  
  
  
  /**
   * Insert the (key,value) mapping in the tree.
   * If the key already has a value, replace it with the new one.
   * Returns the old value of the key or None if it did not have one.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def insert(key: Key, value: Value): Option[Value] = {
    
    def inner(node:Node[Key,Value]): Option[Value] = {
      var compare = ord.compare(key, node.key)
      if (compare < 0) {
          if (node.hasLeft) {
            inner(node.left)
          }else {
            node.left = new Node(key, value)
            _nofKeys += 1
            rebalance(node)
            return None
          }

      }else if(compare > 0) {
          if (node.hasRight) {
            inner(node.right)
          }else {
            node.right = new Node(key, value)
            _nofKeys += 1 
            rebalance(node)
            return None
          }

      }else {
        val result = node.value
        node.value = value
        rebalance(node)
        Some(result)
      }
    }

    if(root == null) {
      _nofKeys += 1
      root = new Node(key, value)
      return None
    } else {
      inner(root)
    }
  }


  /**
   * Get the value of the key, or None if the key does not have a value.
   * Should work in time O(h), where h is the height of the tree.
   */
  def get(key: Key): Option[Value] = {
     def inner(node :Node[Key, Value]) :Option[Value]= {
        var tempCompare = ord.compare(key, node.key)    
        if(tempCompare < 0){
                if(node.hasLeft){
                  inner(node.left)
                }else{
                  return None
                }
              }
        else if(tempCompare > 0){
               
              if(node.hasRight){ 
                 inner(node.right)
               }else{
                 return None
               }
            }
        else if(tempCompare == 0){
              return Some(node.value)
            
          }else return None
        }
      //inner tar slut
    inner(root)
  }

  /*
   * Return the smallest key in the treemap, or None if the tree is empty.
   * Should work in time O(h), where h is the height of the tree.
   */
  def min: Option[Key] = {
     def inner(node :Node[Key, Value]) :Option[Key] ={ 
              if(node.hasLeft && ord.compare(node.left.key, node.key) < 0){
                  inner(node.left)  
              }else{
                Some(node.key)
              }
        }//inner tar slut
      if(root == null) return None
      else inner(root)
  }

  /*
   * Return the smallest key in the treemap that is equal to or greater than
   * the argument key (or None if the tree is empty or all its keys are
   * less than the argument key).
   * Should work in time O(h), where h is the height of the tree.
   */
  def ceiling(key: Key): Option[Key] = {
       def inner(node :Node[Key, Value]) :Option[Key] ={ 
        var tempCompare = ord.compare(node.key, key)
          if(tempCompare == 0){
            Some(node.key)
          }else if(tempCompare < 0){ //node.key < key
            if(node.hasRight) inner(node.right) 
            else None
          }else { //node.key > key
            if(node.hasLeft){
              val a = inner(node.left)
              if(a.isDefined && ord.compare(a.get, key)>=0){// a.get > key
                return Some(a.get)
              }else return Some(node.key) 
          }else Some(node.key)
          }
    }//inner tar slut
    if(root == null) return None
    else inner(root)
  }



  /*
   * An internal helper function.
   * Substitutes the node n1 with the node n2.
   * The node n1 must not be null and it (with all its descendants)
   * will be effectively deleted from the tree.
   * The node n2 can be null.
   */
  private def substWith(n1: Node[Key,Value], n2: Node[Key,Value]): Unit = {
    require(n1 != null)
    // Remove the connection from the previous parent of n2
    if(n2 != null && n2.hasParent) {
      if(n2.parent.left == n2) n2.parent.left = null
      else {assert(n2.parent.right == n2); n2.parent.right = null }
    }
    // Make n2 to substitute n1
    if(n1 == root) {root = n2 }
    else {
      if(n1.parent.left == n1) n1.parent.left = n2
      else {assert(n1.parent.right == n1); n1.parent.right = n2 }
    }
  }

  /*
   * Remove the key from the treemap.
   * Do nothing if the key is not in the treemap.
   * Return the old value of the key or None if the key was not in the treemap.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def remove(key: Key): Option[Value] = {
    def inner(node:Node[Key,Value]): Option[Value] = {
      var compare = ord.compare(key, node.key)
      if (compare < 0) {
        if (node.hasLeft) {
          inner(node.left)
        }else {
          return None
        }
      } else if (compare > 0) {
        if (node.hasRight) {
          inner(node.right)
        } else return  None
   
      }else {
        var result = node.value ; var nodeTemp = node ; var nodeRebalance = node.parent 
        (node.left, node.right) match {
          case (null, null)  => substWith(node, null)
          case (null, right) => substWith(node, right)  
          case (left, null)  => substWith(node, left)
          case (left, right) => nodeTemp = left
            while (nodeTemp.hasRight) {nodeTemp = nodeTemp.right}
            
            node.key = nodeTemp.key ; node.value = nodeTemp.value
            substWith(nodeTemp, nodeTemp.left)
            
            if (node.hasRight) rebalance(node.right)
            if (node.hasLeft) rebalance(node.left)
        }

        _nofKeys -= 1
        if (nodeRebalance != null)
          rebalance(nodeRebalance)
        return Some(result)
      }
    }
    return inner(root)
  }


  /**
   * Check whether the BST property hods in the tree, i.e., for each node
   * the descendants in the left sub-tree should be less than the key, and
   * the descendants in the right sub-tree should be greater than the key.
   * Linear in the size of the tree, only for debuggin and validation purposes.
   */
  def isValidBST: Boolean = {
    def inner(n: Node[Key,Value], lower: Option[Key], upper: Option[Key]): Boolean = {
      if(n == null) true
      else if(lower.nonEmpty && ord.compare(n.key, lower.get) <= 0) false
      else if(upper.nonEmpty && ord.compare(n.key, upper.get) >= 0) false
      else inner(n.left, lower, Some(n.key)) && inner(n.right, Some(n.key), upper)
    }
    inner(root, None, None)
  }

  /**
   * Does the tree have the AVL property, i.e., is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty: Boolean = hasAVLProperty(root)

  /**
   * Does the sub-tree rooted at the node have the AVL property, i.e.,
   * is it properly balanced?
   * Slow, for validation and debugging purposes only
   */
  def hasAVLProperty(node: Node[Key,Value]): Boolean = {
    def inner(n: Node[Key,Value]): (Boolean,Int) = {
      if(n == null) (true, -1)
      else {
        val (leftOk, leftHeight) = inner(n.left)
        if(!leftOk) return (false, 0)
        val (rightOk, rightHeight) = inner(n.right)
        if(!rightOk) return (false, 0)
        val balance = rightHeight - leftHeight
        if(!(-1 <= balance && balance <= 1)) return (false, 0)
        (true, 1 + (leftHeight max rightHeight))
      }
    }
    inner(node)._1
  }


  /**
   * Print the tree in a nicely formatted multi-line string.
   */
  def prettyString: String = prettyString(root)

  def prettyString(subTreeRoot: Node[Key,Value]): String = {
    val s = new scala.collection.mutable.StringBuilder()
    def sep = " "
    def inner(node: Node[Key,Value], indent: String): Unit = {
      s ++= indent
      if(node == null) {
        s ++= "null\n"
      } else {
        s ++= s"key=${node.key} -> value=${node.value}\n"
        inner(node.left, indent+sep)
        inner(node.right, indent+sep)
      }
    }
    inner(subTreeRoot, "")
    s.toString
  }
}
