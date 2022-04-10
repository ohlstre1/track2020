package bst

import scala.reflect.ClassTag

/**
 * An ordered map class, implemented with unbalanced BSTs.
 */
class TreeMap[Key : ClassTag, Value](implicit ord: Ordering[Key]) {
  /*
   * The root of the tree.
   * If the tree is empty, then this should be null.
   * In reality, this would be a private member but we keep it public
   * for the sake of the testing and grading processes.
   */
  var root: Node[Key, Value] = null

  /*
   * In order to have constant time "size" and "isEmpty" queries,
   * we keep count of how many keys the tree currently has.
   * Remember to update this when you insert/remove keys.
   */
  private var _nofKeys = 0
  /** The size of the tree */
  def size: Int = _nofKeys
  /** Is the tree empty? */
  def isEmpty = (_nofKeys == 0)

  /**
   * Insert the (key,value) mapping in the tree.
   * If the key already has a value, replace it with the new one.
   * Returns the old value of the key or None if it did not have one.
   * Should work in time O(h), where h is the height of the tree.
   * Remember to update the _nofKeys counter.
   */
  def insert(key: Key, value: Value): Option[Value] = {
    
      def inner(node :Node[Key, Value]) :Option[Value]= {
        var tempCompare = ord.compare(key, node.key)    
        if(tempCompare < 0){
                
                if(!node.hasLeft){
                  node.left = new Node(key,value)
                  _nofKeys +=1
                  return None
                }else{
                  inner(node.left)
                }
              }
         else if(tempCompare > 0){
               
              if(!node.hasRight){ 
                 node.right_= (new Node(key,value))
                 _nofKeys +=1
                 return None
               }else{
                 inner(node.right)
               }
            }
         else if(tempCompare == 0){
              val result =node.value
              node.value = value 
              return Some(result)
            
          }else return None
        }
      //inner functionen tar slut
    if(root == null){
          root = new Node(key,value)
          _nofKeys +=1
          return None
    }else inner(root)
    
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
   * Return all the keys in the tree in an array, sorted in ascending order.
   */
  def toArray: Array[Key] = {
    val result = new Array[Key](_nofKeys)
    var i = 0
    def inner(node: Node[Key,Value]): Unit = {
      if(node.hasLeft) inner(node.left)
      result(i) = node.key
      i += 1
      if(node.hasRight) inner(node.right)
    }
    inner(root)
    assert(i == _nofKeys)
    result
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
   def inner(node : Node[Key, Value]) : Option[Value] = {
      if(ord.compare(key, node.key) > 0) {
        if(node.hasRight) inner(node.right) 
        else return None
      } else if(ord.compare(key, node.key) < 0) {
        if(node.hasLeft) inner(node.left) 
        else return None
      } else {
          if(node.hasLeft && !node.hasRight) {
          substWith(node, node.left)
        } else if(node.hasRight && !node.hasLeft) {
          substWith(node, node.right)
        } else {
          
        }
      }
      return Some(node.value)
    }
    if(root == null) None else inner(root)
  }


  /**
   * Check whether the BST property holds in the tree, i.e., for each node
   * the descendants in the left sub-tree should be less than the key, and
   * the descendants in the right sub-tree should be greater than the key.
   * Linear in the size of the tree, only for debuggin and validation purposes.
   */
  def isValidBST(): Boolean = {
    def inner(n: Node[Key,Value], lower: Option[Key], upper: Option[Key]): Boolean = {
      if(n == null) true
      else if(lower.nonEmpty && ord.compare(n.key, lower.get) <= 0) false
      else if(upper.nonEmpty && ord.compare(n.key, upper.get) >= 0) false
      else inner(n.left, lower, Some(n.key)) && inner(n.right, Some(n.key), upper)
    }
    inner(root, None, None)
  }



  /**
   * Print the tree in a nicely formatted multi-line string.
   * Can be used for debugging ;)
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
