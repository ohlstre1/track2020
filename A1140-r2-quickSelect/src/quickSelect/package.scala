/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package object quickSelect {
  // (Pseudo)random number generator with a fixed seed so that
  // error situations can be reproduced easier
  val rand = new scala.util.Random(21)

  
  def swap(a: Array[Int], i: Int, j: Int): Unit = {
  val temp = a(i); a(i) = a(j); a(j) = temp
}

def partition(a: Array[Int], lo: Int, hi: Int, pivo : Int): (Int,Int) = {
  var i,m = lo // m stans for the median value important for efficency
  var j = hi
  while(m <= j) {
    if( a(m) < pivo ){   
      swap(a,i,m)
      i += 1
      m += 1
    }else if( a(m) > pivo){
      swap(a,m,j)
      j -= 1
    }else{
      m += 1
    }
  }
  (i,j)
}
  /**
   * Find the k:th smallest (0 for the smallest) element in
   * the integer sequence seq by using the quickselect algorithm.
   */
  def find(seq: Seq[Int], k: Int): Int = {
    require(0 <= k && k < seq.length)
    val a: Array[Int] = seq.toArray
    
    def inner(arr: Array[Int],left :Int,right :Int) : Int ={
       if(left >= right) return arr(left)
     
      val index = left + rand.nextInt((right - left) + 1) // stops worst case (when its already sorted)
      swap(arr, index, right)
      val (l, r) = partition(arr, left, right - 1, arr(right))
      swap(arr, r + 1, right)
     
      if(k >= l && k <= r) return arr(k)
      else if(l > k) return inner(arr, left, l - 1)
      else return inner(arr, r + 1, right)
      
    }
    
    inner(a,0,a.length-1)
  }
}
