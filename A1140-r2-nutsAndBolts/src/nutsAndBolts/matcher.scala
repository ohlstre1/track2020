/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package nutsAndBolts
import scala.reflect.ClassTag

object matcher {
  /**
   * Find matching pairs for the nuts and bolts.
   * A straightforward, quadratic-time algorithm for reference
   * when measuring performance.
   */
  def slow[NutType <% Nut : ClassTag, BoltType <% Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    val result = scala.collection.mutable.ArrayBuffer[(NutType,BoltType)]()
    val unmatchedBolts = bolts.toArray
    var nofUnmatchedBolts = unmatchedBolts.length
    var i = 0
    while(i < nuts.length && nofUnmatchedBolts > 0) {
      val nut = nuts(i)
      var boltFound = false
      var j = 0
      while(j < nofUnmatchedBolts && boltFound == false) {
        val bolt = unmatchedBolts(j)
        if(nut.compare(bolt) == 0) {
          boltFound = true
          // Found a matching bolt, append the pair to the result
          result += ((nut, bolt))
          // (Pseudo)remove the bolt from the unmatched array by
          // moving it to the end of the current subarray and
          // shrinking the subarray
          nofUnmatchedBolts -= 1
          assert(nofUnmatchedBolts >= 0 && j <= nofUnmatchedBolts)
          unmatchedBolts(j) = unmatchedBolts(nofUnmatchedBolts)
          unmatchedBolts(nofUnmatchedBolts) = bolt
        } else
          j += 1
      }
      i += 1
    }
    result.toIndexedSeq
  }


  /**
   * The faster algorithm based on a variant of quicksort.
   */
  
  def partition[NutType <% Nut : ClassTag, BoltType <% Bolt : ClassTag](arr: Array[BoltType], lo: Int, hi: Int, pivo : Nut): Int = {
    var i,j = lo
    val temp = Array[BoltType]()
    while(j < hi){
      if(arr(j).compare(pivo) < 0){
        temp(0) = arr(i)
        arr(i) = arr(j)
        arr(j) = temp(0)
        i += 1
      }else if(arr(j) == pivo){
        temp(0) = arr(j) 
        arr(j) = arr(hi); 
        arr(hi) = temp(0); 
        j -= 1 
      }
    j+=1
    }
    temp(1) = temp(0)
    arr(i) = arr(hi)
    arr(hi) = temp(1)
    return i
  }
  
  def fast[NutType <% Nut : ClassTag, BoltType <% Bolt : ClassTag](nuts: IndexedSeq[NutType], bolts: IndexedSeq[BoltType]): IndexedSeq[(NutType,BoltType)] = {
    val nut = nuts.toArray
    val bolt = bolts.toArray
    def inner(low : Int,high :Int){
      if(low<high){
        val pivo = this.partition(nut, low, high, bolt.drop)
      }
    }
  }
}
