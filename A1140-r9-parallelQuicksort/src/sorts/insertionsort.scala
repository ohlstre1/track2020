package sorts

object insertionsort {
  /**
   * Use insertion sort to sort the subarray a[start...end].
   * Works in-place.
   */
  def sort(a: Array[Int], start: Int, end: Int): Unit = {
    require(0 <= start && start <= end && end < a.length)
    var i = start+1
    while (i <= end) {
      val key = a(i)
      var j = i;
      while (j > start && a(j - 1) > key) {
        a(j) = a(j - 1)
        j -= 1
      }
      a(j) = key
      i += 1
    }
  }
  /**
   * Use insertion sort to sort the array a.
   */
  def sort(a: Array[Int]): Unit = sort(a, 0, a.length-1)
}
