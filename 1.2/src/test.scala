object test {


def hasFactors(a: Array[Int], t: Int): Boolean = {
  var i = 0
  var found = false
  val n = a.length
  while (i < n && !found) {
    val v = a(i)
    var j = 1
    while (j < n && !found) {
      if(v * a(j) == t)
        found = true
      j += 1
    }
    i += 1
  }
  return found
}

}