/* Author: Tommi Junttila, Aalto University.
 * Only for the use on the Aalto course CS-A1140.
 * Redistribution not allowed.
 */

package allAnagrams

/**
 * A simple class for a tool that effciently finds anagrams
 * included in the given dictionary.
 * The initialization phase can take small amount of time but
 * the single queries with the 'find' method should be very fast.
 */
class AnagramFinder(val dictionary: Seq[String]) {
  // Because/if your solution needs initialization code or data structures
  // (a scala.collection.mutable.HashMap perhaps), insert them here
  var hashDictionary = new scala.collection.mutable.HashMap[String, scala.collection.mutable.ListBuffer[String]]()
  for(i <- dictionary){ //i = each word in dict
    val key = i.sorted
    if(hashDictionary.contains(key)){
      hashDictionary(key) += i
    }
    else{
      hashDictionary(key) = (scala.collection.mutable.ListBuffer(i))
    }
  }
  /** Find all the anagrams of the the word in the dictionary */
  def find(word: String): Set[String] = {
    return hashDictionary(word.sorted).toSet
  }
}
