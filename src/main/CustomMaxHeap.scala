package main

/**
 * A data structure that can store a fixed number of
 * elements with highest values.
 *
 * @param capacity The capacity of the heap.
 *
 * @author Ivaylo Toskov
 */
class CustomMaxHeap(capacity: Int) {
  // The elements with the highest score.
  val scores = new Array[Double](capacity)
  // The names of the elements with the highest score.
  val names = new Array[String](capacity)
  // The number of elements contained in the heap
  var size = 0
  /**
   * This method tries to add new value to the heap.
   * If the capacity is not reached yet, the value is
   * automatically added. Otherwise the value is only added
   * if it is higher than the minimum stored element. In this
   * case it gets inserted and the minimum is thrown away so that
   * the capacity is not exceeded.
   *
   * @param value The score value that may get inserted.
   * @param docName The DOCNO of the document that corresponds to the score.
   * @return true if the document was added, false otherwise.
   */
  def add(value: Double, docName: String): Boolean = {
    if(size < capacity) {
      scores(size) = value
      names(size) = docName
      size += 1
    }
    if(value >= scores(size - 1)) {
      scores(size - 1) = value
      names(size - 1) = docName
      var index = size - 1
      while(index >= 1 && scores(index) > scores(index - 1)) {
        val temp = scores(index)
        val tempName = names(index)
        scores(index) = scores(index - 1)
        scores(index - 1) = temp
        names(index) = names(index - 1)
        names(index - 1) = tempName
        index -= 1
      }
      true
    } else {
      false
    }
  }

  /**
   * This method returns the best documents ordered depending on
   * their score. The document on the 0th position has the
   * best score, the document on the 1st has the second best etc.
   *
   * @return List of documents ordered by their score.
   */
  def returnDocuments: List[String] = {
    names.toList
  }
}
