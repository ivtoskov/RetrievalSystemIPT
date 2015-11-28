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
   */
  def add(value: Double, docName: String): Unit = {
    // TODO Implement add method
  }

  /**
   * This method removes and returns the maximum
   * element contained in the data structure.
   *
   * @return The name of the currently best ranked document.
   */
  def removeMax(): String = {
    "" // TODO Implement removeMax method
  }
}
