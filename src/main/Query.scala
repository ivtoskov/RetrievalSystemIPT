package main

/**
 *  A model class that corresponds to a topic query.
 *  E.g. num = 051, tokens = ["airbus", "subsidies"].
 *
 * @param num The number that uniquely identifies the topic query.
 * @param tokens The tokens of the corresponding topic query.
 *
 * @author Ivaylo Toskov
 */
class Query(val num: Integer, val tokens: List[String]) {}
