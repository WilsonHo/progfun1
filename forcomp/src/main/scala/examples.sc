import forcomp.Anagrams.Occurrences

import scala.io.Source

object x {

  val in = Source.fromURL("http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt");
  val words = in.getLines.toList filter (word => word forall (char => char.isLetter))
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI"
    , '5' -> "JKL", '6' -> "MNPO", '7' -> "PQRS"
    , '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for ((digit, str) <- mnem
         ; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String =
    word.toUpperCase map charCode

  wordCode("java")

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] =
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet

  encode("7225247386")

  def translate(number: String): Set[String] =
    encode(number) map (_ mkString " ")

  translate("7225247386")

  def f1(x: String): Char = {
    if (x.charAt(0) == 'o') 'a'
    else 'b'
  }

  val words1 = List("one", "two", "one", "three", "four", "two", "one")
  words1 groupBy f1

  "one".charAt(0)

  //  def abcd(abc: List[(Char, Int)]): List[Vector()] = abc match {
  //    case Nil => List()
  //    case x :: xs => (x match {
  //      case (c, 1) => List((c, 1))
  //      case (c, a) => (c, a) :: abcd(List((c, a - 1)))
  //    }) :: abcd(xs)
  //    //        abc(x) ::: abc(xs)
  //  }

  //  abcd()

  val a = List(('a', 2), ('b', 2))

  def combinations(occurrences: Occurrences): List[Occurrences] =
    (occurrences map (
      occ => ((1 to occ._2) map (count => (occ._1, count))) toList
      )).foldLeft(List(List[(Char, Int)]()))((occ1, occ2) =>
      occ1 ::: (
        for {
          o1 <- occ1
          o2 <- occ2
        } yield o1 ::: List(o2)
        )

    )

  println(combinations(a))
}
