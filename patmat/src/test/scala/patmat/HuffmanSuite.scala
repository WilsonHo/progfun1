package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
    val a = Leaf('a', 9)
    val c = Leaf('c', 10)
    val ac = makeCodeTree(a, c)
    val d = Leaf('d', 6)
    val e = Leaf('e', 7)
    val de = makeCodeTree(d, e)
    val b = Leaf('b', 15)
    val bde = makeCodeTree(de, b)
    val acbde = makeCodeTree(ac, bde)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of a larger acbde") {
    new TestTrees {
      assert(weight(acbde) === 47)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of a larger acbde") {
    new TestTrees {
      assert(chars(acbde) === List('a', 'c', 'd', 'e', 'b'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for some frequency table acbde") {
    assert(makeOrderedLeafList(List(('a', 9), ('b', 15), ('c', 10), ('d', 6), ('e', 7))) === List(Leaf('d', 6), Leaf('e', 7), Leaf('a', 9), Leaf('c', 10), Leaf('b', 15)))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("times of list") {
    new TestTrees {
      val chars = string2Chars("ABABBCBBDEEEABABBAEEDDCCABBBCDEEDCBCCCCDBBBCAAA")
      assert(makeOrderedLeafList(times(chars)) === makeOrderedLeafList(List(('A', 9), ('B', 15), ('C', 10), ('D', 6), ('E', 7))))
    }
  }

  test("singleton of list `trees` 1 ") {
    new TestTrees {
      assert(singleton(List(a)) === true)
    }
  }

  test("singleton of list `trees` 2 ") {
    new TestTrees {
      assert(singleton(List(acbde, a, b)) === false)
    }
  }

  test("combine") {
    new TestTrees {
      val leaflist = makeOrderedLeafList(List(('a', 9), ('b', 15), ('c', 10), ('d', 6), ('e', 7)))
      assert(combine(leaflist) === List(a, c, de, b))
    }
  }

  test("until") {
    new TestTrees {
      val chars = string2Chars("ABABBCBBDEEEABABBAEEDDCCABBBCDEEDCBCCCCDBBBCAAA" map (_.toLower))
      assert(until(singleton, combine)(makeOrderedLeafList(times(chars))) === acbde)
    }
  }

  test("createCodeTree") {
    new TestTrees {
      val tmp = "ABABBCBBDEEEABABBAEEDDCCABBBCDEEDCBCCCCDBBBCAAA" map(_.toLower)
      val chars = string2Chars(tmp)
      assert(createCodeTree(chars) === acbde)
    }
  }

}
