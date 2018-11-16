package objsets

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import sun.invoke.empty.Empty

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val a = new Tweet("a", "a body", 20)
    val set1 = new Empty
    val set2 = set1.incl(a)
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val singletonTweet = new NonEmpty(c, new Empty, new Empty)
    val tweetSet = new NonEmpty(c, set1.incl(a), new Empty)
    val tweetSet1 = new NonEmpty(c, set1.incl(a), set1.incl(d))
    val tweetSet2 = new NonEmpty(c, new Empty, set1.incl(d))
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }

  test("mostRetweeted: the root of a single element TweetSet should be the 'mostRetweeted Tweet'") {
    new TestSets {
      assert(singletonTweet.mostRetweeted === c)
    }
  }

  test("mostRetweeted: testing a TweeetSet with a leaf that has more retweets than the root") {
    new TestSets {
      assert(tweetSet.mostRetweeted === a)
    }
  }

  test("mostRetweeted: a fully formed TweeetSet") {
    new TestSets {
      assert(tweetSet1.mostRetweeted === a)
    }
  }

  test("mostRetweeted: only with a right leaf") {
    new TestSets {
      assert(tweetSet2.mostRetweeted === d)
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

}
