package org.hungerford.fp.collections

import org.hungerford.fp.basic.{FpNone, FpSome}
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FpSeqTest[ T[ +_ ] <: FpSeq[ _ ] ]( className : String, termina : List[ T[ Nothing ] ] ) extends AnyFlatSpecLike with Matchers {

    val intLists1 : Seq[ T[ Int ] ] = for {
        end <- termina
    } yield (1 +: 2 +: 3 +: 4 +: 5 +: end).asInstanceOf[ T[ Int ] ]

    val stringLists1 : Seq[ T[ String ] ] = for {
        end <- termina
    } yield ("hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: end).asInstanceOf[ T[ String ] ]

    behavior of s"FpSeq: $className"

    it should "successfully access index using apply method" in {
        for { intList <- intLists1 } {
            intList( 2 ) shouldBe FpSome( 3 )
            intList( 4 ) shouldBe FpSome( 5 )
            intList( -1 ) shouldBe FpNone
            intList( 5 ) shouldBe FpNone
            intList( -23423432 ) shouldBe FpNone
            intList( 342323432 ) shouldBe FpNone
        }

        for { strList <- stringLists1 } {
            strList( 1 ) shouldBe FpSome( "is" )
            strList( 6 ) shouldBe FpSome( "for?" )
            strList( -1 ) shouldBe FpNone
            strList( 7 ) shouldBe FpNone
            strList( -34523452 ) shouldBe FpNone
            strList( 58645845 ) shouldBe FpNone
        }
    }

    it should "convert to FpList" in {
        for { intList <- intLists1 } {
            intList.toFpList shouldBe (1 +: 2 +: 3 +: 4 +: 5 +: FpNil)
        }

        for { strList <- stringLists1 } {
            strList.toFpList shouldBe ("hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: FpNil)
        }
    }

    it should "return correct values for headOption" in {
        for { intList <- intLists1 } {
            intList.headOption shouldBe FpSome( 1 )
        }

        for { strList <- stringLists1 } {
            strList.headOption shouldBe FpSome( "hello" )
        }

        for { end <- termina } {
            end.headOption shouldBe FpNone
        }
    }

    it should "return correct values for tailOption" in {
        for { intList <- intLists1 } {
            intList.tailOption.map( _.toFpList ) shouldBe FpSome( 2 +: 3 +: 4 +: 5 +: FpNil )
        }

        for { strList <- stringLists1 } {
            strList.tailOption.map( _.toFpList ) shouldBe FpSome( "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: FpNil )
        }

        for { end <- termina } {
            end.tailOption shouldBe FpNone
        }
    }

    it should "return correct values for lastOption" in {
        for { intList <- intLists1 } {
            intList.lastOption shouldBe FpSome( 5 )
        }

        for { strList <- stringLists1 } {
            strList.lastOption shouldBe FpSome( "for?" )
        }

        for { end <- termina } {
            end.lastOption shouldBe FpNone
        }
    }

    it should "append values correctly" in {
        for { intList <- intLists1 } {
            intList :+ 6 shouldBe (1 +: 2 +: 3 +: 4 +: 5 +: 6 +: termina.head)
        }

        for { strList <- stringLists1 } {
            strList :+ "..." :+ "well?" :+ "..." shouldBe ("hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: "..." +: "well?" +: "..." +: termina.head )
        }
    }
}
