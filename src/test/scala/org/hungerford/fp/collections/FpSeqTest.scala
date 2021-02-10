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

    it should "return correct values for tailOrNil" in {
        for { intList <- intLists1 } {
            intList.tailOrNil shouldBe 2 +: 3 +: 4 +: 5 +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.tailOrNil shouldBe "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: termina.head
        }

        for { end <- termina } {
            end.tailOrNil.length shouldBe 0
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

    it should "correctly determine whether sequence isEmpty or is nonEmpty" in {
        for { intList <- intLists1 } {
            intList.isEmpty shouldBe false
            intList.nonEmpty shouldBe true
        }

        for { strList <- stringLists1 } {
            strList.isEmpty shouldBe false
            strList.nonEmpty shouldBe true
        }

        for { end <- termina } {
            end.isEmpty shouldBe true
            end.nonEmpty shouldBe false
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

    it should "concat seqs correctly" in {
        for { intList <- intLists1 } {
            intList :++ intList.asInstanceOf[ FpSeq[ Int ] ] shouldBe (1 +: 2 +: 3 +: 4 +: 5 +: 1 +: 2 +: 3 +: 4 +: 5 +: termina.head)
            intList.asInstanceOf[ FpSeq[ Int ] ] ++: intList shouldBe (1 +: 2 +: 3 +: 4 +: 5 +: 1 +: 2 +: 3 +: 4 +: 5 +: termina.head)
        }

        for { strList <- stringLists1 } {
            strList :++ strList.asInstanceOf[ FpSeq[ String ] ] shouldBe ("hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: "hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: termina.head )
            strList.asInstanceOf[ FpSeq[ String ] ] ++: strList shouldBe ("hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: "hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: termina.head )
        }
    }

    it should "concat to FpString correctly" in {
        for { intList <- intLists1 } {
            intList.fpString( FpString( " + " ) ) shouldBe FpString( "1 + 2 + 3 + 4 + 5" )
            intList.fpString( ", " ) shouldBe FpString( "1, 2, 3, 4, 5" )
        }

        for { strList <- stringLists1 } {
            strList.fpString( FpString( " " ) ) shouldBe FpString( "hello is it me you're looking for?" )
            strList.fpString( " quack " ) shouldBe FpString( "hello quack is quack it quack me quack you're quack looking quack for?" )
        }
    }

    it should "reverse correctly" in {
        for { intList <- intLists1 } {
            intList.reverse shouldBe 5 +: 4 +: 3 +: 2 +: 1 +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.reverse shouldBe "for?" +: "looking" +: "you're" +: "me" +: "it" +: "is" +: "hello" +: termina.head
        }
    }

    it should "calculate correct length" in {
        for { intList <- intLists1 } {
            intList.length shouldBe 5
        }

        for { strList <- stringLists1 } {
            strList.length shouldBe 7
        }

        for { end <- termina } end.length shouldBe 0
    }

    it should "concat list to itself using 'times' correctly" in {
        for { intList <- intLists1 } {
            intList.times( 0 ) shouldBe termina.head
            intList.times( 1 ) shouldBe intList
            intList.times( 2 ) shouldBe 1 +: 2 +: 3 +: 4 +: 5 +: 1 +: 2 +: 3 +: 4 +: 5 +: termina.head
            intList.times( 50 ).length shouldBe 250
            intList.times( -234 ) shouldBe termina.head
        }

        for { strList <- stringLists1 } {
            strList.times( 0 ) shouldBe termina.head
            strList.times( 1 ) shouldBe strList
            strList.times( 2 ) shouldBe "hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: "hello" +: "is" +: "it" +: "me" +: "you're" +: "looking" +: "for?" +: termina.head
            strList.times( 50 ).length shouldBe 350
            strList.times( -234 ) shouldBe termina.head
        }

        for { end <- termina } {
            end.times( 0 ).length shouldBe 0
            end.times( 1 ).length shouldBe 0
            end.times( 2 ).length shouldBe 0
            end.times( 50 ).length shouldBe 0
            end.times( -234 ).length shouldBe 0
        }
    }

    it should "filter correctly" in {
        for { intList <- intLists1} {
            intList.asInstanceOf[ FpSeq[ Int ] ].filter( _ % 2 == 0 ) shouldBe 2 +: 4 +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].filter( _ < 4 ) shouldBe 1 +: 2 +: 3 +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].filter( _ > 10 ).length shouldBe 0
            intList.asInstanceOf[ FpSeq[ Int ] ].filter( _ >= 0 ) shouldBe intList
            intList.asInstanceOf[ FpSeq[ Int ] ].filter( _ < 100 ) shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].filter( _.length < 4 ) shouldBe "is" +: "it" +: "me" +: termina.head
            strList.asInstanceOf[ FpSeq[ String ] ].filter( _.contains( 'i' ) ) shouldBe "is" +: "it" +: "looking" +: termina.head
            strList.asInstanceOf[ FpSeq[ String ] ].filter( _.contains( 'z' ) ).length shouldBe 0
            strList.asInstanceOf[ FpSeq[ String ] ].filter( _.length > 0 ) shouldBe strList
        }
    }

    it should "take elements correctly" in {
        for { intList <- intLists1 } {
            intList.take( 2 ) shouldBe 1 +: 2 +: termina.head
            intList.take( 5 ) shouldBe intList
            intList.take( 0 ).length shouldBe 0
            intList.take( -23 ).length shouldBe 0
            intList.take( 1000 ) shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.take( 2 ) shouldBe "hello" +: "is" +: termina.head
            strList.take( 7 ) shouldBe strList
            strList.take( 0 ).length shouldBe 0
            strList.take( -23 ).length shouldBe 0
            strList.take( 1000 ) shouldBe strList
        }
    }

    it should "take elements correctly using takeWhile" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].takeWhile( _ < 3 ) shouldBe 1 +: 2 +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].takeWhile( _ <= 5 ) shouldBe intList
            intList.asInstanceOf[ FpSeq[ Int ] ].takeWhile( _ < 1 ).length shouldBe 0
            intList.asInstanceOf[ FpSeq[ Int ] ].takeWhile( _ < -23 ).length shouldBe 0
            intList.asInstanceOf[ FpSeq[ Int ] ].takeWhile( _ < 1000 ) shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].takeWhile( !_.contains( "'" ) ) shouldBe "hello" +: "is" +: "it" +: "me" +: termina.head
            strList.asInstanceOf[ FpSeq[ String ] ].takeWhile( !_.contains( "z" ) ) shouldBe strList
            strList.asInstanceOf[ FpSeq[ String ] ].takeWhile( _.contains( "z" ) ).length shouldBe 0
        }
    }

    it should "drop elements correctly" in {
        for { intList <- intLists1 } {
            intList.drop( 2 ) shouldBe 3 +: 4 +: 5 +: termina.head
            intList.drop( 5 ).length shouldBe 0
            intList.drop( 0 ) shouldBe intList
            intList.drop( -23 ) shouldBe intList
            intList.drop( 1000 ).length shouldBe 0
        }

        for { strList <- stringLists1 } {
            strList.drop( 2 ) shouldBe "it" +: "me" +: "you're" +: "looking" +: "for?" +: termina.head
            strList.drop( 7 ).length shouldBe 0
            strList.drop( 0 ) shouldBe strList
            strList.drop( -23 ) shouldBe strList
            strList.drop( 1000 ).length shouldBe 0
        }
    }

    it should "drop elements correctly using dropWhile" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].dropWhile( _ < 3 ) shouldBe 3 +: 4 +: 5 +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].dropWhile( _ <= 5 ).length shouldBe 0
            intList.asInstanceOf[ FpSeq[ Int ] ].dropWhile( _ < 1 ) shouldBe intList
            intList.asInstanceOf[ FpSeq[ Int ] ].dropWhile( _ < -23 ) shouldBe intList
            intList.asInstanceOf[ FpSeq[ Int ] ].dropWhile( _ < 1000 ).length shouldBe 0
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].dropWhile( !_.contains( "'" ) ) shouldBe "you're" +: "looking" +: "for?" +: termina.head
            strList.asInstanceOf[ FpSeq[ String ] ].dropWhile( !_.contains( "z" ) ).length shouldBe 0
            strList.asInstanceOf[ FpSeq[ String ] ].dropWhile( _.contains( "z" ) ) shouldBe strList
        }
    }

    it should "slice correctly" in {
        for { intList <- intLists1 } {
            intList.slice( 0, 5 ) shouldBe intList
            intList.slice( 0, 7 ) shouldBe intList
            intList.slice( -1, 5 ) shouldBe intList
            intList.slice( -10000, 10000 ) shouldBe intList
            intList.slice( 0, 2 ) shouldBe 1 +: 2 +: termina.head
            intList.slice( 1, 2 ) shouldBe 2 +: termina.head
            intList.slice( 2, 2 ).length shouldBe 0
            intList.slice( 3, 2 ).length shouldBe 0
            intList.slice( 100, -100 ).length shouldBe 0
            intList.slice( 2, 4 ) shouldBe 3 +: 4 +: termina.head
            intList.slice( 2, 5 ) shouldBe 3 +: 4 +: 5 +: termina.head
            intList.slice( 2, 6 ) shouldBe 3 +: 4 +: 5 +: termina.head
        }
    }

    it should "correctly determine whether a condition exists in a sequence" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].exists( _ == 1 ) shouldBe true
            intList.asInstanceOf[ FpSeq[ Int ] ].exists( _ == 0 ) shouldBe false
            intList.asInstanceOf[ FpSeq[ Int ] ].exists( _ < 4 ) shouldBe true
            intList.asInstanceOf[ FpSeq[ Int ] ].exists( _ < 1 ) shouldBe false
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _ == "looking" ) shouldBe true
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _ == "asdfas" ) shouldBe false
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _.length > 2 ) shouldBe true
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _.length < 2 ) shouldBe false
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _.contains( "'" ) ) shouldBe true
            strList.asInstanceOf[ FpSeq[ String ] ].exists( _ == "z" ) shouldBe false
        }
    }

    it should "correctly determine whether a sequence contains an element" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].contains( 1 ) shouldBe true
            intList.asInstanceOf[ FpSeq[ Int ] ].contains( 0 ) shouldBe false
            intList.asInstanceOf[ FpSeq[ Int ] ].contains( 5 ) shouldBe true
            intList.asInstanceOf[ FpSeq[ Int ] ].contains( -344 ) shouldBe false
            intList.asInstanceOf[ FpSeq[ Int ] ].contains( 6 ) shouldBe false
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].contains( "looking" ) shouldBe true
            strList.asInstanceOf[ FpSeq[ String ] ].contains( "asdfas" ) shouldBe false
        }
    }

    it should "filter out repeated elements using distinct method, preserving order" in {
        for { intList <- intLists1 } {
            intList.distinct shouldBe intList
            intList.times( 2 ).distinct shouldBe intList
            intList.times( 1000 ).distinct shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.distinct shouldBe strList
            strList.times( 2 ).distinct shouldBe strList
            strList.times( 1000 ).distinct shouldBe strList
        }

        (1 +: 1 +: 3 +: 2 +: 1 +: 3 +: 1 +: 2 +: termina.head).distinct shouldBe 1 +: 3 +: 2 +: termina.head
    }

    it should "partition elements into a 2-tuple of sequences, with the elements passing a condition being on the left sequence" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].partition( _ % 2 == 0 ) shouldBe(2 +: 4 +: termina.head, 1 +: 3 +: 5 +: termina.head)
            intList.asInstanceOf[ FpSeq[ Int ] ].partition( _ < 4 ) shouldBe(1 +: 2 +: 3 +: termina.head, 4 +: 5 +: termina.head)
            val (t1l, t1r) = intList.asInstanceOf[ FpSeq[ Int ] ].partition( _ > 10 )
            t1l.length shouldBe 0
            t1r shouldBe intList
            val (t2l, t2r) = intList.asInstanceOf[ FpSeq[ Int ] ].partition( _ >= 0 )
            t2l shouldBe intList
            t2r.length shouldBe 0
            val (t3l, t3r) = intList.asInstanceOf[ FpSeq[ Int ] ].partition( _ < 100 )
            t3l shouldBe intList
            t3r.length shouldBe 0
        }

        for { strList <- stringLists1 } {
            val (t1l, t1r) = strList.asInstanceOf[ FpSeq[ String ] ].partition( _.length < 4 )
            t1l shouldBe "is" +: "it" +: "me" +: termina.head
            t1r shouldBe "hello" +: "you're" +: "looking" +: "for?" +: termina.head
            val (t2l, t2r) = strList.asInstanceOf[ FpSeq[ String ] ].partition( _.contains( 'i' ) )
            t2l shouldBe "is" +: "it" +: "looking" +: termina.head
            t2r shouldBe "hello" +: "me" +: "you're" +: "for?" +: termina.head
            val (t3l, t3r) = strList.asInstanceOf[ FpSeq[ String ] ].partition( _.contains( 'z' ) )
            t3l.length shouldBe 0
            t3r shouldBe strList
            val (t4l, t4r) = strList.asInstanceOf[ FpSeq[ String ] ].partition( _.length > 0 )
            t4l shouldBe strList
            t4r.length shouldBe 0
        }
    }

    it should "filter and map a sequence using collect" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].collect {
                case i if i % 2 == 0 => i * 3
            } shouldBe 6 +: 12 +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].collect {
                case i if i % 2 == 0 => s"$i is even"
                case i => s"$i is odd"
            } shouldBe "1 is odd" +: "2 is even" +: "3 is odd" +: "4 is even" +: "5 is odd" +: termina.head
            intList.asInstanceOf[ FpSeq[ Int ] ].collect {
                case i if i > 10 => i * 1000
            }.length shouldBe 0
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ] collect {
                case "hello" => "salutations"
                case "is" => "tis"
                case "me" => "I"
                case "you're" => "you"
                case "looking" => "seek"
            } shouldBe "salutations" +: "tis" +: "I" +: "you" +: "seek" +: termina.head
        }
    }

    it should "sort elements" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].reverse.sort shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].sort shouldBe "for?" +: "hello" +: "is" +: "it" +: "looking" +: "me" +: "you're" +: termina.head
        }
    }

    it should "sort elements using sortBy" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].sortBy( _ % 4 ) shouldBe 4 +: 1 +: 5 +: 2 +: 3 +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].sortBy( _.length ) shouldBe "is" +: "it" +: "me" +: "for?" +: "hello" +: "you're" +: "looking" +: termina.head
        }
    }

    it should "sort elements using sortWith" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].reverse.sortWith( _ - _ ) shouldBe intList
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].sortWith( _.length - _.length ) shouldBe "is" +: "it" +: "me" +: "for?" +: "hello" +: "you're" +: "looking" +: termina.head
        }
    }

    it should "zip sequences together" in {
        for {
            intList <- intLists1
            strList <- stringLists1
        } {
            intList.zipWith( strList.asInstanceOf[ FpSeq[ String ] ] ) shouldBe (1, "hello") +: (2, "is") +: (3, "it") +: (4, "me") +: (5, "you're") +: termina.head
        }
    }

    it should "zip sequences with their elements' indices" in {
        for { intList <- intLists1 } {
            intList.zipWithIndex shouldBe (1, 0) +: (2, 1) +: (3, 2) +: (4, 3) +: (5, 4) +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.zipWithIndex shouldBe ("hello", 0) +: ("is", 1) +: ("it", 2) +: ("me", 3) +: ("you're", 4) +: ("looking", 5) +: ("for?", 6) +: termina.head
        }

    }

    it should "be able to map with elements to the left" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].mapWithLeft( 1 )( _ * _ ) shouldBe 1 +: 2 +: 6 +: 12 +: 20 +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].mapWithLeft( "" )( _ + " " + _ ) shouldBe " hello" +: "hello is" +: "is it" +: "it me" +: "me you're" +: "you're looking" +: "looking for?" +: termina.head
        }
    }

    it should "be able to map with elements to the right" in {
        for { intList <- intLists1 } {
            intList.asInstanceOf[ FpSeq[ Int ] ].mapWithRight( 100 )( _ * _ ) shouldBe 2 +: 6 +: 12 +: 20 +: 500 +: termina.head
        }

        for { strList <- stringLists1 } {
            strList.asInstanceOf[ FpSeq[ String ] ].mapWithRight( "" )( _ + " " + _ ) shouldBe "hello is" +: "is it" +: "it me" +: "me you're" +: "you're looking" +: "looking for?" +: "for? " +: termina.head
        }
    }
}

