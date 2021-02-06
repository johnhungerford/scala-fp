//package org.hungerford.fp.impure
//
//import org.scalatest.flatspec.AnyFlatSpecLike
//import org.scalatest.matchers.should.Matchers
//
//class FpImpureListTest extends AnyFlatSpecLike with Matchers {
//    val test : FpImpureList[ Int ] = 1 +: FpImpureList( test.map( _ + 2 ) )
//
//    behavior of "Recursive FpImpureList"
//
//    it should "flatMap recursive list without getting stuck in loop" in {
//        test.take( 4 ).toFpList shouldBe ( 1 +: 2 +: 3 +: 4 +: FpImpureNil )
//    }
//}
