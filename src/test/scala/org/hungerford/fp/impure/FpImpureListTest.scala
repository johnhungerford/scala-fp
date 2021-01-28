package org.hungerford.fp.impure

import org.hungerford.fp.collections.FpNil
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FpImpureListTest extends AnyFlatSpecLike with Matchers {
    val test : FpImpureList[ Int ] = FpImpureList( test.map( _ + 2 ) ) + 1

    behavior of "Recursive FpImpureList"

    it should "flatMap recursive list without getting stuck in loop" in {
        test.take( 4 ).toFpList shouldBe FpNil + 1 + 3 + 5 + 7
    }
}
