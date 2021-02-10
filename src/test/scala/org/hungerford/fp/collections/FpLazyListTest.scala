package org.hungerford.fp.collections

import org.hungerford.fp.basic.FpSome
import org.hungerford.fp.types.{MonadTest, TypedMonoidTest}

class FpLazyListMonadTest extends MonadTest(
    "FpLazyList",
    FpLazyList,
    List(
       1 +: 2 +: 3 +: FpLazyNil,
       FpLazyNil,
       FpLazyList.makeLazy( FpLazyNil ),
       FpLazyList.makeLazy( 1 +: 2 +: 3 +: 4 +: FpLazyNil ),
       FpLazyList.makeLazy( FpLazyList.makeLazy( FpLazyList.makeLazy( FpLazyListEval( 100000, -1 +: 34534 +: 2 +: FpLazyNil ) ) ) )
    ),
) {
    behavior of "FpLazyList recursion"

    it should "permit construction of infinite recursive list with freezing or blowing stack" in {
        lazy val test : FpLazyList[ Int ] = 1 +: FpLazyList.makeLazy( test.map( _ + 1 ) )
    }

    it should "permit access of high index in infinite recursive list without freezing or blowing stack" in {
        lazy val test : FpLazyList[ Int ] = 0 +: FpLazyList.makeLazy( test.map( _ + 1 ) )
        test( 10000 ) shouldBe FpSome( 10000 )
    }

    it should "be able to go even higher when not recursive" in {
        lazy val test : FpLazyList[ Int ] = {
            def next( i : Int ) : FpLazyList[ Int ] = i +:: FpLazyList.makeLazy( next( i + 1 ) )
            next( 0 )
        }

        test( 1000000 ) shouldBe FpSome( 1000000 )
    }
}

class FpLazyListSeqTest extends FpSeqTest[ FpLazyList ](
    "FpLazyList",
    List( FpLazyNil ),
)

class FpLazyListMonoidTest extends TypedMonoidTest[ FpLazyList, Int ](
    "FpLazyList",
    FpLazyList,
    List(
        1 +: 2 +: 3 +: 4 +: 5 +: FpLazyNil,
        1 +:: 2 +:: 3 +:: 4 +:: 5 +:: FpLazyNil,
        -2342 +: 34532 +: FpLazyNil,
        FpLazyNil,
        0 +: FpLazyNil,
        199 +: -300 +: 342334 +:: -4323423 +: 43523542 +: FpLazyNil,
        (1 +:: 2 +:: 3 +:: FpLazyNil).times( 5000 ),
    )
)
