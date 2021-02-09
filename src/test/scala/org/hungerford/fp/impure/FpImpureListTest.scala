package org.hungerford.fp.impure

import org.hungerford.fp.collections.FpSeqTest
import org.hungerford.fp.types.MonadTest

class FpImpureListMonadTest extends MonadTest[ FpImpureList ](
    "FpImpureList",
    FpImpureList,
    List(
        1 +: 2 +: 3 +: 4 +: FpImpureNil,
        1 +: 2 +: 3 +: 4 +: FpImpureFail( new NullPointerException( "end of list" ) ),
        FpImpureNil,
        FpImpureFail( new IllegalStateException( "no list" ) ),
        -234525234 +:: FpImpureNil
    )
)

class FpImpureListSeqTest extends FpSeqTest[ FpImpureList ](
    "FpImpureList",
    List(
        FpImpureNil,
        FpImpureFail( new Exception( "test" ) ),
        FpImpureFail( new NullPointerException( "these should not exist" ) ),
        FpImpureFail( new IllegalStateException( "exceptions? here??" ) ),
    )
)
