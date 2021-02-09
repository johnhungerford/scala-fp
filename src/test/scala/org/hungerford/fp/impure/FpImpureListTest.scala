package org.hungerford.fp.impure

import org.hungerford.fp.types.MonadTest

class FpImpureListTest extends MonadTest[ FpImpureList ](
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
