package org.hungerford.fp.collections

import org.hungerford.fp.types.MonadTest

class FpListMonadTest extends MonadTest[ FpList ](
    "FpList",
    FpList,
    List(
        1 +: 2 +: 3 +: 4 +: 5 +: FpNil,
        FpNil,
        -1 +: 1000 +: -3423324 +: 2324 +: FpNil,
    )
)

class FpListSeqTest extends FpSeqTest[ FpList ](
    "FpList",
    List( FpNil )
)
