package org.hungerford.fp.collections

import org.hungerford.fp.types.{MonadTest, TypedMonoidTest}

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

class FpListMonoidTest extends TypedMonoidTest[ FpList, Int ](
    "FpList",
    FpList,
    List(
        1 +: 2 +: 3 +: 4 +: 5 +: FpNil,
        -2342 +: 34532 +: FpNil,
        FpNil,
        0 +: FpNil,
        199 +: -300 +: 342334 +: -4323423 +: 43523542 +: FpNil,
        (1 +: 2 +: 3 +: FpNil).times( 5000 ),
    )
)
