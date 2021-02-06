package org.hungerford.fp.basic

import org.hungerford.fp.types.MonadTest

class FpOptionTest extends MonadTest[ FpOption ](
    "FpOption",
    FpOption,
    List(
        FpNone,
        FpSome( 0 ),
        FpSome( -1 ),
        FpSome( 1000 ),
        FpSome( -1000 ),
        FpSome( 345345653 ),
        FpSome( -323432349 )
    )
)
