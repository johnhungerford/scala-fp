package org.hungerford.fp.recursion

import org.hungerford.fp.types.MonadCovariantTest

class StackSafeTest extends MonadCovariantTest(
    "StackSafe",
    StackSafe,
    List(
        Result( 1 ),
        Result( 0 ),
        Result( -1 ),
        Call.from( Result( 2 ) ),
        FlatMap[ Double, Int ]( Call.from( Result( -1.02342 ) ), i => Result( Math.ceil( i * 300 ).toInt ) ),
    ),
) {

}
