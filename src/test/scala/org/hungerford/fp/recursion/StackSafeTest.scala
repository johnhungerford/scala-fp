package org.hungerford.fp.recursion

import org.hungerford.fp.types.MonadTest

class StackSafeTest extends MonadTest[ StackSafe ](
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
