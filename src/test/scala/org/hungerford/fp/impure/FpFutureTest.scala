package org.hungerford.fp.impure

import org.hungerford.fp.types.MonadCovariantTest

import scala.concurrent.ExecutionContext.Implicits.global

class FpFutureTest extends MonadCovariantTest(
    "FpFuture",
    FpFuture,
    List(
        FpFuture { println( "future test 1" ); 1 },
        FpFuture { println( "future test 0" ); 0 },
        FpFuture { println( "future test -1" ); -1 },
        FpFuture {
            Thread.sleep( 3000 )
            println( "future test 2343234" )
            2343234
        },
        FpFuture {
            println( "future test -23523423" )
            -23523423
        },
    )
) {

}
