package org.hungerford.fp.impure

import org.hungerford.fp.types.MonadCovariantTest

import scala.concurrent.ExecutionContext.Implicits.global

class FpImpureFutureTest extends MonadCovariantTest(
    "FpImpureFuture",
    FpImpureFuture,
    List[ FpImpureFuture[ Int ] ](
        FpImpureFuture[ Int ](
            FpImpure {
                FpFuture( 1 )
            }
        ),
        FpImpureFuture[ Int ] {
            FpImpure {
                println( "testing fpImpureFuture - impure part" )
                FpFuture {
                    println( "testing fpImpureFuture - future part" )
                    0
                }
            }
        },
        FpImpureFuture[ Int ] {
            FpImpure {
                println( "testing fpImpureFuture - impure part" )
                FpFuture {
                    println( "testing fpImpureFuture - future part" )
                    -1
                }
            }
        },
        FpImpureFuture[ Int ] {
            FpImpure {
                println( "testing fpImpureFuture - impure part" )
                FpFuture {
                    Thread.sleep( 500 )
                    println( "testing fpImpureFuture - future part" )
                    234233423
                }
            }
        },
        FpImpureFuture[ Int ] {
            FpImpure {
                println( "testing fpImpureFuture - impure part" )
                FpFuture {
                    println( "testing fpImpureFuture - future part" )
                    -235232343
                }
            }
        },
    ),
)
