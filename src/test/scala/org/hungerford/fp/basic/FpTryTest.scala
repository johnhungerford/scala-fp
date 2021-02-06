package org.hungerford.fp.basic

import org.hungerford.fp.types.MonadTest

class FpTryTest extends MonadTest[ FpTry ](
    "FpTry",
    FpTry,
    List(
        FpSuccess( 0 ),
        FpFailure( new Exception() ),
        FpSuccess( 1 ),
        FpFailure( new Exception( "Some message" ) ),
        FpSuccess( -1 ),
        FpFailure( new NullPointerException( "these shouldn't exist in Scala!" ) ),
        FpSuccess( 24352456 ),
        FpFailure( new StackOverflowError( "Now that's more like it..." ) ),
        FpSuccess( -34523423 ),
    ),
) {

}
