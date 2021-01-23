package org.hungerford.fp.impure

import org.hungerford.fp.types.MonadCovariantTest

class FpImpureTest extends MonadCovariantTest(
    "FpImpure",
    FpImpure,
    List(
        FpImpure( 1 ),
        FpImpure { println( "test" ); -1 },
        FpImpure { 234524354 },
        FpImpure { throw new IllegalStateException( "this is why we wrap everything in an FpTry" ) },
        FpImpure { -32423432 },
        FpImpure { println( "another test " ); 0  }
    )
) {

}
