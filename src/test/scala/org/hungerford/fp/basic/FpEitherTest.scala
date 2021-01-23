package org.hungerford.fp.basic

import org.hungerford.fp.types.MonadCovariantTest

class FpEitherTest extends MonadCovariantTest(
    "FpEither",
    FpEitherStatic[ String ],
    List(
        FpLeft[ Int, String ]( 1 ),
        FpRight[ Int, String ]( "test string" ),
        FpLeft[ Int, String ]( 0 ),
        FpLeft[ Int, String ]( -1 ),
        FpRight[ Int, String ]( "" ),
        FpLeft[ Int, String ]( 100234123 ),
        FpLeft[ Int, String ]( -324424 ),
        FpRight[ Int, String ]( "kasjdflaskdjflasdkhgflaskdhjfkajdfghlajsdkfgjkdfjghalsdkfjskadjghalsdkgjfsldfjhgalsdkjgfalsdhfgjalskjgldakjhgasldhglakdfghlaksdhglakdjfglaksdhjflajrgisebdnslkvjdnakjgrbisobgdoigreognif" )
    )
)
