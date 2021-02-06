package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadTest}

class FpEitherTest extends MonadTest[ ({ type E[ +A ] = FpEither[ A, String ]})#E ](
    "FpEither",
    FpEitherStatic[ String ],
    List[ FpEither[ Int, String ] ](
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
