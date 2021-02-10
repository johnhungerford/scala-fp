package org.hungerford.fp.basic

import org.hungerford.fp.collections.FpString
import org.hungerford.fp.types.MonadTest

class FpWriterTest extends MonadTest[ ({ type T[+X] = FpWriter[X, FpString] })#T ](
    "FpWriter",
    FpWriter.Static[ FpString ]( FpString ),
    List[ FpWriter[ Int, FpString ] ](
        FpWriter[ Int, FpString ]( 0, FpString( "hello" ) ),
        FpWriter[ Int, FpString ]( 1, FpString( "hi" ) ),
        FpWriter[ Int, FpString ]( -1, FpString( "" ) ),
        FpWriter[ Int, FpString ]( 2, FpString( "" ) ),
        FpWriter[ Int, FpString ]( -2, FpString( "whatever" ) ),
        FpWriter[ Int, FpString ]( 452352452, FpString( "  " ) ),
        FpWriter[ Int, FpString ]( -345345345, FpString( "dfgsdfgfasdfadsfasdfasdfgasdfgasdfgsdfgsdfgsdfgdasfsagdsadgasgafgafgasdfasfa" ) ),
        FpWriter[ Int, FpString ]( 100, FpString( "world" ) ),
    )
)
