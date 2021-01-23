package org.hungerford.fp.basic

import org.hungerford.fp.collections.{FpList, FpNil, FpString}
import org.hungerford.fp.types.MonadCovariantTest

class FpWriterCovariantTest extends MonadCovariantTest(
    "FpWriterCovariant",
    FpWriterCovariantStatic[ FpString ]( FpString ),
    List[ FpWriterCovariant[ Int, FpString ] ](
        FpWriterCovariant[ Int, FpString ]( 0, "hello" ),
        FpWriterCovariant[ Int, FpString ]( 1, "hi" ),
        FpWriterCovariant[ Int, FpString ]( -1, "" ),
        FpWriterCovariant[ Int, FpString ]( 2, "" ),
        FpWriterCovariant[ Int, FpString ]( -2,  "whatever" ),
        FpWriterCovariant[ Int, FpString ]( 452352452, "  " ),
        FpWriterCovariant[ Int, FpString ]( -345345345, "dfgsdfgfasdfadsfasdfasdfgasdfgasdfgsdfgsdfgsdfgdasfsagdsadgasgafgafgasdfasfa" ),
        FpWriterCovariant[ Int, FpString ]( 100, "world" ),
    )
    )