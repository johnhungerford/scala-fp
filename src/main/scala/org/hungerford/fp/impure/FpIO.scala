package org.hungerford.fp.impure

import org.hungerford.fp.collections.FpString

object FpIO {

    def fpReadLine : FpImpure[ FpString ] = FpImpure( FpString( scala.io.StdIn.readLine() ) )

    def fpPrintLine( line : FpString ) : FpImpure[ Unit ] = FpImpure( println( line.toString ) )

}
