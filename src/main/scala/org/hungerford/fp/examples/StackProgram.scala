package org.hungerford.fp.examples

import org.hungerford.fp.basic.{FpOption, FpTry}
import org.hungerford.fp.collections.FpString
import org.hungerford.fp.impure.{FpIO, FpImpure}

object StackProgram {
    def apply() : FpImpure[ FpOption[ Int ] ] = for {
        strValue <- FpIO.fpReadLine
        _ <- FpIO.fpPrintLine( strValue )
        i <- FpImpure( FpTry( strValue.toString.toInt ).toOption )
        _ <- FpIO.fpPrintLine( i.map( v => FpString( s"${v * 100}" ) ).getOrElse( FpString( "Not an integer!" ) ) )
    } yield i
}
