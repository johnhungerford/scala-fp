package org.hungerford.fp.examples

import org.hungerford.fp.basic.{FpOption, FpTry}
import org.hungerford.fp.collections.FpString
import org.hungerford.fp.impure.{FpIO, FpImpure}

object StackProgram {
    import FpIO._

    def apply() : FpImpure[ FpOption[ Int ] ] = for {
        strValue <- fpReadLine
        _ <- fpPrintLine( strValue )
        i <- FpImpure( FpTry( strValue.toString.toInt ).toOption )
        _ <- fpPrintLine( i.map( v => FpString( s"${v * 100}" ) ).getOrElse( FpString( "Not an integer!" ) ) )
    } yield i

    def verbose() : FpImpure[ FpOption[ Int ] ] =
        fpReadLine.flatMap( strValue =>
            fpPrintLine( strValue ).flatMap( _ =>
                FpImpure( FpTry( strValue.toString.toInt ).toOption ).flatMap( i =>
                    fpPrintLine( i.map( v => FpString( s"${v * 100}" ) ).getOrElse( FpString( "Not an integer!" ) ) ).map( _ =>
                        i ) ) ) )
}
