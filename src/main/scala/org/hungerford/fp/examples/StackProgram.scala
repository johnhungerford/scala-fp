package org.hungerford.fp.examples

import hungerford.fp.impure.FpIO
import org.hungerford.fp.collections.FpString
import org.hungerford.fp.impure.{FpIO, FpImpure}

import scala.util.{Success, Try}

object StackProgram {
    def apply() : Unit = (for {
        strValue <- FpIO.fpReadLine
        i <- FpImpure( Try( strValue.toString.toInt ).toOption )
        _ <- FpIO.fpPrintLine( FpString( s"${i.getOrElse( 1 ) * 100}" ) )
    } yield ()).loop.run()
}
