package org.hungerford.fp.utilities

import org.hungerford.fp.basic.{FpNone, FpOption, FpSome}
import org.hungerford.fp.collections.{FpList, FpNil, FpString}
import org.hungerford.fp.types.{Monad, MonadStatic}

trait Logger[ +T ] extends Monad[ Logger, T ] {
    override val static : MonadStatic[ Logger ] = Logger

    private val outerThis = this

    val logs : FpList[ FpString ]
    val value : FpOption[ T ]

    def record( msg : FpString ) : Logger[ T ] = new Logger[ T ] {
        override val logs : FpList[ FpString ] = outerThis.logs :+ msg
        override val value : FpOption[ T ] = outerThis.value
    }

    def record( msgs : FpList[ FpString ] ) : Logger[ T ] = new Logger[ T ] {
        override val logs : FpList[ FpString ] = outerThis.logs :++ msgs
        override val value : FpOption[ T ] = outerThis.value
    }

    override def toString : String = s"Logger( ${value.getOrElse( "Empty" )}, ${logs} )"

}

case class Log( msg : FpString ) extends Logger[ Unit ] {
    val logs : FpList[ FpString ] = FpNil :+ msg
    override val value : FpOption[ Unit ] = FpSome( () )
}

case class Logged[ +T ]( valueIn : T, msg : FpString ) extends Logger[ T ] {
    override val logs : FpList[ FpString ] = FpNil :+ msg
    override val value : FpOption[ T ] = FpSome( valueIn )
}

object Logger extends MonadStatic[ Logger ] {

    def apply[ T ]( valueIn : FpOption[ T ], logsIn : FpList[ FpString ] ) : Logger[ T ] = {
        new Logger[ T ] {
            override val logs : FpList[ FpString ] = logsIn
            override val value : FpOption[ T ] = valueIn
        }
    }


    override def flatMap[ A, B ]( a : Logger[ A ] )
                                ( fn : A => Logger[ B ] ) : Logger[ B ] = {
        a.value match {
            case FpNone => a.asInstanceOf[ Logger[ B ] ]
            case FpSome( x : A ) =>
                val res : Logger[ B ] = fn( x )
                println( res )
                Logger[ B ]( res.value, a.logs :++ res.logs )
        }
    }

    override def unit[ A ]( ele : A ) : Logger[ A ] = new Logger[A] {
        override val logs : FpList[ FpString ] = FpNil :+ FpString( "" )
        override val value : FpOption[ A ] = FpSome( ele )
    }
}
