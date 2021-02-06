package org.hungerford.fp.basic

import org.hungerford.fp
import org.hungerford.fp.collections.{FpList, FpNil}
import org.hungerford.fp.types.{Monad, MonadStatic, WithTransformer}

sealed trait FpOption[ +T ] extends Monad[ FpOption, T ] {
    override val static : MonadStatic[ FpOption ] = FpOption

    def isEmpty : Boolean = this match {
        case FpNone => true
        case _ => false
    }

    def isDefined : Boolean = !isEmpty

    def getOrElse[ B >: T ]( alternative : B ) : B = this match {
        case FpNone => alternative
        case FpSome( actual ) => actual
    }

    def toFpList[ B >: T ] : FpList[ B ] = this match {
        case FpNone => FpNil
        case FpSome( head ) => FpNil :+ head
    }
}

case object FpNone extends FpOption[ Nothing ]

case class FpSome[ T ]( ele : T ) extends FpOption[ T ]

object FpOption extends MonadStatic[ FpOption ] with WithTransformer[ FpOption ] {
    override def flatMap[ A, B ]( a : FpOption[ A ] )
                                ( fn : A => FpOption[ B ] ) : FpOption[ B ] = a match {
        case FpNone => FpNone
        case FpSome( value ) => fn( value )
    }

    override def unit[ A ]( ele : A ) : FpOption[ A ] = FpSome( ele )

    private class ThisTransformerStatic[ M[ +_ ] <: Monad[ M, _ ] ](
        override val innerStatic : MonadStatic[ FpOption ],
        override val outerStatic : MonadStatic[ M ],
    ) extends TransformerStatic[ M ] {

        override def flatMap[ A, B ]( a : fp.basic.FpOption.Transformer[ M, A ] )
                                    ( fn : A => fp.basic.FpOption.Transformer[ M, B ] ) : fp.basic.FpOption.Transformer[ M, B ] = {
            T[ M, B ]( a.value.asInstanceOf[ Monad[ M, FpOption[ A ] ] ].flatMap {
                case FpNone => outerStatic.unit( FpNone )
                case FpSome( v ) => fn( v ).value
            } )
        }

    }

    override protected def TS[ M[ +_ ] <: Monad[ M, _ ] ]( outerStatic : MonadStatic[ M ] ) : TransformerStatic[ M ] = {
        new ThisTransformerStatic[ M ]( this, outerStatic )
    }
}
