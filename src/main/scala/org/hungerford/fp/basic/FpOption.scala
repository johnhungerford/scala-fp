package org.hungerford.fp.basic

import org.hungerford.fp.collections.{FpList, FpNil}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpOption[ +T ] extends MonadCovariant[ FpOption, T ] {
    override def flatMap[ A, B ]( a : FpOption[ A ] )
                                ( fn : A => FpOption[ B ] ) : FpOption[ B ] = FpOption.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpOption[ A ] = FpOption.unit( ele )

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
        case FpSome( head ) => FpNil + head
    }
}

case object FpNone extends FpOption[ Nothing ]

case class FpSome[ T ]( ele : T ) extends FpOption[ T ]

object FpOption extends MonadStatic[ FpOption ] {

    sealed trait FpOptionT[ M[ _ ], +T ] extends MonadCovariant[ ({ type A[ B ] = FpOptionT[ M, B ]})#A, T ] {
        val value : MonadCovariant[ M, FpOption[ T ] ]

        override def flatMap[ A, B ]( a : FpOptionT[ M, A ] )
                                    ( fn : A => FpOptionT[ M, B ] ) : FpOptionT[ M, B ] = {
            FpOption.T[ M, B ]( a.value.flatMap { l : FpOption[ A ] => l match {
                case FpNone => a.value.unit( FpNone ).asInstanceOf[ M[ FpOption[ B ] ] ]
                case FpSome( v ) => fn( v ).value.asInstanceOf[ M[ FpOption[ B ] ] ]
            } }.asInstanceOf[ MonadCovariant[ M, FpOption[ B ] ] ])
        }

        override def unit[ A ]( ele : A ) : FpOptionT[ M, A ] = FpOption.T( value.unit( FpSome( ele ) ).asInstanceOf[ MonadCovariant[ M, FpOption[ A ] ] ] )
    }

    def T[ M[ _ ], X ]( valueIn : MonadCovariant[ M, FpOption[ X ] ] ) : FpOptionT[ M, X ] = new FpOptionT[ M, X ] {
        override val value : MonadCovariant[ M, FpOption[ X ] ] = valueIn
    }

    override def flatMap[ A, B ]( a : FpOption[ A ] )
                                ( fn : A => FpOption[ B ] ) : FpOption[ B ] = a match {
        case FpNone => FpNone
        case FpSome( value ) => fn( value )
    }

    override def unit[ A ]( ele : A ) : FpOption[ A ] = FpSome( ele )
}
