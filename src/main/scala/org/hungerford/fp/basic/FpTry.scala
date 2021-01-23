package org.hungerford.fp.basic

import org.hungerford.fp.collections.FpList
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpTry[ +T ] extends MonadCovariant[ FpTry, T ] {
    override def flatMap[ A, B ]( a : FpTry[ A ] )
                                ( fn : A => FpTry[ B ] ) : FpTry[ B ] = FpTry.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpTry[ A ] = FpTry.unit( ele )

    def toOption[ B >: T ] : FpOption[ B ] = this match {
        case FpSuccess( b ) => FpSome( b )
        case _ => FpNone
    }

    def toTry[ B >: T ] : FpEither[ B, Throwable ] = this match {
        case FpSuccess( b ) => FpLeft[ B, Throwable ]( b )
        case FpFailure( t ) => FpRight[ B, Throwable ]( t )
    }

    def getOrElse[ B >: T ]( alternative : B ) : B = this.toOption.getOrElse( alternative )

    def toFpList[ B >: T ] : FpList[ B ] = this.toOption.toFpList

    def recover[ A >: T ]( fn : Throwable => A ) : FpSuccess[ A ] = this match {
        case s@FpSuccess( _ ) => s
        case FpFailure( thr ) => FpSuccess( fn( thr ) )
    }

    def recoverWith[ B >: T ]( fn : Throwable => FpTry[ B ] ) : FpTry[ B ] = this match {
        case s@FpSuccess( _ ) => s
        case FpFailure( thr ) => fn( thr )
    }

    def recoverPartial[ A >: T ]( fn : PartialFunction[ Throwable, A ] ) : FpTry[ A ] = this match {
        case s@FpSuccess( _ ) => s
        case f@FpFailure( thr ) => if ( fn.isDefinedAt( thr ) ) {
            FpSuccess( fn( thr ) )
        } else f
    }

    def recoverWithPartial[ A >: T ]( fn : PartialFunction[ Throwable, FpTry[ A ] ] ) : FpTry[ A ] = this match {
        case s@FpSuccess( _ ) => s
        case f@FpFailure( thr ) => if ( fn.isDefinedAt( thr ) ) fn( thr ) else f
    }

    def transform[ A >: T ]( fn : PartialFunction[ FpTry[ T ], A ] ) : FpTry[ A ] = {
        if ( fn.isDefinedAt( this ) ) FpSuccess( fn( this ) ) else this
    }

    def transformWith[ A >: T ]( fn : PartialFunction[ FpTry[ T ], FpTry[ A ] ] ) : FpTry[ A ] = {
        if ( fn.isDefinedAt( this ) ) fn( this ) else this
    }
}

case class FpSuccess[ +T ]( value : T ) extends FpTry[ T ]

case class FpFailure( throwable : Throwable ) extends FpTry[ Nothing ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case FpFailure( _ ) => true
        case _ => false
    }
}

object FpTry extends MonadStatic[ FpTry ] {
    def apply[ T ]( block : => T ) : FpTry[ T ] = try {
        FpSuccess( block )
    } catch {
        case t : Throwable => FpFailure( t )
    }

    sealed trait FpTryT[ M[ _ ], +T ] extends MonadCovariant[ ({ type A[ B ] = FpTryT[ M, B ]})#A, T ] {
        val value : MonadCovariant[ M, FpTry[ T ] ]

        override def flatMap[ A, B ]( a : FpTryT[ M, A ] )
                                    ( fn : A => FpTryT[ M, B ] ) : FpTryT[ M, B ] = {
            FpTry.T[ M, B ]( a.value.flatMap { l : FpTry[ A ] => l match {
                case FpFailure( t ) => a.value.unit( FpFailure( t ) ).asInstanceOf[ M[ FpTry[ B ] ] ]
                case FpSuccess( v ) => fn( v ).value.asInstanceOf[ M[ FpTry[ B ] ] ]
            } }.asInstanceOf[ MonadCovariant[ M, FpTry[ B ] ] ] )
        }

        override def unit[ A ]( ele : A ) : FpTryT[ M, A ] = FpTry.T( value.unit( FpSome( ele ) ).asInstanceOf[ MonadCovariant[ M, FpTry[ A ] ] ] )
    }

    def T[ M[ _ ], X ]( valueIn : MonadCovariant[ M, FpTry[ X ] ] ) : FpTryT[ M, X ] = new FpTryT[ M, X ] {
        override val value : MonadCovariant[ M, FpTry[ X ] ] = valueIn
    }

    override def flatMap[ A, B ]( a : FpTry[ A ] )
                                ( fn : A => FpTry[ B ] ) : FpTry[ B ] = a match {
        case FpSuccess( v ) => fn( v )
        case FpFailure( v ) => FpFailure( v )
    }

    override def unit[ A ]( ele : A ) : FpTry[ A ] = FpSuccess( ele )
}
