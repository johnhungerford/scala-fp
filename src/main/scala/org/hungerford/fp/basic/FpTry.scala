package org.hungerford.fp.basic

import org.hungerford.fp.collections.FpList
import org.hungerford.fp.types.{Monad, MonadStatic, WithTransformer}

sealed trait FpTry[ +T ] extends Monad[ FpTry, T ] {
    override val static : MonadStatic[ FpTry ] = FpTry

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

object FpTry extends MonadStatic[ FpTry ] with WithTransformer[ FpTry ] {
    def apply[ T ]( block : => T ) : FpTry[ T ] = try {
        FpSuccess( block )
    } catch {
        case t : Throwable => FpFailure( t )
    }

    override def flatMap[ A, B ]( a : FpTry[ A ] )
                                ( fn : A => FpTry[ B ] ) : FpTry[ B ] = a match {
        case FpSuccess( v ) => fn( v )
        case FpFailure( v ) => FpFailure( v )
    }

    override def unit[ A ]( ele : A ) : FpTry[ A ] = FpSuccess( ele )

    private class ThisTransformerStatic[ M[ +_ ] <: Monad[ M, _ ] ](
        override val innerStatic : MonadStatic[ FpTry ],
        override val outerStatic : MonadStatic[ M ],
    ) extends TransformerStatic[ M ] {
        override def flatMap[ A, B ]( a : Transformer[ M, A ] )
                                    ( fn : A => Transformer[ M, B ] ) : Transformer[ M, B ] = {
            T[ M, B ]( a.value.asInstanceOf[ Monad[ M, FpTry[ A ] ] ].flatMap {
                case FpFailure( t ) => outerStatic.unit( FpFailure( t ) )
                case FpSuccess( v ) => fn( v ).value
            } )
        }
    }

    override protected def TS[ M[ +_ ] <: Monad[ M, _ ] ]( outerStatic : MonadStatic[ M ] ) : TransformerStatic[ M ] = {
        new ThisTransformerStatic[ M ]( this, outerStatic )
    }
}
