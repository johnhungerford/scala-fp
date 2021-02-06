package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadStatic, WithTransformer}

sealed trait FpEither[ +T, U ] extends Monad[ ({ type X[ +Y ] = FpEither[ Y, U ] })#X, T ] {
    override val static : MonadStatic[ ( {
        type X[ +Y ] = FpEither[ Y, U ]
    } )#X ] = FpEitherStatic[ U ]()

    def toOption[ B >: T ] : FpOption[ B ] = this match {
        case FpLeft( b ) => FpSome( b )
        case _ => FpNone
    }

    def getOrElse[ B >: T ]( alternative : B ) : B = this.toOption.getOrElse( alternative )

    def reverse[ B >: T ] : FpEither[ U, B ] = this match {
        case FpLeft( v ) => FpRight[ U, B ]( v )
        case FpRight( v ) => FpLeft[ U, B ]( v )
    }
}

case class FpLeft[ +T, U ]( value : T ) extends FpEither[ T, U ]

case class FpRight[ +T, U ]( value : U ) extends FpEither[ Nothing, U ]

case class FpEitherStatic[ U ]()
  extends MonadStatic[ ({ type E[ +A ] = FpEither[ A, U ]})#E ]
    with WithTransformer[ ({ type E[ +A ] = FpEither[ A, U ]})#E ] {

    override def flatMap[ A, B ]( a : FpEither[ A, U ] )
                                ( fn : A => FpEither[ B, U ] ) : FpEither[ B, U ] = a match {
        case FpLeft( v ) => fn( v )
        case FpRight( v ) => FpRight( v )
    }

    override def unit[ A ]( ele : A ) : FpEither[ A, U ] = FpLeft[ A, U ]( ele )

    private val outerThis = this

    private class ThisTransformerStatic[ M[ +_ ] <: Monad[ M, _ ] ]( staticIn : MonadStatic[ M ] ) extends super.TransformerStatic[ M ] {
        override val outerStatic : MonadStatic[ M ] = outerStatic
        override val innerStatic : MonadStatic[ ( { type E[ +A ] = FpEither[ A, U ] } )#E ] = outerThis

        override def flatMap[ A, B ]( a : Transformer[ M, A ] )
                                    ( fn : A => Transformer[ M, B ] ) : Transformer[ M, B ] = {

            T[ M, B ]( a.value.asInstanceOf[ Monad[ M, FpEither[ A, U ] ] ].flatMap {
                case FpRight( v ) => outerStatic.unit( FpRight( v ) )
                case FpLeft( v ) => fn( v ).value
            } )
        }
    }

    override protected def TS[ M[ +_ ] <: Monad[ M, _ ] ]( outerStatic : MonadStatic[ M ] ) : TransformerStatic[ M ] = {
        new ThisTransformerStatic[ M ]( outerStatic )
    }
}
