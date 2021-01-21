package org.hungerford.fp.basic

import org.hungerford.fp.types.MonadCovariant

sealed trait FpEither[ +T, U ] extends MonadCovariant[ ({ type X[ Y ] = FpEither[ Y, U ] })#X, T ] {
    override def flatMap[ A, B ]( a : FpEither[ A, U ] )
                                ( fn : A => FpEither[ B, U ] ) : FpEither[ B, U ] = a match {
        case FpLeft( v ) => fn( v )
        case FpRight( v ) => FpRight( v )
    }

    override def unit[ A ]( ele : A ) : FpEither[ A, U ] = FpLeft( ele )

    def toOption[ B >: T ] : FpOption[ B ] = this match {
        case FpLeft( b ) => FpSome( b )
        case _ => FpNone
    }

    def getOrElse[ B >: T ]( alternative : B ) : B = this.toOption.getOrElse( alternative )

    def reverse[ B >: T ] : FpEither[ U, B ] = this match {
        case FpLeft( v ) => FpRight( v )
        case FpRight( v ) => FpLeft( v )
    }
}

case class FpLeft[ +T, U ]( value : T ) extends FpEither[ T, U ]

case class FpRight[ +T, U ]( value : U ) extends FpEither[ Nothing, U ]

object FpEither {

    sealed trait FpEitherT[ M[ _ ], +T, U ] extends MonadCovariant[ ({ type A[ B ] = FpEitherT[ M, B, U ]})#A, T ] {
        val value : MonadCovariant[ M, FpEither[ T, U ] ]

        override def flatMap[ A, B ]( a : FpEitherT[ M, A, U ] )
                                    ( fn : A => FpEitherT[ M, B, U ] ) : FpEitherT[ M, B, U ] = {
            FpEither.T[ M, B, U ]( a.value.flatMap { l : FpEither[ A, U ] => l match {
                case FpRight( v ) => a.value.unit( FpRight( v ) ).asInstanceOf[ M[ FpEither[ B, U ] ] ]
                case FpLeft( v ) => fn( v ).value.asInstanceOf[ M[ FpEither[ B, U ] ] ]
            } }.asInstanceOf[ MonadCovariant[ M, FpEither[ B, U ] ] ])
        }

        override def unit[ A ]( ele : A ) : FpEitherT[ M, A, U ] = FpEither.T( value.unit( FpLeft( ele ) ).asInstanceOf[ MonadCovariant[ M, FpEither[ A, U ] ] ] )
    }

    def T[ M[ _ ], X, U ]( valueIn : MonadCovariant[ M, FpEither[ X, U ] ] ) : FpEitherT[ M, X, U ] = new FpEitherT[ M, X, U ] {
        override val value : MonadCovariant[ M, FpEither[ X, U ] ] = valueIn
    }
}
