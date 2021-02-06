package org.hungerford.fp.types

import org.hungerford.fp.recursion.{Call, Result, StackSafe}

trait MonadStatic[ T[ +_ ] ] extends ApplicativeStatic[ T ] {

    def flatMap[ A, B ]( a : T[ A ] )( fn : A => T[ B ] ) : T[ B ]

    override def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ] = {
        ( ta : T[ A ] ) => flatMap( fn )( f => flatMap( ta )( a => unit( f( a ) ) ) )
    }

    final def autoChain[ A ]( ele : T[ A ] )( num : Int )( fn : T[ A ] => T[ A ] ) : T[ A ] = {
        StackSafe.selfCall[ Int, T[ A ] ] {
            ( thisFn : Int => StackSafe[ T[ A ] ] ) =>
                ( i : Int ) =>
                    if ( i <= 0 ) Result( ele.asInstanceOf[ T[ A ] ] )
                    else if ( i == 1 ) Result( fn( ele.asInstanceOf[ T[ A ] ] ) )
                    else Call.from {
                        thisFn( i - 1 ).map( v => fn( v ) )
                    }
        }( num )
    }

    final def sub[ X, Y ]( a : T[ X ], b : T[ Y ] ) : T[ Y ] = flatMap( a )( ( _ : X ) => b )
}

trait WithTransformer[ T[ +_ ] <: Monad[ T, _ ] ] { this : MonadStatic[ T ] =>

    protected class Transformer[ M[ +_ ] <: Monad[ M, _ ], +X ]( val value : M[ T[ X ] ] )
      extends Monad[ ({ type A[ +B ] = Transformer[ M, B ]})#A, X ] {
        override val static : MonadStatic[ ( { type A[ +B ] = Transformer[ M, B ] } )#A ] = TS[ M ]( value.static )
    }

    protected trait TransformerStatic[ M[ +_ ] <: Monad[ M, _ ] ] extends MonadStatic[ ({ type A[ +B ] = Transformer[ M, B ] })#A ] {
        val outerStatic : MonadStatic[ M ]
        val innerStatic : MonadStatic[ T ]

        override def unit[ A ]( ele : A ) : Transformer[ M, A ] = T( outerStatic.unit( innerStatic.unit( ele ) ) )
    }

    protected def TS[ M[ +_ ] <: Monad[ M, _ ] ]( outerStatic : MonadStatic[ M ] ) : TransformerStatic[ M ]

    final def T[ M[ +_ ] <: Monad[ M, _ ], X ]( value : M[ T[ X ] ] ) : Transformer[ M, X ] = new Transformer( value )
}

trait Monad[ T[ +_ ], +A ] extends Applicative[ T, A ] { this : T[ A ] =>
    override val static : MonadStatic[ T ]

    final def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = static.flatMap( this.asInstanceOf[ T[ A ] ] )( fn )

    final def autoChain[ B >: A ]( num : Int )( fn : T[ B ] => T[ B ] ) : T[ B ] = static.autoChain[ B ]( this.asInstanceOf[ T[ B ] ] )( num )( fn  )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = static.sub( this.asInstanceOf[ T[ A ] ], repl )
}

trait MonadStaticInvariant[ T[ _ ] ] extends ApplicativeStaticInvariant[ T ] {

    def flatMap[ A, B ]( a : T[ A ] )( fn : A => T[ B ] ) : T[ B ]

    override def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ] = {
        ( ta : T[ A ] ) => flatMap( fn )( f => flatMap( ta )( a => unit( f( a ) ) ) )
    }

    final def autoChain[ A ]( ele : T[ A ] )( num : Int )( fn : T[ A ] => T[ A ] ) : T[ A ] = {
        StackSafe.selfCall[ Int, T[ A ] ] {
            ( thisFn : Int => StackSafe[ T[ A ] ] ) =>
                ( i : Int ) =>
                    if ( i <= 0 ) Result( ele.asInstanceOf[ T[ A ] ] )
                    else if ( i == 1 ) Result( fn( ele.asInstanceOf[ T[ A ] ] ) )
                    else Call.from {
                        thisFn( i - 1 ).map( (v : T[ A ]) => fn( v ) )
                    }
        }( num )
    }

    final def sub[ X, Y ]( a : T[ X ], b : T[ Y ] ) : T[ Y ] = flatMap( a )( ( _ : X ) => b )

}

trait WithTransformerInvariant[ T[ _ ] <: MonadInvariant[ T, _ ] ] { this : MonadStaticInvariant[ T ] =>

    protected class Transformer[ M[ _ ] <: MonadInvariant[ M, _ ], X ]( val value : M[ T[ X ] ] )
      extends MonadInvariant[ ({ type A[ B ] = Transformer[ M, B ]})#A, X ] {
        override val static : TransformerStatic[ M ] = TS[ M ]( value.static )
    }

    protected trait TransformerStatic[ M[ _ ] <: MonadInvariant[ M, _ ] ] extends MonadStaticInvariant[ ({ type A[ B ] = Transformer[ M, B ] })#A ] {
        val outerStatic : MonadStaticInvariant[ M ]
        val innerStatic : MonadStaticInvariant[ T ]

        override def unit[ A ]( ele : A ) : Transformer[ M, A ] = T( outerStatic.unit( innerStatic.unit( ele ) ) )
    }

    protected def TS[ M[ _ ] <: MonadInvariant[ M, _ ] ]( outerStatic : MonadStaticInvariant[ M ] ) : TransformerStatic[ M ]

    final def T[ M[ _ ] <: MonadInvariant[ M, _ ], X ]( value : M[ T[ X ] ] ) : Transformer[ M, X ] = new Transformer( value )
}

trait MonadInvariant[ T[ _ ], A ] extends ApplicativeInvariant[ T, A ] { this : T[ A ] =>
    override val static : MonadStaticInvariant[ T ]

    final def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = static.flatMap( this )( fn )

    final def autoChain( num : Int )( fn : T[ A ] => T[ A ] ) : T[ A ] = static.autoChain[ A ]( this )( num )( fn )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = static.sub( this, repl )

}
