package org.hungerford.fp.types

import org.hungerford.fp.recursion.{Call, Result, StackSafe}

trait MonadStatic[ T[ _ ] ] extends FunctorStatic[ T ] with ApplicativeStatic[ T ] {

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

trait Monad[ T[ _ ], A ] extends MonadStatic[ T ] with Applicative[ T, A ] { this : T[ A ] =>

    def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = flatMap( this )( fn )

    final def autoChain( num : Int )( fn : T[ A ] => T[ A ] ) : T[ A ] = autoChain[ A ]( this )( num )( fn )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = sub( this, repl )

}

trait MonadCovariant[ T[ _ ], +A ] extends MonadStatic[ T ] with ApplicativeCovariant[ T, A ] { this : T[ _ ] =>

    def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = flatMap( this.asInstanceOf[ T[ A ] ] )( fn )

    final def autoChain[ B >: A ]( num : Int )( fn : T[ B ] => T[ B ] ) : T[ B ] = autoChain[ B ]( this.asInstanceOf[ T[ B ] ] )( num )( fn  )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = sub( this.asInstanceOf[ T[ A ] ], repl )
}

