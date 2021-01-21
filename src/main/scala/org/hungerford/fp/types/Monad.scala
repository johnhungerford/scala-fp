package org.hungerford.fp.types

import scala.annotation.tailrec

trait MonadStatic[ T[ _ ] ] extends FunctorStatic[ T ] with ApplicativeStatic[ T ] {

    def flatMap[ A, B ]( a : T[ A ] )( fn : A => T[ B ] ) : T[ B ]

    override def appl[ A, B ]( fn : A => B ) : T[ A ] => T[ B ] = {
        (ele : T[ A ]) => flatMap( ele )( a => unit( fn( a ) ) )
    }

    final def sub[ X, Y ]( a : T[ X ], b : T[ Y ] ) : T[ Y ] = flatMap( a )( ( _ : X ) => b )

}

trait Monad[ T[ _ ], A ] extends MonadStatic[ T ] with Applicative[ T, A ] { this : T[ A ] =>

    def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = flatMap( this )( fn )

    final def autoChain( num : Int )( fn : T[ A ] => T[ A ] ) : T[ A ] =
        if ( num <= 0 ) this.asInstanceOf[ T[ A ] ]
        else if ( num == 1 ) fn( this.asInstanceOf[ T[ A ] ] )
        else fn( this.asInstanceOf[ T[ A ] ] ).asInstanceOf[ Monad[ T, A ] ].autoChain( num - 1 )( fn )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = sub( this, repl )

}

trait MonadCovariant[ T[ _ ], +A ] extends MonadStatic[ T ] with ApplicativeCovariant[ T, A ] { this : T[ _ ] =>

    def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = flatMap( this.asInstanceOf[ T[ A ] ] )( fn )

    final def autoChain[ B ]( num : Int )( fn : T[ B ] => T[ B ] ) : T[ B ] =
        if ( num <= 0 ) this.asInstanceOf[ T[ B ] ]
        else if ( num == 1 ) fn( this.asInstanceOf[ T[ B ] ] )
        else fn( this.asInstanceOf[ T[ B ] ] ).asInstanceOf[ MonadCovariant[ T, B ] ].autoChain( num - 1 )( fn )

    final def sub[ Y ]( repl : T[ Y ] ) : T[ Y ] = sub( this.asInstanceOf[ T[ A ] ], repl )
}

