package hungerford.fp

trait MonadStatic[ T[ _ ] ] extends ApplicativeStatic[ T ] {

    def flatMap[ A, B ]( a : T[ A ] )( fn : A => T[ B ] ) : T[ B ]

    override def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ] = flatMap( ele )( a => unit( fn( a ) ) )

    def sub[ X, Y ]( a : T[ X ], b : T[ Y ] ) : T[ Y ] = flatMap( a )( ( _ : X ) => b )

}

trait Monad[ T[ _ ], +A ] extends MonadStatic[ T ] with Applicative[ T, A ] { this : T[ _ ] =>

    def flatMap[ B ]( fn : A => T[ B ] ) : T[ B ] = flatMap( this.asInstanceOf[ T[ A ] ] )( fn )
}

