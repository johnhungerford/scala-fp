package org.hungerford.fp.types

trait ApplicativeStatic[ T[ +_ ] ] extends FunctorStatic[ T ] {

    def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ]

    override def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ] = appl( unit( fn ) )( ele )

}

trait Applicative[ T[ +_ ], +A ] extends Functor[ T, A ] { this : T[ A ] =>
    override val static : ApplicativeStatic[ T ]
}

trait ApplicativeStaticInvariant[ T[ _ ] ] extends FunctorStaticInvariant[ T ] {

    def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ]

    override def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ] = appl( unit( fn ) )( ele )

}

trait ApplicativeInvariant[ T[ _ ], A ] extends FunctorInvariant[ T, A ] { this : T[ A ] =>
    override val static : ApplicativeStaticInvariant[ T ]
}
