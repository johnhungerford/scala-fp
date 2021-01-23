package org.hungerford.fp.types

trait ApplicativeStatic[ T[_] ] extends FunctorStatic[ T ] {

    def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ]

    override def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ] = appl( unit( fn ) )( ele )

}

trait Applicative[ T[ _ ], A ] extends ApplicativeStatic[ T ] with Functor[ T, A ] { this : T[ A ] =>
}

trait ApplicativeCovariant[ T[ _ ], +A ] extends ApplicativeStatic[ T ] with FunctorCovariant[ T, A ] { this : T[ _ ] =>

}
