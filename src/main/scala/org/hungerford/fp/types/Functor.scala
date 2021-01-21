package org.hungerford.fp.types

trait FunctorStatic[ T[ _ ] ] {

    def unit[ A ]( ele : A ) : T[ A ]

    def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ]

}

trait Functor[ T[ _ ], A ] extends FunctorStatic[ T ] { this : T[ A ] =>

    def map[ B ]( fn : A => B ) : T[ B ] = map( this )( fn )

}

trait FunctorCovariant[ T[ _ ], +A ] extends FunctorStatic[ T ] { this : T[ _ ]  =>

    def map[ B ]( fn : A => B ) : T[ B ] = map( this.asInstanceOf[ T[ A ] ] )( fn )

}
