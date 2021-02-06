package org.hungerford.fp.types

trait FunctorStatic[ T[ +_ ] ] {

    def unit[ A ]( ele : A ) : T[ A ]

    def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ]

}

trait Functor[ T[ +_ ], +A ] { this : T[ A ] =>
    val static : FunctorStatic[ T ]

    def map[ B ]( fn : A => B ) : T[ B ] = static.map( this )( fn )

}

trait FunctorStaticInvariant[ T[ _ ] ] {

    def unit[ A ]( ele : A ) : T[ A ]

    def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ]

}

trait FunctorInvariant[ T[ _ ], A ] { this : T[ A ]  =>
    val static : FunctorStaticInvariant[ T ]

    def map[ B ]( fn : A => B ) : T[ B ] = static.map( this.asInstanceOf[ T[ A ] ] )( fn )
}
