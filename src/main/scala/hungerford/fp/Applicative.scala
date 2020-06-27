package hungerford.fp

trait ApplicativeStatic[ T[_] ] {

    def unit[ A ]( ele : A ) : T[ A ]

    def map[ A, B ]( ele : T[ A ] )( fn : A => B ) : T[ B ]

    def get[ A ]( ele : T[ A ] ) : Option[ A ]

}

trait Applicative[ T[ _ ], +A ] extends ApplicativeStatic[ T ] {

    this : T[ A ] =>
        def map[ B ]( fn : A => B ) : T[ B ] = map( this )( fn )

        def get : Option[ A ] = get( this )

}

