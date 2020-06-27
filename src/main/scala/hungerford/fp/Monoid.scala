package hungerford.fp

trait MonoidStatic [ T[ _ ] ] {
    def empty : T[ Nothing ]
    def combine[ B  ]( a : T[ B ], b : T[ B ] ) : T[ B ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles match {
        case FpNil => empty.asInstanceOf[ T[ B ] ]
        case FpList( FpNil, head : T[ B ] ) => head
        case FpList( fpList : FpList[ T[ B ] ], head : T[ B ] ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T[ _ ], +A ] extends MonoidStatic[ T ] {

    this : T[ A ] =>
        def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = combine( this.asInstanceOf[ T[ B ] ], a )

}

