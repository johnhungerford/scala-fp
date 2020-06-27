package hungerford.fp

trait MonoidStatic [ T[ _ ] ] {
    def empty : T[ Nothing ]
    def emptyM : Monoid[ T, Nothing ] = empty.asInstanceOf[ Monoid[ T, Nothing ] ]
    def combine[ B  ]( a : T[ B ], b : T[ B ] ) : T[ B ]
    def combine[ B ]( a : Monoid[ T, B ], b : Monoid[ T, B ] ) : Monoid[ T, B ] = combine( a.native, b.native ).asInstanceOf[ Monoid[ T, B ] ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles match {
        case FpNil => empty.asInstanceOf[ T[ B ] ]
        case FpList( FpNil, head : T[ B ] ) => head
        case FpList( fpList : FpList[ T[ B ] ], head : T[ B ] ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T[ _ ], +A ] extends MonoidStatic[ T ] {

    this : T[ A ] =>
        def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = combine( this.asInstanceOf[ T[ B ] ], a )
        def combine[ B >: A ]( a : Monoid[ T, B ] ) : T[ B ] = combine( a.native )
        def combineM[ B >: A ]( a : Monoid[ T, B ] ) : Monoid[ T,  B ] = combine( this.monoid, a )
        def native[ B >: A ] : T[ B ] = this.asInstanceOf[ T[ B ] ]
        def monoid[ B >: A ] : Monoid[ T, B ] = this.asInstanceOf[ Monoid[ T, B ] ]

}

