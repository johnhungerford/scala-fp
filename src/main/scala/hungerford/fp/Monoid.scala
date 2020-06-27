package hungerford.fp

trait MonoidStatic[ T[ _ ] ] {

    def empty : T[ Nothing ]

    def emptyM : Monoid[ T, Nothing ] = empty.asInstanceOf[ Monoid[ T, Nothing ] ]

    def combine[ B ]( a : T[ B ], b : T[ B ] ) : T[ B ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles match {
        case FpNil => empty.asInstanceOf[ T[ B ] ]
        case FpList( FpNil, head : T[ B ] ) => head
        case FpList( fpList : FpList[ T[ B ] ], head : T[ B ] ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T[ _ ], +A ] extends MonoidStatic[ T ] { this : T[ _ ] =>

    def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = combine( this.asInstanceOf[ T[ B ] ], a )

    def combineM[ B >: A ]( a : Monoid[ T, B ] ): Monoid[ T, B ] = combine( this.asInstanceOf[ T[ B ] ], a.asInstanceOf[ T[ B ] ] ).asInstanceOf[ Monoid[ T, B ] ]

    def native[ B >: A ] : T[ B ] = this.asInstanceOf[ T[ B ] ]

    def monoid[ B >: A ] : Monoid[ T, B ] = this.asInstanceOf[ Monoid[ T, B ] ]

}

