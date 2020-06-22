package hungerford.fp

trait Monoid [ +T ] {
    def empty : T
    def combine[ B >: T ]( ele : B ) : B
    def concat[ B >: T ]( eles : FpList[ B ] ) : B = eles match {
        case FpNil => empty
        case FpList( FpNil, head : B ) => head
        case FpList( fpList : FpList[ B ], head : B ) => concat( fpList ).asInstanceOf[ Monoid[ B ] ].combine( head )
    }
}

