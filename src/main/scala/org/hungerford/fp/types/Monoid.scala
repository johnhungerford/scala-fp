package org.hungerford.fp.types

import org.hungerford.fp.collections.{FpList, FpNil}

trait MonoidStatic[ T ] {

    def empty : T

    def emptyM : Monoid[ T ] = empty.asInstanceOf[ Monoid[ T ] ]

    def combine[ B ]( a : T, b : T ) : T

    def concat[ B ]( eles : FpList[ T ] ) : T = eles match {
        case FpNil => empty.asInstanceOf[ T ]
        case FpList( FpNil, head : T ) => head
        case FpList( fpList : FpList[ T ], head : T ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T ] extends MonoidStatic[ T ] { this : T =>

    def combine( a : T ) : T = combine( this, a )

    def combineM( a : Monoid[ T ] ): Monoid[ T ] = combine( a.native ).asInstanceOf[ Monoid[ T ] ]

    def native : T = this

    def monoid : Monoid[ T ] = this.asInstanceOf[ Monoid[ T ] ]

}

trait MonoidCovariantStatic[ T[ _ ] ] {

    def empty : T[ Nothing ]

    def emptyM : MonoidCovariant[ T, Nothing ] = empty.asInstanceOf[ MonoidCovariant[ T, Nothing ] ]

    def combine[ B ]( a : T[ B ], b : T[ B ] ) : T[ B ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles match {
        case FpNil => empty.asInstanceOf[ T[ B ] ]
        case FpList( FpNil, head : T[ B ] ) => head
        case FpList( fpList : FpList[ T[ B ] ], head : T[ B ] ) => combine( concat( fpList ), head )
    }
}

trait MonoidCovariant[ T[ _ ], +A ] extends MonoidCovariantStatic[ T ] { this : T[ _ ] =>

    def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = combine( this.asInstanceOf[ T[ B ] ], a )

    def combineM[ B >: A ]( a : MonoidCovariant[ T, B ] ): MonoidCovariant[ T, B ] = combine( this.asInstanceOf[ T[ B ] ], a.asInstanceOf[ T[ B ] ] ).asInstanceOf[ MonoidCovariant[ T, B ] ]

    def native[ B >: A ] : T[ B ] = this.asInstanceOf[ T[ B ] ]

    def monoid[ B >: A ] : MonoidCovariant[ T, B ] = this.asInstanceOf[ MonoidCovariant[ T, B ] ]

}

