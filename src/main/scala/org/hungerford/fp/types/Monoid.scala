package org.hungerford.fp.types

import org.hungerford.fp.collections.{FpList, FpNil}

trait MonoidStatic[ T ] {

    def empty : T

    def emptyM : Monoid[ T ] = empty.asInstanceOf[ Monoid[ T ] ]

    def combine[ B ]( a : T, b : T ) : T

    def concat[ B ]( eles : FpList[ T ] ) : T = eles match {
        case FpNil => empty
        case FpList( FpNil, head : T ) => head
        case FpList( fpList : FpList[ T ], head : T ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T ] { this : T =>

    val static : MonoidStatic[ T ]

    def combine( a : T ) : T = static.combine( this, a )

    def combineM( a : Monoid[ T ] ): Monoid[ T ] = static.combine( this, a.native ).asInstanceOf[ Monoid[ T ] ]

    def native : T = this

    def monoid : Monoid[ T ] = this.asInstanceOf[ Monoid[ T ] ]

}

trait MonoidCovariantStatic[ T[ +_ ] ] {

    def empty : T[ _ ]

    def combine[ B ]( a : T[ B ], b : T[ B ] ) : T[ B ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles match {
        case FpNil => empty.asInstanceOf[ T[ B ] ]
        case FpList( FpNil, head : T[ B ] ) => head
        case FpList( fpList : FpList[ T[ B ] ], head : T[ B ] ) => combine( concat( fpList ), head )
    }
}

trait MonoidCovariant[ T[ +_ ], +A ] { this : T[ A ] =>

    val static : MonoidCovariantStatic[ T ]

    def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = static.combine( this.asInstanceOf[ T[ B ] ], a )

}

