package org.hungerford.fp.types

import org.hungerford.fp.collections.{FpList, FpNil}

trait MonoidStatic[ T ] {

    def empty : T

    def combine( a : T, b : T ) : T

    def concat[ B ]( eles : FpList[ T ] ) : T = eles match {
        case FpNil => empty
        case FpList( FpNil, head : T ) => head
        case FpList( fpList : FpList[ T ], head : T ) => combine( concat( fpList ), head )
    }
}

trait Monoid[ T ] { this : T =>

    val static : MonoidStatic[ T ]

    def combine( a : T ) : T = static.combine( this, a )

}

trait TypedMonoid[ T[ +_ ], +A ] { this : T[ A ] =>

    val static : TypedMonoidStatic[ T ]

    def combine[ B >: A ]( a : T[ B ] ) : T[ B ] = static.combine( this, a )

}

trait TypedMonoidStatic[ T[ +_ ] ] {

    def empty : T[ _ ]

    def combine[ B ]( a : T[ B ], b : T[ B ] ) : T[ B ]

    def concat[ B ]( eles : FpList[ T[ B ] ] ) : T[ B ] = eles.foldLeft( empty.asInstanceOf[ T[ B ] ] )( combine )
}

