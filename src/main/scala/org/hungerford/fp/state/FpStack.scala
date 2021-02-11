package org.hungerford.fp.state

import org.hungerford.fp.basic.{FpOption, FpState}
import org.hungerford.fp.collections.{FpList, FpNil}

sealed class FpStack[ T, U  ]( fn : FpList[ T ] => (U, FpList[ T ])) extends FpState[ FpList[ T ], U ]( fn )

object FpStack {
    def peek[ T ] : FpStack[ T, FpOption[ T ] ] = new FpStack[ T, FpOption[ T ] ]( ( list : FpList[ T ] ) => (list.headOption, list) )
    def put[ T ]( item : T ) : FpStack[ T, Unit ] = new FpStack[ T, Unit ]( ( list : FpList[ T ] ) => ((), list :+ item) )
    def pop[ T ] : FpStack[ T, FpOption[ T ] ] = new FpStack[ T, FpOption[ T ] ]( ( list : FpList[ T ] ) => (list.headOption, list match {
        case FpNil => FpNil
        case FpList( tail, _ ) => tail
    } ) )
}


