package org.hungerford.fp.collections

import org.hungerford.fp.basic.FpOption
import org.hungerford.fp.types._

sealed class FpStack[ T, U  ]( fn : FpList[ T ] => (U, FpList[ T ])) extends FpState[ FpList[ T ], U ]( fn )

object FpStack {
    def peek[ T ] : FpStack[ T, FpOption[ T ] ] = new FpStack[ T, FpOption[ T ] ]( ( list : FpList[ T ] ) => (list.headOption, list) )
    def put[ T ]( item : T ) : FpStack[ T, Unit ] = new FpStack[ T, Unit ]( ( list : FpList[ T ] ) => ((), list + item) )
    def pop[ T ] : FpStack[ T, FpOption[ T ] ] = new FpStack[ T, FpOption[ T ] ]( ( list : FpList[ T ] ) => (list.headOption, if ( list == FpNil ) FpNil else list.tail ) )
}


