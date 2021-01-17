package org.hungerford.fp.collections

import hungerford.fp.types.FpStateFul
import org.hungerford.fp.types.{FpAction, FpState, FpStateFul, FpView}

trait FpStack[ T ] extends FpState[ FpStack[ T ] ] {
    def internalState : FpList[ T ]
    override def value : FpStack[ T ] = this

    def push( ele : T ) : FpStack[ T ] = act( FpStack.pushAction( ele ) ).value
    def pop : (Option[ T ], FpStack[ T ]) = ( see( FpStack.peekView[ T ] ), act( FpStack.popAction[ T ] ).value )
    def peek : Option[ T ] = see( FpStack.peekView[ T ] )

    override def toString : String = s"FpStack( ${internalState} )"
}

object FpStack {

    def apply[ A ]() : FpStack[ A ] = new FpStack[ A ] {
        override def internalState : FpList[ A ] = FpNil
    }

    def apply[ A ]( list : FpList[ A ] ) : FpStack[ A ] = new FpStack[ A ] {
        override def internalState : FpList[ A ] = list
    }

    def popAction[ T ] : FpAction[ FpStack[ T ] ] = FpAction( (stack : FpStack[ T ]) => stack.internalState match {
        case FpNil => FpStack( FpNil )
        case FpList( next : FpList[ T ], _ ) => FpStack( next )
    } )

    def peekView[ T ] = new FpView[ FpStack[ T ], Option[ T ] ]( (stack : FpStack[ T ]) => stack.internalState match {
        case FpNil => None
        case FpList( _, head : T ) => Some( head )
    } )

    def pushAction[ T ]( ele : T ) : FpAction[ FpStack[ T ] ] = FpAction( (stack : FpStack[ T ]) => FpStack( stack.internalState + ele ) )

    def pop[ A ] : FpStateFul[ A, FpState[ FpStack[ A ] ], FpStack[ A ] ] = FpStateFul.fromActViewPre( popAction, peekView )

    def push[ A ]( ele : A ) : FpStateFul[ A, FpState[ FpStack[ A ] ], FpStack[ A ] ] =
        FpStateFul.fromActViewPost( pushAction( ele ), peekView )

    def peek[ A, S <: FpState[ FpStack[ A ] ] ] : FpStateFul[ A, S, FpStack[ A ] ] = FpStateFul.fromView[A, S, FpStack[ A ] ]( peekView[ A ] )

}
