package hungerford.fp

import scala.annotation.tailrec

trait FpAction[ S ] {
    def transform : S => S

    def transformState : FpState[ S ] => FpState[ S ] =
        ( state : FpState[ S ] ) => state.map( transform )

    def >> ( that : FpAction[ S ] ) : FpAction[ S ] = FpAction( ( s : S ) => {
        that.transform( transform( s ) )
    } )

    def times( times : Int ) : FpAction[ S ] = FpAction( times )( this )
}

object FpAction {
    def apply[ S ]( fn : S => S ) : FpAction[ S ] = new FpAction[S] {
        override def transform : S => S = fn
    }

    def apply[ S ]( times : Int )( fpAction : FpAction[ S ] ) : FpAction[ S ] =
        if ( times < 1 ) fpAction
        else fpAction >> apply( times - 1 )( fpAction)

    def identity[ S ] : FpAction[ S ] = FpAction( ( s : S ) => s )

    def apply[ S ]( actions : FpList[ FpAction[ S ] ] ) : FpAction[ S ] = actions match {
        case FpNil => identity
        case FpList( FpNil, action ) => action
        case FpList( next, action ) => apply( next ) >> action
    }
}

class FpView[ S, T ]( fn : S => T ) {
    def surface : S => T = fn
}

object FpView {
    def identity[ S ] : FpView[ S, S ] = new FpView( ( s : S ) => s )
}


trait FpState[ S ] extends Monad[ FpState, S ] {

    def value : S

    def exec[ A ]( stateful : FpStateFul[ A, FpState[ S ], S] ) : (Option[ A ], FpState[ S ]) = stateful.exec( this )

    override def get[ A ]( ele : FpState[ A ] ) : Option[ A ] = Some( ele.value )

    def act( action : FpAction[ S ] ) : FpState[ S ] = map( action.transform )

    def see[ T ]( view : FpView[ S, T ] ) : T = view.surface( value )

    @tailrec
    final def act( times : Int )( action : FpAction[ S ] ) : FpState[ S ] =
        if ( times < 1 ) this
        else act( action ).act( times - 1 )( action )

    def doActions[ T ]( state : FpState[ T ] )( actions : FpList[ FpAction[ T ] ] ) : FpState[ T ]  = actions match {
        case FpNil => state
        case FpList( FpNil, a : FpAction[ T ] ) => state.act( a )
        case FpList( nextActions : FpList[ FpAction[ T ] ], a : FpAction[ T ] ) =>
            doActions( state.act( a ) )( nextActions )
    }

    def act( actions : FpList[ FpAction[ S ] ] ) : FpState[ S ] = doActions( this )( actions.reverse )

    override def flatMap[ A, B ]( a : FpState[ A ] )( fn : A => FpState[ B ] ) : FpState[ B ] = fn( a.value )

    override def unit[ A ]( ele : A ) : FpState[ A ] = FpState( ele )

    override def toString : String = s"FpState(${value})"

}

object FpState {
    def apply[ A ]( s : A ) : FpState[ A ] = new FpState[ A ] {
        override def value : A = s
    }
}

trait FpStateFul[ A, S <: FpState[ T ], T ] extends Monad[ ({ type U[ X ] = FpStateFul[ X, S, T ] })#U, A ] {
    def exec : S => (Option[ A ], S)

    def >>[ B ]( that : FpStateFul[ B, S, T ] ) : FpStateFul[ B, S, T ] =
        FpStateFul( ( s : S ) => {
            that.exec( exec( s )._2 )
        } )

    def times( num : Int ) : FpStateFul[ A, S, T ] =
        if ( num < 1 ) FpStateFul.identity
        else this >> ( times( num - 1 ) )

    override def flatMap[ A, B ]( a : FpStateFul[ A, S, T ] )( fn : A => FpStateFul[ B, S, T ] ) : FpStateFul[ B, S, T ] =
        FpStateFul( ( s : S ) => {
            val (optValue : Option[ A ], newS : S ) = exec( s )
            optValue match {
                case None => (None, newS)
                case Some( value : A ) => fn( value ).exec( newS )
            }
        } )

    override def unit[ B ]( ele : B ) : FpStateFul[ B, S, T ] = FpStateFul( ( state : S ) => ( Some( ele ), state ) )

    override def get[ B ]( ele : FpStateFul[ B, S, T ] ) : Option[ B ] = None

    def get( state : S ) : Option[ A ] = exec( state )._1

}

object FpStateFul {

    def apply[ A, S <: FpState[ T ], T ]( e : S => (Option[ A ], S) ) : FpStateFul[ A, S, T ] = new FpStateFul[ A , S, T ] {
        override def exec : S => (Option[ A ], S) = e
    }

    def fromAct[ A, S <: FpState[ T ], T ]( a : FpAction[ T ]) : FpStateFul[ A, S ,T ]  = FpStateFul(
        ( s : S ) => ( None, a.transformState( s ).asInstanceOf[ S ] ) )

    def fromView[ A, S <: FpState[ T ], T ]( a : FpView[ T, Option[ A ] ]) : FpStateFul[ A, S ,T ]  = FpStateFul(
        ( s : S ) => ( s.see( a ), s ) )

    def fromActViewPre[ A, S <: FpState[ T ], T ]( a : FpAction[ T ], v : FpView[ T, Option[ A ] ]) : FpStateFul[ A, S ,T ]  = FpStateFul(
        ( s : S ) => ( v.surface( s.value ), a.transformState( s ).asInstanceOf[ S ] ) )

    def fromActViewPost[ A, S <: FpState[ T ], T ]( a : FpAction[ T ], v : FpView[ T, Option[ A ] ]) : FpStateFul[ A, S ,T ]  = FpStateFul(
        ( s : S ) => {
            val nextState = a.transformState( s ).asInstanceOf[ S ]
            ( v.surface( nextState.value ), nextState )
        } )

    def identity[ A, S <: FpState[ T ], T ] : FpStateFul[ A, S , T ] = FpStateFul( (s : S) => (None, s) )
}
