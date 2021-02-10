package org.hungerford.fp.basic

import org.hungerford.fp.types.{Monad, MonadStatic, WithTransformer}


class FpState[ StateType, +ReturnType ]( private val fn : StateType => (ReturnType, StateType) )
  extends Monad[ ({ type U[ +X ] = FpState[ StateType, X ] })#U, ReturnType ] {
    def apply( state : StateType ) : (ReturnType, StateType) = fn( state )

    def * ( numTimes : Int ) : FpState[ StateType, ReturnType ] = {
        if ( numTimes <= 0 ) throw new IllegalArgumentException( s"Cannot repeat state operation less than once: ${numTimes}" )
        else if ( numTimes <= 1 ) this
        else this.flatMap( _ => *( numTimes - 1 ) )
    }

    def >>[ NewReturnType ]( fpState : FpState[ StateType, NewReturnType ]) : FpState[ StateType, NewReturnType ] = flatMap( _ => fpState )

    override val static : MonadStatic[ ( {
        type U[ +X ] = FpState[ StateType, X ]
    } )#U ] = FpState.Static[ StateType ]
}

object FpState {
    def apply[ StateType, ReturnType ]( fn : StateType => (ReturnType, StateType) ) = new FpState[ StateType, ReturnType ]( fn )

    def Nothing[ StateType ] : FpState[ StateType, Unit ] = FpState[ StateType, Unit ]( ( state : StateType ) => ((), state) )

    def Peek[ StateType ] : FpState[ StateType, StateType ] = FpState[ StateType, StateType ]( ( state : StateType ) => (state, state) )

    def Mutate[ StateType ]( fn : StateType => StateType ) : FpState[ StateType, Unit ] = FpState[ StateType, Unit ]( ( state : StateType ) => ((), fn( state )) )

    def Static[ StateType ] : MonadStatic[ ( { type U[ +X ] = FpState[ StateType, X ] } )#U ] = {

        new MonadStatic[ ( { type U[ +X ] = FpState[ StateType, X ] } )#U ] {
            override def flatMap[ A, B ]( a : FpState[ StateType, A ] )( fn : A => FpState[ StateType, B ] ) : FpState[ StateType, B ] = {
                FpState[ StateType, B ] { state : StateType =>
                    val (output, newState) = a( state )
                    fn( output )( newState )
                }
            }

            override def unit[ A ]( ele : A ) : FpState[ StateType, A ] = FpState[ StateType, A ]( state => ( ele, state ) )
        }
    }

}


