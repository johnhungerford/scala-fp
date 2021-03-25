package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpFailure, FpSuccess, FpTry}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadStatic}

import scala.concurrent.ExecutionContext

// An effects type
sealed trait FpImpure[ +T ] extends Monad[ FpImpure, T ] {
    private[impure] val ss : StackSafe[ FpTry[ T ] ]

    def run : () => FpTry[ T ] = () => ss.run()

    override val static : MonadStatic[ FpImpure ] = FpImpure

    def loop( times : Int ) : FpImpure[ T ] = FpImpure.loop( times )( this )

    def loop : FpImpure[ Unit ] = FpImpure.loop( this )

    def async[ B >: T ]( implicit ec : ExecutionContext ) : FpImpureFuture[ B ] = FpImpureFuture.fromImpure( this )

    private def doWhileStatic[ B >: T ]( imp : FpImpure[ B ] )( fn : B => Boolean) : FpImpure[ B ] = StackSafe.selfCall2[ FpImpure[ B ], B => Boolean, FpImpure[ B ] ] {
        thisFn =>
            ( ele : FpImpure[ B ], cnd : B => Boolean ) =>
                Call.from {
                    ele.ss.flatMap {
                        case FpSuccess( res ) if !cnd( res  ) => Result( FpImpure( res ) )
                        case _ => Call.from {
                            thisFn( ele, cnd )
                        }
                    }
                }
    } ( imp, fn )

    final def doWhile( condition : T => Boolean ) : FpImpure[ T ] = FpImpure.fromTry { doWhileStatic( this )( condition ).run() }

    override def equals( obj : Any ) : Boolean = obj match {
        case fpi : FpImpure[ _ ] => fpi.run() == this.run()
        case _ => false
    }
}

object FpImpure extends MonadStatic[ FpImpure ] {

    def fromTry[ A ]( block : => FpTry[ A ] ) : FpImpure[ A ] = new FpImpure[A] {
        override private[impure] val ss : StackSafe[ FpTry[ A ] ] = Call.from( Result( block ) )
    }

    def apply[ A ]( block : => A ) : FpImpure[ A ] = new FpImpure[ A ] {
        override private[impure] val ss : StackSafe[ FpTry[ A ] ] = Call.from( Result( FpTry( block ) ) )
    }

    private[ impure ] def fromSs[ A ]( stackSafe : => StackSafe[ FpTry[ A ] ] ) : FpImpure[ A ] = new FpImpure[ A ] {
        override private[ impure ] val ss = stackSafe
    }

    def loop[ A ]( times : Int )( impure : FpImpure[ A ] ) : FpImpure[ A ] = FpImpure.fromTry ( StackSafe.selfCall2[ Int, FpImpure[ A ], FpTry[ A ] ] {
        thisFn =>
            ( n : Int, imp : FpImpure[ A ] ) =>
                if ( n <= 1 ) Call.from( imp.ss )
                else {
                    Call.from {
                        imp.ss.run()
                        thisFn( n - 1, imp )
                    }
                }
    }( times, impure ) )

    def loop[ A ]( impure : FpImpure[ A ] ) : FpImpure[ Unit ] = FpImpure.fromTry( StackSafe.selfCall[ FpImpure[ A ], FpTry[ A ] ] {
        thisFn => stackSafe =>
          Call.from {
              stackSafe.run()
              thisFn( stackSafe )
          }
    }( impure ) ).map( _ => () )

    override def flatMap[ A, B ]( a : FpImpure[ A ] )
                                ( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = {
        fromSs( a.ss.flatMap {
            case FpSuccess( res ) => fn( res ).ss
            case failed@FpFailure( _ ) => Result( failed )
        } )
    }

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure ( ele )

}
