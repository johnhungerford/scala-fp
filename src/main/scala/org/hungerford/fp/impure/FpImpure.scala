package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpSuccess, FpTry}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadCovariant, MonadStatic, Monoid, MonoidStatic}

import scala.concurrent.ExecutionContext

// An effects type
sealed trait FpImpure[ +T ] extends MonadCovariant[ FpImpure, T ] with Monoid[ FpImpure[ _ ] ] {
    def run : () => FpTry[ T ]

    override def flatMap[ A, B ]( a : FpImpure[ A ] )( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = FpImpure.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure.unit( ele )

    override def empty : FpImpure[ Unit ] = FpImpure.empty

    override def combine[ B ]( a : FpImpure[ _ ], b : FpImpure[ _ ] ) : FpImpure[ _ ] = FpImpure.combine( a, b )

    def >>[ U ]( fpImpure : FpImpure[ U ] ) : FpImpure[ U ] = this.flatMap( _ => fpImpure )

    def loop( times : Int ) : FpImpure[ T ] = FpImpure.loop( times )( this )

    def loop : FpImpure[ Unit ] = FpImpure.loop( this )

    def async[ B >: T ]( implicit ec : ExecutionContext ) : FpImpureFuture[ B ] = FpImpureFuture.fromImpure( this )

    private def doWhileStatic[ B >: T ] : (FpImpure[ B ], B => Boolean) => FpImpure[ B ] = StackSafe.selfCall2[ FpImpure[ B ], B => Boolean, FpImpure[ B ] ] {
        thisFn =>
            ( ele : FpImpure[ B ], cnd : B => Boolean ) =>
                ele.run() match {
                    case FpSuccess( res ) if ( !cnd( res ) ) => Result( FpImpure( res ) )
                    case _ =>
                        Call.from {
                            thisFn( ele, cnd )
                        }
                }
    }

    final def doWhile( condition : T => Boolean ) : FpImpure[ T ] = FpImpure.fromTry { doWhileStatic( this, condition ).run() }

    override def equals( obj : Any ) : Boolean = obj match {
        case fpi : FpImpure[ _ ] => fpi.run() == this.run()
        case _ => false
    }
}

object FpImpure extends MonadStatic[ FpImpure ] with MonoidStatic[ FpImpure[ _ ] ] {
    def fromTry[ A ]( block : => FpTry[ A ] ) : FpImpure[ A ] = new FpImpure[A] {
        override def run : ( ) => FpTry[ A ] = () => block
    }

    def apply[ A ]( block : => A ) : FpImpure[ A ] = new FpImpure[ A ] {
        override def run : ( ) => FpTry[ A ] = ( ) => FpTry( block )
    }

    def loop[ A ]( times : Int )( impure : FpImpure[ A ] ) : FpImpure[ A ] = FpImpure.fromTry ( StackSafe.selfCall2[ Int, FpImpure[ A ], FpTry[ A ] ] {
        thisFn =>
            ( n : Int, imp : FpImpure[ A ] ) =>
                if ( n <= 1 ) Result( imp.run() )
                else {
                    imp.run()
                    Call.from {
                        thisFn( n - 1, imp )
                    }
                }
    }( times, impure ) )

    def loop[ A ]( impure : FpImpure[ A ] ) : FpImpure[ Unit ] = FpImpure( StackSafe.selfCall[ FpImpure[ A ], A ] {
        thisFn => imp =>
            imp.run() match {
                case _ => Call.from {
                    thisFn( imp )
                }
            }
    }( impure ) )

    override def flatMap[ A, B ]( a : FpImpure[ A ] )
                                ( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = FpImpure.fromTry ( a.run().flatMap( ( res : A ) => fn( res ).run() ) )

    override def empty : FpImpure[ Unit ] = FpImpure[ Unit ]( () => () )

    override def combine[ B ]( a : FpImpure[ _ ], b : FpImpure[ _ ] ) : FpImpure[ _ ] = a >> b

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure ( ele )
}
