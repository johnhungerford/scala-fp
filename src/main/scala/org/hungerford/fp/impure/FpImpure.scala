package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpSuccess, FpTry}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, Monoid}

import scala.annotation.tailrec

// An effects type
trait FpImpure[ T ] extends Monad[ FpImpure, T ] with Monoid[ FpImpure[ _ ] ] {
    def run : () => FpTry[ T ]

    override def flatMap[ A, B ]( a : FpImpure[ A ] )( fn : A => FpImpure[ B ] ) : FpImpure[ B ] = FpImpure.fromTry ( a.run().flatMap( ( res : A ) => fn( res ).run() ) )

    override def unit[ A ]( ele : A ) : FpImpure[ A ] = FpImpure ( ele )

    override def empty : FpImpure[ Unit ] = FpImpure( () => () )

    override def combine[ B ]( a : FpImpure[ _ ], b : FpImpure[ _ ] ) : FpImpure[ _ ] = a >> b

    def >>[ U ]( fpImpure : FpImpure[ U ] ) : FpImpure[ U ] = this.flatMap( _ => fpImpure )

    def loop( times : Int ) : FpImpure[ T ] = FpImpure.loop( times )( this )

    def loop : FpImpure[ Unit ] = FpImpure.loop( this )

    private val doWhileStatic : (FpImpure[ T ], T => Boolean) => FpImpure[ T ] = StackSafe.selfCall2[ FpImpure[ T ], T => Boolean, FpImpure[ T ] ] {
        thisFn =>
            ( ele : FpImpure[ T ], cnd : T => Boolean ) =>
                ele.run() match {
                    case FpSuccess( res ) if ( !cnd( res ) ) => Result( FpImpure( res ) )
                    case _ =>
                        Call.from {
                            thisFn( ele, cnd )
                        }
                }
    }

    final def doWhile( condition : T => Boolean ) : FpImpure[ T ] = FpImpure.fromTry { doWhileStatic( this, condition ).run() }
}

object FpImpure {
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
}
