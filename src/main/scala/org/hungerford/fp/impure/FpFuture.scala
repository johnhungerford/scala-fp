package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpFailure, FpNone, FpOption, FpSome, FpSuccess, FpTry}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future, TimeoutException}

case class Timeout( ms : Long )

sealed trait FpFuture[ +T ] extends MonadCovariant[ FpFuture, T ] {
    val ec : ExecutionContext

    private final val outerThis = this

    def isComplete : Boolean

    def await[ B >: T ]( timeout : Long ) : FpTry[ B ]

    def await[ B >: T ] : FpTry[ B ]

    def impurify[ B >: T ] : FpImpureFuture[ B ] = FpImpureFuture( FpImpure( this ) )

    override def flatMap[ A, B ]( a : FpFuture[ A ] )
                                ( fn : A => FpFuture[ B ] ) : FpFuture[ B ] = FpFuture.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpFuture[ A ] = FpFuture { ele }( ec )

    override def equals( obj : Any ) : Boolean = {
        obj match {
            case fut : FpFuture[ _ ] =>
                await == fut.await
            case _ => false
        }
    }
}

sealed class FpFutureIncomplete[ T ]( io : FpImpure[ T ] )( implicit ecIn : ExecutionContext ) extends FpFuture[ T ] {
    override val ec = ecIn

    private val outerThis = this

    private[impure] var result : FpOption[ FpTry[ _ ] ] = FpNone

    override def isComplete : Boolean = result.isDefined

    final def await[ B >: T ]( timeout : Long ) : FpTry[ B ] = {
        val startTime = System.currentTimeMillis()
        var break = false
        do {
            if ( result.isDefined ) break = true
            if ( !break ) {
                Thread.sleep( 1 )
            }
        } while( System.currentTimeMillis() - startTime < timeout && !break )

        result match {
            case FpNone => FpFailure( new TimeoutException( s"Future took longer than $timeout ms to return result" ) )
            case FpSome( r : FpTry[ B ] ) => r
        }
    }

    override final def await[ B >: T ] : FpTry[ B ] = {
        var break = false
        do {
            if ( result.isDefined ) break = true
            if ( !break ) {
                Thread.sleep( 1 )
            }
        } while( !break )

        result match {
            case FpSome( r : FpTry[ B ] ) => r
            case _ => FpFailure( new IllegalStateException( "FpFuture should have completed finish open ended await" ) )
        }
    }

    ec.execute( new Runnable {
        override final def run( ) : Unit = {
            val res = io.run()
            var succeeded = false
            while ( !succeeded ) {
                FpTry( outerThis.result = FpSome( res ) ) match {
                    case FpSuccess( _ ) => succeeded = true
                    case _ =>
                }
            }
        }
    } )
}

sealed class FpFutureComplete[ T ]( res : FpTry[ T ] ) extends FpFuture[ T ] {
    override val ec : ExecutionContext = new ExecutionContext {
        override def execute( runnable : Runnable ) : Unit = ()
        override def reportFailure( cause : Throwable ) : Unit = ()
    }

    override def isComplete : Boolean = true

    final def await[ B >: T ]( timeout : Long ) : FpTry[ B ] = res

    override def await[ B >: T ] : FpTry[ B ] = res
}

object FpFuture extends MonadStatic[ FpFuture ] {
    def apply[ T ]( block : => T )( implicit ec : ExecutionContext ) : FpFuture[ T ] = new FpFutureIncomplete[T]( FpImpure( block ) )

    def fromImpure[ T ]( block : FpImpure[ T ] )( implicit ec : ExecutionContext ) : FpFuture[ T ] = new FpFutureIncomplete[T]( block )

    def fromTry[ T ]( block : FpTry[ T ] )( implicit ec : ExecutionContext ) : FpFuture[ T ] = block match {
        case FpFailure( t ) => failed( t )
        case FpSuccess( v ) => successful( v )
    }

    def fromFuture[ T ]( scalaFuture : Future[ T ] )( implicit ec : ExecutionContext ) : FpFuture[ T ] = {
        val io : FpImpure[ T ] = FpImpure.fromTry {
            @tailrec
            def tryRes : FpTry[ T ] = {
                val tr = FpTry( Await.result( scalaFuture, Duration( 1, scala.concurrent.duration.HOURS ) ) )
                tr match {
                    case FpFailure( _ : TimeoutException ) => tryRes
                    case r@FpSuccess( _ ) => r
                }
            }

            tryRes
        }

        new FpFutureIncomplete[T]( io )
    }

    def successful[ T ]( res : T ) : FpFuture[ T ] = new FpFutureComplete[T]( FpSuccess( res ) )

    def failed[ T ]( throwable : Throwable ) : FpFuture[ T ] = new FpFutureComplete[T]( FpFailure( throwable ) )

    override def flatMap[ A, B ]( a : FpFuture[ A ] )
                                ( fn : A => FpFuture[ B ] ) : FpFuture[ B ] = {
        fromImpure( FpImpure.fromTry[ B ]( {
           while ( !a.isComplete ) { Thread.sleep( 1 ) }
           a.await( 1 ).flatMap { b : A =>
               val f2 = fn( b )
               while ( !f2.isComplete ) { Thread.sleep( 1 ) }
               f2.await( 1 )
           }
        } ) )( a.ec )
    }

    override def unit[ A ]( ele : A ) : FpFuture[ A ] = apply( ele )( scala.concurrent.ExecutionContext.global )

}

case class FpImpureFuture[ +T ]( value : FpImpure[ FpFuture[ T ] ] ) extends MonadCovariant[ FpImpureFuture, T ] {
    override def flatMap[ A, B ]( a : FpImpureFuture[ A ] )
                                ( fn : A => FpImpureFuture[ B ] ) : FpImpureFuture[ B ] = FpImpureFuture.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpImpureFuture[ A ] = FpImpureFuture.unit( ele )

    override def equals( obj : Any ) : Boolean = obj match {
        case FpImpureFuture( thatValue ) => this.value == thatValue
        case _ => false
    }

    def run[ B >: T ]() : FpFuture[ B ] = value.run() match {
        case FpSuccess( fut ) => fut
        case FpFailure( t ) => FpFuture.failed( t )
    }

    def await[ B >: T ]( timeout : Long )( implicit ec : ExecutionContext ) : FpTry[ B ] = FpFuture {
        value.run() match {
            case res@FpFailure( t ) => res
            case FpSuccess( fut ) => fut.await
        }
    } await timeout flatMap { v => v }

    def await[ B >: T ]( implicit ec : ExecutionContext ) : FpTry[ B ] = FpFuture {
        value.run() match {
            case res@FpFailure( t ) => res
            case FpSuccess( fut ) => fut.await
        }
    }.await.flatMap( ( v : FpTry[ T ] ) => v )
}

object FpImpureFuture extends MonadStatic[ FpImpureFuture ] {
    def of[ T ]( block : => T )( implicit ec : ExecutionContext ) : FpImpureFuture[ T ] = FpImpureFuture( FpImpure( FpFuture( block ) ) )

    def fromTry[ T ]( fpTry : FpTry[ T ] )( implicit ec : ExecutionContext ) : FpImpureFuture[ T ] = FpImpureFuture {
        FpImpure.fromTry( fpTry ).map( v => FpFuture( v ) )
    }

    def fromImpure[ T ]( fpImpure : FpImpure[ T ] )( implicit ec : ExecutionContext ) : FpImpureFuture[ T ] = {
        FpImpureFuture( fpImpure.map( v => FpFuture( v ) ) )
    }

    def fromFuture[ T ]( fpFuture : FpFuture[ T ] ) : FpImpureFuture[ T ] = FpImpureFuture( FpImpure( fpFuture ) )

    override def flatMap[ A, B ]( a : FpImpureFuture[ A ] )
                                ( fn : A => FpImpureFuture[ B ] ) : FpImpureFuture[ B ] = {
        FpImpureFuture( a.value flatMap { ( fut : FpFuture[ A ] ) =>
            FpImpure( fut flatMap { v : A =>
                fn( v ).value.run() match {
                    case FpSuccess( res : FpFuture[ B ] ) => res
                    case FpFailure( t ) => FpFuture.failed( t )
                }
            } )
        } )
    }

    override def unit[ A ]( ele : A ) : FpImpureFuture[ A ] = FpImpureFuture.of( ele )( scala.concurrent.ExecutionContext.global )
}
