package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpFailure, FpNone, FpOption, FpSome, FpSuccess, FpTry}
import org.hungerford.fp.collections.{FpList, FpNil, FpSeq, FpString}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadStatic}

sealed trait FpImpureList[ +T ] extends FpSeq[ T ] with Monad[ FpImpureList, T ] {
    override val static : MonadStatic[ FpImpureList ] = FpImpureList

    private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ]

    override def apply[ B >: T ]( index : Int ) : FpOption[ B ] = ???

    final def evaluate( ) : FpImpureEvaluatedList[ T ] = impure.run() match {
        case FpSuccess( res ) => res
        case FpFailure( _ ) => FpImpureNil
    }

    final def evaluateTry( ) : FpTry[ FpImpureEvaluatedList[ T ] ] = impure.run()

    final def evaluate( num : Int ) : FpImpureList[ T ] = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn =>
            (i, il) =>
                if ( i < 1 ) Result( il ) else il match {
                    case FpImpureUnevaluatedList( imp ) => imp.run() match {
                        case FpSuccess( res ) => Call.from( thisFn( i, res ) )
                        case FpFailure( _ ) => Result( FpImpureNil )
                    }
                    case FpImpureNil => Result( FpImpureNil )
                    case FpImpureListEval( v, next ) => Call.from {
                        thisFn( i - 1, next ).map( ll => FpImpureListEval( v, ll ) )
                    }
                }
    }( num, this )

    final def evaluateAll() : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureEvaluatedList[ T ] ] {
        thisFn => {
            case FpImpureUnevaluatedList( imp ) => imp.run() match {
                case FpSuccess( res ) => Call.from( thisFn( res ) )
                case FpFailure( _ ) => Result( FpImpureNil )
            }
            case FpImpureNil => Result( FpImpureNil )
            case FpImpureListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => FpImpureListEval( v, ll ) )
            }
        }
    }( this )

    final def withTry : FpImpureList[ FpTry[ T ] ] = this match {
        case FpImpureNil => FpImpureNil
        case FpImpureListEval( v, next ) => FpImpureList {
            FpImpureListEval( FpSuccess( v ), next.withTry )
        }
        case FpImpureUnevaluatedList( imp ) => FpImpureList {
            imp.run() match {
                case FpSuccess( il ) => il.withTry
                case FpFailure( t ) => FpImpureList.of( FpFailure( t ) )
            }
        }
    }

    override def toFpList : FpList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpNil )
            case FpImpureListEval( head, next ) => Call.from {
                thisFn( next ).map( l => head +: l )
            }
            case FpImpureUnevaluatedList( imp ) => Call.from {
                thisFn( imp.run().getOrElse( FpImpureNil ) )
            }
        }
    }( this )

    override def :+[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, FpImpureList.of( that ) )
    override def +:[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    override def :++[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = FpImpureList.combine( this, FpImpureList.fromFpList( that.toFpList ) )
    override def ++:[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = FpImpureList.combine( this, FpImpureList.fromFpList( that.toFpList ) )

    override def headOption : FpOption[ T ] = this match {
        case FpImpureNil => FpNone
        case FpImpureListEval( head, _ ) => FpSome( head )
        case FpImpureUnevaluatedList( impure ) => impure.run() match {
            case FpSuccess( next ) => next.headOption
            case FpFailure( _ ) => FpNone
        }
    }

    override def tailOption : FpOption[ FpImpureList[ T ] ] = this match {
        case FpImpureNil => FpNone
        case FpImpureListEval( _, tail ) => FpSome( tail )
        case FpImpureUnevaluatedList( impure ) => impure.run() match {
            case FpSuccess( next ) => next.tailOption
            case FpFailure( _ ) => FpNone
        }
    }

    override def reverse : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        ( thisFn : FpImpureList[ T ] => StackSafe[ FpImpureList[ T ] ] ) => {
            case FpImpureNil => Result( FpImpureNil )
            case uneval@FpImpureUnevaluatedList( _ ) => Result( FpImpureUnevaluatedList {
                FpImpure( uneval.evaluate().reverse.evaluate() )
            } )
            case FpImpureListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => ll :++ FpImpureListEval( v, ll ) )
            }
        }
    }( this )

    override def lengthOpt : FpOption[ Int ] = ???

    override def times( num : Int ) : FpImpureList[ T ] = ???

    override def filter( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def take( num : Int ) : FpImpureList[ T ] = {
        val res = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
            thisFn =>
                ( i, ll ) =>
                    if ( i < 1 ) Result( FpImpureNil ) else ll match {
                        case FpImpureNil => Result( FpImpureNil )
                        case FpImpureListEval( head, tail ) => Call.from {
                            thisFn( i - 1, tail ).map( ll => FpImpureList( FpImpureListEval( head, ll ) ) )
                        }
                        case uneval@FpImpureUnevaluatedList( _ ) => Result( FpImpureList( StackSafe( thisFn( i, uneval.evaluate() ) ) ) )
                    }
        }( if ( num < 0 ) -num else num, if ( num < 0 ) this.reverse else this )
        if ( num < 0 ) res.reverse else res
    }

    override def takeWhile( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def drop( num : Int ) : FpImpureList[ T ] = ???

    override def dropWhile( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def slice( start : Int, end : Int ) : FpImpureList[ T ] = drop( start ).take( end - start )

    override def exists( fn : T => Boolean ) : Boolean = ???

    override def contains[ B >: T ]( ele : B ) : Boolean = ???

    override def distinct : FpSeq[ T ] = ???

    override def partition( fn : T => Boolean ) : (FpSeq[ T ], FpSeq[ T ]) = ???

    override def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpSeq[ B ] = ???

    override def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpSeq[ B ] = ???

    override def sortBy[ B >: T, C ]( fn : B => C )( implicit ord : Ordering[ C ] ) : FpSeq[ B ] = ???

    override def sortWith[ B >: T ]( cmp : (B, B) => Int ) : FpSeq[ B ] = ???

    override def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpImpureList[ (B, C) ] = this match {
        case FpImpureNil => FpImpureNil
        case FpImpureListEval( vLeft, nextLeftIl ) => that match {
            case FpImpureNil => FpImpureNil
            case FpImpureListEval( vRight, nextRightIl ) => FpImpureList {
                FpImpureListEval( (vLeft, vRight), nextLeftIl.zipWith( nextRightIl ) )
            }
            case uneval@FpImpureUnevaluatedList( _ ) => FpImpureList( this.zipWith( uneval.evaluate() ) )
            case _ : FpSeq[ _ ] => this.zipWith( FpImpureList.fromFpList( that.toFpList ) )
        }
        case uneval@FpImpureUnevaluatedList( _ ) => FpImpureList( uneval.evaluate().zipWith( that ) )
    }

    override def zipWithIndex[ B >: T ] : FpSeq[ (B, Int) ] = ???

    override def withLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    override def withRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    def reduce[ B >: T ]( fn : (B, B) => B ) : FpImpure[ B ] = ???

    def foldLeft[ A, B >: T ]( aggregation : A )( fn : (A, B) => A ) : FpImpure[ A ] = StackSafe.selfCall2[ A, FpImpureList[ B ], FpImpure[ A ] ] {
        thisFn =>
            ( agg : A, il : FpImpureList[ B ] ) =>
                il match {
                    case FpImpureNil => Result( FpImpure( agg ) )
                    case FpImpureListEval( v, next ) => Call.from {
                        thisFn( fn( agg, v ), next )
                    }
                    case FpImpureUnevaluatedList( imp ) => Result( StackSafe {
                        imp.run() match {
                            case FpSuccess( res : FpImpureEvaluatedList[ B ] ) => thisFn( agg, res )
                            case FpFailure( _ ) => Result( FpImpure( agg ) )
                        }
                    } )
                }
    }( aggregation, this )

    def foldRight[ A, B >: T ]( aggregation : A )( fn : (A, B) => A ) : FpImpure[ A ] = this.reverse.foldLeft( aggregation )( fn )

    private def toStringInternal[ A ]( ll : FpImpureList[ A ] ) : String = StackSafe.selfCall[ FpImpureList[ A ], String ] {
        thisFn => {
            case FpImpureNil => Result( "FpImpureNil" )
            case FpImpureUnevaluatedList( _ ) => Result( "??" )
            case FpImpureListEval( head, tail ) => Call.from {
                thisFn( tail ).map( tailString => s"${head.toString} + ${tailString}" )
            }
        }
    }( ll )

    override def toString : String = s"[${toStringInternal( this )}]"

}

case class FpImpureUnevaluatedList[ +T ]( override private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ] ) extends FpImpureList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

sealed trait FpImpureEvaluatedList[ +T ] extends FpImpureList[ T ] {
    private[ impure ] val impure : FpImpure[ FpImpureEvaluatedList[ T ] ] = FpImpure( this )
}

case object FpImpureNil extends FpImpureEvaluatedList[ Nothing ]

case class FpImpureListEval[ +T ]( head : T, tail : FpImpureList[ T ] ) extends FpImpureEvaluatedList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

object FpImpureList extends MonadStatic[ FpImpureList ] {

    def apply[ T ]( il : => FpImpureList[ T ] ) : FpImpureList[ T ] = FpImpureUnevaluatedList( FpImpure( il.evaluate() ) )

    def of[ T ]( v : =>T ) : FpImpureList[ T ] = apply( FpImpureListEval( v, FpImpureNil ) )

    def fromFpList[ A ]( fpList : FpList[ A ] ) : FpImpureList[ A ] = StackSafe.selfCall[ FpList[ A ], FpImpureList[ A ] ] {
        thisFn => {
            case FpNil => Result( FpImpureNil )
            case FpList( tail, head ) => Call.from {
                thisFn( tail ).map( ll => FpImpureList( FpImpureListEval( head, ll ) ) )
            }
        }
    }( fpList )

    def combine[ A ]( a : FpImpureList[ A ], b : A ) : FpImpureList[ A ] = FpImpureList( FpImpureListEval( b, a ) )

    def combine[ A ]( a : FpImpureList[ A ], b : FpImpureList[ A ] ) : FpImpureList[ A ] = {
        if ( a == FpImpureNil ) b else if ( b == FpImpureNil ) a else a match {
            case FpImpureNil => b
            case uneval@FpImpureUnevaluatedList( _ ) => FpImpureList( combine( uneval.evaluate(), b ) )
            case FpImpureListEval( head, tail ) => FpImpureList {
                FpImpureListEval( head, combine( tail, b ) )
            }
        }
    }


    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = a match {
        case FpImpureNil => FpImpureNil
        case uneval@FpImpureUnevaluatedList( imp ) => FpImpureList( flatMap( uneval.evaluate() )( fn ) )
        case FpImpureListEval( head, tail : FpImpureList[ A ] ) => FpImpureList {
            fn( head ).evaluate() match {
                case FpImpureNil => tail.flatMap( fn )
                case FpImpureListEval( nextHead, nextTail ) =>
                    FpImpureListEval( nextHead, FpImpureList( nextTail :++ tail.flatMap( fn ) ) )
            }
        }
    }

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = FpImpureList.of( ele )
}
