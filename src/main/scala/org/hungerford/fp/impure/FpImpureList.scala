package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpFailure, FpNone, FpOption, FpSome, FpSuccess, FpTry}
import org.hungerford.fp.collections.{FpList, FpNil, FpSeq, FpString}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpImpureList[ +T ] extends FpSeq[ T ] with MonadCovariant[ FpImpureList, T ] {
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
                    case FpImpureListEval( next, v ) => Call.from {
                        thisFn( i - 1, next ).map( ll => FpImpureListEval( ll, v ) )
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
            case FpImpureListEval( next, v ) => Call.from {
                thisFn( next ).map( ll => FpImpureListEval( ll, v ) )
            }
        }
    }( this )

    final def withTry : FpImpureList[ FpTry[ T ] ] = this match {
        case FpImpureNil => FpImpureNil
        case FpImpureListEval( next, v ) => FpImpureList {
            FpImpureListEval( next.withTry, FpSuccess( v ) )
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
            case FpImpureListEval( next, head ) => Call.from {
                thisFn( next ).map( l => l + head )
            }
            case FpImpureUnevaluatedList( imp ) => Call.from {
                thisFn( imp.run().getOrElse( FpImpureNil ) )
            }
        }
    }( this )

    override def +[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    override def ++[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = FpImpureList.combine( this, FpImpureList.fromFpList( that.toFpList ) )

    override def headOption : FpOption[ T ] = this match {
        case FpImpureNil => FpNone
        case FpImpureListEval( _, head ) => FpSome( head )
        case FpImpureUnevaluatedList( impure ) => impure.run() match {
            case FpSuccess( next ) => next.headOption
            case FpFailure( _ ) => FpNone
        }
    }

    override def tailOption : FpOption[ FpImpureList[ T ] ] = this match {
        case FpImpureNil => FpNone
        case FpImpureListEval( tail, _ ) => FpSome( tail )
        case FpImpureUnevaluatedList( impure ) => impure.run() match {
            case FpSuccess( next ) => next.tailOption
            case FpFailure( _ ) => FpNone
        }
    }

    override def append[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    override def fpString : FpString = ???

    override def reverse : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        ( thisFn : FpImpureList[ T ] => StackSafe[ FpImpureList[ T ] ] ) => {
            case FpImpureNil => Result( FpImpureNil )
            case uneval@FpImpureUnevaluatedList( _ ) => Result( FpImpureUnevaluatedList {
                FpImpure( uneval.evaluate().reverse.evaluate() )
            } )
            case FpImpureListEval( next, v ) => Call.from {
                thisFn( next ).map( ll => ll ++ FpImpureListEval( FpImpureNil, v ) )
            }
        }
    }( this )

    override def length : Int = ???

    override def times( num : Int ) : FpImpureList[ T ] = ???

    override def filter( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def take( num : Int ) : FpImpureList[ T ] = {
        val res = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
            thisFn =>
                ( i, ll ) =>
                    if ( i < 1 ) Result( FpImpureNil ) else ll match {
                        case FpImpureNil => Result( FpImpureNil )
                        case FpImpureListEval( tail, head ) => Call.from {
                            thisFn( i - 1, tail ).map( ll => FpImpureList( FpImpureListEval( ll, head ) ) )
                        }
                        case uneval@FpImpureUnevaluatedList( _ ) => Result( FpImpureList( StackSafe( thisFn( i, uneval.evaluate() ) ) ) )
                    }
        }( if ( num < 0 ) -num else num, if ( num < 0 ) this.reverse else this )
        if ( num < 0 ) res.reverse else res
    }

    override def takeWhileEnd( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def takeWhile( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def drop( num : Int ) : FpImpureList[ T ] = ???

    override def dropWhileEnd( fn : T => Boolean ) : FpImpureList[ T ] = ???

    override def dropWhile( fn : T => Boolean ) : FpImpureList[ T ] = ???

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
        case FpImpureListEval( nextLeftIl, vLeft ) => that match {
            case FpImpureNil => FpImpureNil
            case FpImpureListEval( nextRightIl, vRight ) => FpImpureList {
                FpImpureListEval( nextLeftIl.zipWith( nextRightIl ), (vLeft, vRight) )
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
                    case FpImpureListEval( next, v ) => Call.from {
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

    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = FpImpureList.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = FpImpureList.unit( ele )

    private def toStringInternal[ A ]( ll : FpImpureList[ A ] ) : String = StackSafe.selfCall[ FpImpureList[ A ], String ] {
        thisFn => {
            case FpImpureNil => Result( "FpImpureNil" )
            case FpImpureUnevaluatedList( _ ) => Result( "??" )
            case FpImpureListEval( tail, head ) => Call.from {
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

case class FpImpureListEval[ +T ]( tail : FpImpureList[ T ], head : T ) extends FpImpureEvaluatedList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

object FpImpureList extends MonadStatic[ FpImpureList ] {

    def apply[ T ]( il : => FpImpureList[ T ] ) : FpImpureList[ T ] = FpImpureUnevaluatedList( FpImpure( il.evaluate() ) )

    def of[ T ]( v : =>T ) : FpImpureList[ T ] = apply( FpImpureListEval( FpImpureNil, v ) )

    def fromFpList[ A ]( fpList : FpList[ A ] ) : FpImpureList[ A ] = StackSafe.selfCall[ FpList[ A ], FpImpureList[ A ] ] {
        thisFn => {
            case FpNil => Result( FpImpureNil )
            case FpList( tail, head ) => Call.from {
                thisFn( tail ).map( ll => FpImpureList( FpImpureListEval( ll, head ) ) )
            }
        }
    }( fpList )

    def combine[ A ]( a : FpImpureList[ A ], b : A ) : FpImpureList[ A ] = FpImpureList( FpImpureListEval( a, b ) )

    def combine[ A ]( a : FpImpureList[ A ], b : FpImpureList[ A ] ) : FpImpureList[ A ] = {
        if ( a == FpImpureNil ) b else if ( b == FpImpureNil ) a else a match {
            case FpImpureNil => b
            case uneval@FpImpureUnevaluatedList( _ ) => FpImpureList( combine( uneval.evaluate(), b ) )
            case FpImpureListEval( tail, head ) => FpImpureList {
                FpImpureListEval( combine( tail, b ), head )
            }
        }
    }


    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = a match {
        case FpImpureNil => FpImpureNil
        case uneval@FpImpureUnevaluatedList( imp ) => FpImpureList( flatMap( uneval.evaluate() )( fn ) )
        case FpImpureListEval( tail : FpImpureList[ A ], head ) => FpImpureList {
            fn( head ).evaluate() match {
                case FpImpureNil => tail.flatMap( fn )
                case FpImpureListEval( nextTail, nextHead ) =>
                    FpImpureListEval( FpImpureList( nextTail ++ tail.flatMap( fn ) ), nextHead )
            }
        }
    }

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = FpImpureList.of( ele )
}
