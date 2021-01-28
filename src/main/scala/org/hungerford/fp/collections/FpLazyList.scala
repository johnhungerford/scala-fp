package org.hungerford.fp.collections

import org.hungerford.fp.basic.{FpNone, FpOption, FpSome}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{MonadCovariant, MonadStatic}

sealed trait FpLazyList[ +T ] extends FpSeq[ T ] with MonadCovariant[ FpLazyList, T ] {
    private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ]

    def evaluate[ B >: T ]( num : Int ) : FpLazyList[ B ] = StackSafe.selfCall2[ Int, FpLazyList[ B ], FpLazyList[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
            case FpLazyNil => Result( FpLazyNil )
            case FpLazyListEval( next, v ) => Call.from {
                thisFn(i - 1, next).map( ll => FpLazyListEval( ll, v ) )
            }
            case FpUnevaluatedLazyList( evalFn ) => Call.from {
                thisFn( i, evalFn() )
            }
        }
    }( num, this )

    def evaluateAll() : FpLazyList[ T ] =  StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
                case FpLazyNil => Result( FpLazyNil )
                case FpLazyListEval( next, v ) => Call.from {
                    thisFn( next ).map( ll => FpLazyListEval( ll, v ) )
                }
                case FpUnevaluatedLazyList( evalFn ) => Call.from {
                    thisFn( evalFn() )
                }
            }
    }( this )

    override def apply[ B >: T ]( index : Int ) : FpOption[ B ] = StackSafe.selfCall2 [ Int, FpLazyList[ B ], FpOption[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 0 ) Result( FpNone ) else ll match {
                case FpLazyNil => Result( FpNone )
                case FpLazyListEval( next, v ) =>
                    if ( i == 0 ) Result( FpSome( v ) ) else Call.from {
                        thisFn( i - 1, next )
                    }
                case FpUnevaluatedLazyList( evalFn ) => Call.from {
                    thisFn( i, evalFn() )
                }
            }
    }( index, this )

    def toFpList : FpList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpNil )
            case FpLazyListEval( next, head ) => Call.from {
                thisFn( next ).map( l => l + head )
            }
            case FpUnevaluatedLazyList( evalFn ) => Call.from {
                thisFn( evalFn() )
            }
        }
    }( this )

    def +[ B >: T ]( that : B ) : FpLazyList[ B ] = FpLazyList.combine( this, that )

    override def ++[ B >: T ]( that : FpSeq[ B ] ) : FpLazyList[ B ] = that match {
        case ll : FpLazyList[ B ] => FpLazyList.combine( this, ll )
        case _ => FpLazyList.combine( this, FpLazyList.fromFpList( that.toFpList ) )
    }

    override def headOption : FpOption[ T ] = this match {
        case FpUnevaluatedLazyList( evalFn ) => evalFn().headOption
        case FpLazyNil => FpNone
        case FpLazyListEval( _, head ) => FpSome( head )
    }

    override def tailOption : FpOption[ FpSeq[ T ] ] = this match {
        case FpUnevaluatedLazyList( evalFn ) => evalFn().tailOption
        case FpLazyNil => FpNone
        case FpLazyListEval( tail, _ ) => FpSome( tail )
    }

    override def append[ B >: T ]( ele : B ) : FpLazyList[ B ] = this + ele

    override def fpString : FpString = this.toFpList.fpString

    override def reverse : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        ( thisFn : FpLazyList[ T ] => StackSafe[FpLazyList[ T ] ] ) => {
            case FpLazyNil => Result( FpLazyNil )
            case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList { () =>
                evalFn().reverse.evaluate()
            } )
            case FpLazyListEval( next, v ) => Call.from {
                thisFn( next ).map( ll => ll ++ FpLazyListEval( FpLazyNil, v ) )
            }
        }
    }( this )

    override def length : Int = StackSafe.selfCall2[ Int, FpLazyList[ T ], Int ] {
        thisFn =>
            ( i : Int, ll : FpLazyList[ T ] ) => ll match {
                case FpLazyNil => Result( i )
                case FpLazyListEval( next, _ ) => Call.from {
                    thisFn( i + 1, next )
                }
                case FpUnevaluatedLazyList( evalFn ) => Call.from {
                    thisFn( i, evalFn() )
                }
            }
    }( 0, this )

    override def times( num : Int ) : FpSeq[ T ] = {
                if ( num < 1 ) FpLazyNil
                else if ( num == 1 ) this
                else FpLazyList.makeLazy(
                    this ++ this.times( num - 1 )
                )
    }

    override def filter( fn : T => Boolean ) : FpLazyList[ T ] = this.flatMap( t => {
        if ( fn( t  ) ) unit( t )
        else FpLazyNil
    } )

    override def take( num : Int ) : FpLazyList[ T ] = {
        if ( num < 1 ) FpLazyNil else this match {
            case FpLazyNil => FpLazyList.makeLazy( FpLazyNil )
            case FpLazyListEval( tail, head ) => FpLazyList.makeLazy {
                FpLazyListEval( tail.take( num - 1 ), head )
            }
            case FpUnevaluatedLazyList( evalFn ) => FpUnevaluatedLazyList( ( ) => {
                evalFn().take( num ).evaluate()
            } )
        }
    }


    override def takeWhileEnd( fn : T => Boolean ) : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
                case FpLazyNil => Result( FpLazyNil )
                case FpLazyListEval( tail, head ) => if ( !fn( head ) ) Result( FpLazyNil ) else Call.from {
                    thisFn( tail ).map( ll => FpLazyListEval( ll, head ) )
                }
                case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList( () => {
                    evalFn().takeWhileEnd( fn ).evaluate()
                } ))
            }
    }( this )

    override def takeWhile( fn : T => Boolean ) : FpSeq[ T ] = this.reverse.takeWhileEnd( fn ).reverse

    override def drop( num : Int ) : FpLazyList[ T ] = {
        val res = StackSafe.selfCall2[ Int, FpLazyList[ T ], FpLazyList[ T ] ] {
            thisFn =>
                (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
                    case FpLazyNil => Result( FpLazyNil )
                    case FpLazyListEval( next, v ) => Call.from {
                        thisFn( i - 1, next ).map( nextL => FpLazyListEval( nextL, v ) )
                    }
                    case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList( () => {
                        StackSafe( thisFn( i, evalFn() ) ).evaluate()
                    } ) )
                }
        }( if ( num < 0 ) -num else num, if ( num < 0 ) this else this.reverse )
        if ( num < 0 ) res else res.reverse
    }

    override def dropWhileEnd( fn : T => Boolean ) : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case ll@FpLazyListEval( tail, head ) => if ( !fn( head ) ) Result( ll ) else Call.from {
                thisFn( tail )
            }
            case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList( () => {
                evalFn().dropWhileEnd( fn ).evaluate()
            } ))
        }
    }( this )

    override def dropWhile( fn : T => Boolean ) : FpSeq[ T ] = this.reverse.dropWhileEnd( fn ).reverse

    override def exists( fn : T => Boolean ) : Boolean = StackSafe.selfCall[ FpLazyList[ T ], Boolean ] {
        thisFn => {
            case FpLazyNil => Result( false )
            case FpLazyListEval( next, v ) => if ( fn( v ) ) Result( true ) else Call.from {
                thisFn( next )
            }
            case FpUnevaluatedLazyList( evalFn ) => Call.from {
                thisFn( evalFn() )
            }
        }
    }( this )

    override def contains[ B >: T ]( ele : B ) : Boolean = exists( _ == ele )

    override def distinct : FpLazyList[ T ] = StackSafe.selfCall2[ FpLazyList[ T ], Set[ T ], FpLazyList[ T ] ] {
        thisFn =>
            ( ll, set ) =>
                ll match {
                    case FpLazyNil => Result( FpLazyNil )
                    case FpLazyListEval( next, v ) if ( set.contains( v ) ) => Call.from {
                        thisFn( next, set )
                    }
                    case FpLazyListEval( next, v ) => Call.from {
                        thisFn( next, set + v ).map( _ + v )
                    }
                    case FpUnevaluatedLazyList( evalFn ) => Result( FpUnevaluatedLazyList( ( ) => {
                        StackSafe( thisFn( evalFn(), set ) ).evaluate()
                    } ) )
                }
    }( this, Set.empty[ T ] )

    override def partition( fn : T => Boolean ) : (FpSeq[ T ], FpSeq[ T ]) = {
        (this.filter( fn ), this.filter( t => !fn( t ) ))
    }

    override def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpSeq[ B ] = ???

    override def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpSeq[ B ] = ???

    override def sortBy[ B >: T, C ]( fn : B => C )( implicit ord : Ordering[ C ] ) : FpSeq[ B ] = ???

    override def sortWith[ B >: T ]( cmp : (B, B) => Int ) : FpSeq[ B ] = ???

    override def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpLazyList[ (B, C) ] = this match {
        case FpLazyNil => FpLazyList.makeLazy( FpLazyNil )
        case FpUnevaluatedLazyList( evalFn ) => FpLazyList.makeLazy {
            evalFn().zipWith( that )
        }
        case thisEval@FpLazyListEval( nextThis, vThis ) => that match {
            case FpLazyNil => FpLazyList.makeLazy( FpLazyNil )
            case FpUnevaluatedLazyList( evalFn ) => FpLazyList.makeLazy {
                thisEval.zipWith( evalFn() )
            }
            case FpLazyListEval( nextThat, vThat ) => FpLazyList.makeLazy {
                FpLazyListEval( nextThis.zipWith( nextThat ), (vThis, vThat) )
            }
            case _ => FpLazyList.makeLazy {
                thisEval.zipWith( FpLazyList.fromFpList( that.toFpList ) )
            }
        }
    }

    override def zipWithIndex[ B >: T ] : FpSeq[ (B, Int) ] = ???

    override def withLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    override def withRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    def reduce[ B >: T ]( fn : (B, B) => B ) : Option[ B ] = ???

    def foldLeft[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : A = ???

    def foldRight[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : A = reverse.foldLeft( aggregate )( fn )

    override def flatMap[ A, B ]( a : FpLazyList[ A ] )
                                ( fn : A => FpLazyList[ B ] ) : FpLazyList[ B ] = FpLazyList.flatMap( a )( fn )

    override def unit[ A ]( ele : A ) : FpLazyList[ A ] = FpLazyList.unit( ele )

    private def toStringInternal[ A ]( ll : FpLazyList[ A ] ) : String = StackSafe.selfCall[ FpLazyList[ A ], String ] {
        ( thisFn : FpLazyList[ A ] => StackSafe[String ] ) => {
            case FpLazyNil => Result( "FpLazyNil" )
            case FpUnevaluatedLazyList( _ ) => Result( "??" )
            case FpLazyListEval( tail, head ) => Call.from {
                thisFn( tail ).map( ( tailString : String ) => s"${head.toString} + ${tailString}" )
            }
        }
    }( ll )

    override def toString : String = s"[${toStringInternal( this )}]"

}

case class FpUnevaluatedLazyList[ +T ]( override val evaluate : ( ) => FpEvaluatedLazyList[ T ] ) extends FpLazyList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

trait FpEvaluatedLazyList[ +T ] extends FpLazyList[ T ] {
    private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ] = ( ) => this
}

case object FpLazyNil extends FpEvaluatedLazyList[ Nothing ]

case class FpLazyListEval[ +T ]( tail : FpLazyList[ T ], head : T ) extends FpEvaluatedLazyList[ T ] {
    override private[ collections ] val evaluate : ( ) => FpEvaluatedLazyList[ T ] = ( ) => this

    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toList == ll.toList
        case _ => false
    }
}

object FpLazyList extends MonadStatic[ FpLazyList ] {

    def apply[ A ]( ele : A ) : FpLazyList[ A ] = unit( ele )

    def makeLazy[ A ]( ll : => FpLazyList[ A ] ) : FpLazyList[ A ] = FpUnevaluatedLazyList( () => ll.evaluate() )

    def fromFpList[ A ]( list : FpList[ A ] ) : FpLazyList[ A ] = StackSafe.selfCall[ FpList[ A ], FpLazyList[ A ] ] {
        ( thisFn : FpList[ A ] => StackSafe[FpLazyList[ A ] ] ) => {
            case FpNil => Result( FpLazyNil )
            case FpList( tail, head ) => Call.from {
                thisFn( tail ).map( ( ll : FpLazyList[ A ] ) => FpLazyListEval( ll, head ) )
            }
        }
    }( list )

    def combine[ A ]( a : FpLazyList[ A ], b : A ) : FpLazyList[ A ] = FpLazyList.makeLazy( FpLazyListEval( a, b ) )

    def combine[ A ]( a : FpLazyList[ A ], b : FpLazyList[ A ] ) : FpLazyList[ A ] = a match {
        case FpLazyNil => FpLazyList.makeLazy( b )
        case FpUnevaluatedLazyList( evalFn ) => b match {
            case FpLazyNil => FpLazyList.makeLazy( a )
            case _ => FpLazyList.makeLazy( combine( evalFn(), b ).evaluate() )
        }
        case FpLazyListEval( tail, head ) => FpLazyList.makeLazy {
            FpLazyListEval( combine( tail, b ), head )
        }
    }



    override def flatMap[ A, B ]( a : FpLazyList[ A ] )
                                ( fn : A => FpLazyList[ B ] ) : FpLazyList[ B ] = a match {
        case FpLazyNil => FpLazyList.makeLazy( FpLazyNil )
        case FpUnevaluatedLazyList( evalFn ) => FpUnevaluatedLazyList( ( ) => {
            flatMap( evalFn() )( fn ).evaluate()
        } )
        case FpLazyListEval( tail : FpLazyList[ A ], head ) => FpLazyList.makeLazy {
           fn( head ) ++ flatMap( tail )( fn )
        }
    }

    override def unit[ A ]( ele : A ) : FpLazyList[ A ] = FpUnevaluatedLazyList( () => FpLazyListEval( FpLazyNil, ele ) )
}
