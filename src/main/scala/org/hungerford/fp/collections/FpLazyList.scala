package org.hungerford.fp.collections

import org.hungerford.fp.basic.{FpNone, FpOption, FpSome}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadStatic, TypedMonoid, TypedMonoidStatic}

sealed trait FpLazyList[ +T ] extends FpSeq[ T ] with Monad[ FpLazyList, T ] with TypedMonoid[ FpLazyList, T ] {
    override val static : MonadStatic[ FpLazyList ] with TypedMonoidStatic[ FpLazyList ] = FpLazyList

    private[ collections ] val ss : StackSafe[ FpEvaluatedLazyList[ T ] ]

    def makeLazy : FpLazyList[ T ] = FpLazyList.makeLazy( this )

    def evaluate[ B >: T ]( num : Int ) : FpLazyList[ B ] = StackSafe.selfCall2[ Int, FpLazyList[ B ], FpLazyList[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
                case FpLazyNil => Result( FpLazyNil )
                case FpLazyListEval( v, next ) => Call.from {
                    thisFn(i - 1, next).map( ll => FpLazyListEval( v, ll ) )
                }
                case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                    thisFn( i, evalLazy.run() )
                }
            }
    }( num, this )

    def devaluate( num : Int ) : FpLazyList[ T ] = StackSafe.selfCall2[ Int, FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn =>
            ( i, ll ) => if ( i <= 0 ) Result( ll ) else ll match {
                    case FpLazyNil => Result( FpLazyList.makeLazy( FpLazyNil ) )
                    case FpUnevaluatedLazyList( nextEval ) => Result( FpUnevaluatedLazyList(
                        nextEval.flatMap( ( nextLl => thisFn( i - 1, nextLl ).flatMap( _.ss ) ) )
                    ) )
                    case FpLazyListEval( head, tail ) => Result( FpUnevaluatedLazyList(
                        Call.from( thisFn( i - 1, tail ) ).map( ll => FpLazyListEval( head, ll ) )
                    ) )
                }
    }( num, this )

    def evaluateAll() : FpLazyList[ T ] =  StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case FpLazyListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => FpLazyListEval( v, ll ) )
            }
            case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                thisFn( evalLazy.run() )
            }
        }
    }( this )

    def devaluateAll() : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyList.makeLazy( FpLazyNil ) )
            case uneval@FpUnevaluatedLazyList( _ ) => Result( uneval )
            case FpLazyListEval( head, tail ) => Result( FpUnevaluatedLazyList(
                Call.from( thisFn( tail ) ).map( ll => FpLazyListEval( head, ll ) )
                ) )
        }
    }( this )

    override def apply[ B >: T ]( index : Int ) : FpOption[ B ] = StackSafe.selfCall2 [ Int, FpLazyList[ B ], FpOption[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 0 ) Result( FpNone ) else ll match {
                case FpLazyNil => Result( FpNone )
                case FpLazyListEval( v, next ) =>
                    if ( i == 0 ) Result( FpSome( v ) ) else Call.from {
                        thisFn( i - 1, next )
                    }
                case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                    thisFn( i, evalLazy.run() )
                }
            }
    }( index, this )

    def toFpList : FpList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpNil )
            case FpLazyListEval( head, next ) => Call.from {
                thisFn( next ).map( l => head +: l )
            }
            case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                thisFn( evalLazy.run() )
            }
        }
    }( this )

    def :+[ B >: T ]( that : B ) : FpLazyList[ B ] = FpLazyList.combine( this, FpLazyList.unit( that ) )
    def +:[ B >: T ]( that : B ) : FpLazyList[ B ] = FpLazyList.combine( this, that )

    def ::+[ B >: T ]( that : B ) : FpLazyList[ B ] = {
        FpLazyList.makeLazy( FpLazyList.combine( this, FpLazyList.makeLazy( FpLazyList.unit( that ) ) ) )
    }
    def +::[ B >: T ]( that : B ) : FpLazyList[ B ] = FpLazyList.makeLazy( FpLazyList.combine( FpLazyList.makeLazy( this ), that ) )

    override def :++[ B >: T ]( that : FpSeq[ B ] ) : FpLazyList[ B ] = that match {
        case ll : FpLazyList[ B ] => FpLazyList.combine( this, ll )
        case _ => FpLazyList.combine( this, FpLazyList.fromFpList( that.toFpList ) )
    }

    def ::++[ B >: T ]( that : FpSeq[ B ] ) : FpLazyList[ B ] = that match {
        case ll : FpLazyList[ B ] => FpLazyList.combine( this, FpLazyList.makeLazy( ll ) )
        case _ => FpLazyList.combine( this, FpLazyList.makeLazy( FpLazyList.fromFpList( that.toFpList ) ) )
    }

    override def ++:[ B >: T ]( that : FpSeq[ B ] ) : FpLazyList[ B ] = that match {
        case ll : FpLazyList[ B ] => FpLazyList.combine( ll, this )
        case _ => FpLazyList.combine( FpLazyList.fromFpList( that.toFpList ), this )
    }

    def ++::[ B >: T ]( that : FpSeq[ B ] ) : FpLazyList[ B ] = that match {
        case ll : FpLazyList[ B ] => FpLazyList.combine( FpLazyList.makeLazy( ll ), this )
        case _ => FpLazyList.combine( FpLazyList.makeLazy( FpLazyList.fromFpList( that.toFpList ) ), this )
    }

    override def headOption : FpOption[ T ] = this match {
        case FpUnevaluatedLazyList( evalLazy ) => evalLazy.run().headOption
        case FpLazyNil => FpNone
        case FpLazyListEval( head, _ ) => FpSome( head )
    }

    override def tailOption : FpOption[ FpLazyList[ T ] ] = this match {
        case FpUnevaluatedLazyList( evalLazy ) => evalLazy.run().tailOption
        case FpLazyNil => FpNone
        case FpLazyListEval( _, tail ) => FpSome( tail )
    }

    override def tailOrNil : FpSeq[ T ] = tailOption.getOrElse( FpLazyNil )

    override def lastOption : FpOption[ T ] = StackSafe.selfCall2[ FpOption[ T ], FpLazyList[ T ], FpOption[ T ] ] {
        thisFn =>
            ( init, ll ) => ll match {
                case FpLazyNil => Result( init )
                case FpLazyListEval( v, tail ) => Call.from( thisFn( FpSome( v ), tail ) )
                case FpUnevaluatedLazyList( ss ) => Call.from( ss.flatMap( ll => thisFn( init, ll ) ) )
            }
    }( FpNone, this )

    override def reverse : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        ( thisFn : FpLazyList[ T ] => StackSafe[FpLazyList[ T ] ] ) => {
            case FpLazyNil => Result( FpLazyNil )
            case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( v ) )
            case FpLazyListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => ll :++ FpLazyListEval( v, FpLazyNil ) )
            }
        }
    }( this )

    def lengthOpt : FpOption[ Int ] = StackSafe.selfCall2[ Int, FpLazyList[ T ], FpOption[ Int ] ] {
        thisFn =>
            ( i : Int, ll : FpLazyList[ T ] ) => ll match {
                case FpLazyNil => Result( FpSome( i ) )
                case FpLazyListEval( _, next ) => Call.from {
                    thisFn( i + 1, next )
                }
                case FpUnevaluatedLazyList( _ ) => Result( FpNone )
            }
    }( 0, this )

    def length : Int = StackSafe.selfCall2[ Int, FpLazyList[ T ], Int ] {
        thisFn =>
            ( i : Int, ll : FpLazyList[ T ] ) => ll match {
                case FpLazyNil => Result( i )
                case FpLazyListEval( _, next ) => Call.from {
                    thisFn( i + 1, next )
                }
                case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                    thisFn( i, evalLazy.run() )
                }
            }
    }( 0, this )

    override def times( num : Int ) : FpLazyList[ T ] = StackSafe.selfCall[ Int, FpLazyList[ T ] ] {
        thisFn =>
            i =>
                if ( this == FpLazyNil ) Result( FpLazyNil )
                else if ( i < 1 ) Result( FpLazyNil )
                else if ( num == 1 ) Result( this )
                else Call.from {
                    thisFn( i - 1 ).map( ll => FpLazyList.makeLazy( this :++ ll ) )
                }
    }( num )

    override def filter( fn : T => Boolean ) : FpLazyList[ T ] = this.flatMap( t => {
        if ( fn( t  ) ) static.unit( t )
        else FpLazyNil
    } )

    override def take( num : Int ) : FpLazyList[ T ] = StackSafe.selfCall2[ Int, FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn =>
            (i, ll) =>
                if ( i < 1 ) Result( FpLazyNil ) else ll match {
                    case FpLazyNil => Result( FpLazyNil )
                    case FpLazyListEval( head, tail ) =>  Call.from {
                        thisFn( i - 1, tail ).map( nextll => FpLazyListEval( head, nextll ) )
                    }
                    case FpUnevaluatedLazyList( evalLazy ) => Result( FpUnevaluatedLazyList(
                        evalLazy.flatMap( v => thisFn( i, v ).flatMap( x => x.ss ) )
                    ) )
                }
    }( num, this )


    override def takeWhile( fn : T => Boolean ) : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case FpLazyListEval( head, tail ) => if ( !fn( head ) ) Result( FpLazyNil ) else Call.from {
                thisFn( tail ).map( ll => FpLazyListEval( head, ll ) )
            }
            case FpUnevaluatedLazyList( evalLazy ) => Result( FpUnevaluatedLazyList(
                evalLazy.flatMap( v => thisFn( v ).flatMap( x => x.ss ) )
            ) )
        }
    }( this )


    override def drop( num : Int ) : FpLazyList[ T ] = StackSafe.selfCall2[ Int, FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn =>
            (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
                case FpLazyNil => Result( FpLazyNil )
                case FpLazyListEval( v, next ) => Call.from {
                    thisFn( i - 1, next )
                }
                case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( i , v ) )
            }
    }( num, this )


    override def dropWhile( fn : T => Boolean ) : FpLazyList[ T ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ T ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case ll@FpLazyListEval( head, tail ) => if ( !fn( head ) ) Result( ll ) else Call.from {
                thisFn( tail )
            }
            case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( v ) )
        }
    }( this )

    override def slice( start : Int, end : Int ) : FpLazyList[ T ] = drop( start ).take( end - start )

    override def exists( fn : T => Boolean ) : Boolean = StackSafe.selfCall[ FpLazyList[ T ], Boolean ] {
        thisFn => {
            case FpLazyNil => Result( false )
            case FpLazyListEval( v, next ) => if ( fn( v ) ) Result( true ) else Call.from {
                thisFn( next )
            }
            case FpUnevaluatedLazyList( evalLazy ) => Call.from {
                thisFn( evalLazy.run() )
            }
        }
    }( this )

    override def distinct : FpLazyList[ T ] = StackSafe.selfCall2[ FpLazyList[ T ], Set[ T ], FpLazyList[ T ] ] {
        thisFn =>
            ( ll, set ) =>
                ll match {
                    case FpLazyNil => Result( FpLazyNil )
                    case FpLazyListEval( v, next ) if set.contains( v ) => Call.from {
                        thisFn( next, set )
                    }
                    case FpLazyListEval( v, next ) => Call.from {
                        thisFn( next, set + v ).map( v +: _ )
                    }
                    case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( v, set )  )
                }
    }( this, Set.empty[ T ] )

    override def partition( fn : T => Boolean ) : (FpLazyList[ T ], FpLazyList[ T ]) = {
        (this.filter( fn ), this.filter( t => !fn( t ) ))
    }

    override def collect[ B ]( fn : PartialFunction[ T, B ] ) : FpLazyList[ B ] = StackSafe.selfCall[ FpLazyList[ T ], FpLazyList[ B ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case FpLazyListEval( v, next ) => if ( fn.isDefinedAt( v ) ) {
                Call.from {
                    thisFn( next ).map( ll => FpLazyListEval( fn( v ), ll ) )
                }
            } else Call.from( thisFn( next ) )
            case FpUnevaluatedLazyList( evalSs ) => Result( FpUnevaluatedLazyList(
                Call.from( evalSs.flatMap( ll => thisFn( ll ).flatMap( _.ss ) ) )
            ) )
        }
    }( this )

    override def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpLazyList[ B ] = {
        FpLazyList.makeLazy( FpLazyList.fromFpList( toFpList.asInstanceOf[ FpList[ B ] ].sort ) )
    }

    override def sortBy[ C ]( fn : T => C )( implicit ord : Ordering[ C ] ) : FpLazyList[ T ] = sort( Ordering.by( fn ) )

    override def sortWith[ B >: T ]( cmp : (B, B) => Int ) : FpLazyList[ B ] = sort( new Ordering[ B ] {
        override def compare( x : B, y : B ) : Int = cmp( x, y )
    } )

    override def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpLazyList[ (B, C) ] = StackSafe.selfCall2[ FpLazyList[ B ], FpSeq[ C ], FpLazyList[ (B, C) ] ] {
        thisFn =>
            (l1, l2) =>
                l1 match {
                    case FpLazyNil => Result( FpLazyList.makeLazy( FpLazyNil ) )
                    case FpUnevaluatedLazyList( evalLazy ) => Result( FpUnevaluatedLazyList( evalLazy.flatMap( v => thisFn( v, l2 ).flatMap( _.ss ) ) ) )
                    case thisEval@FpLazyListEval( vL1, nextL1 ) => l2 match {
                        case FpLazyNil => Result( FpLazyList.makeLazy( FpLazyNil ) )
                        case FpUnevaluatedLazyList( evalLazy ) => Result( FpUnevaluatedLazyList( evalLazy.flatMap( v => thisFn( thisEval, v ).flatMap( _.ss ) ) ) )
                        case FpLazyListEval( vL2, nextL2 ) => Result( FpUnevaluatedLazyList {
                            Call.from( thisFn( nextL1, nextL2 ).map( ll => FpLazyListEval( (vL1, vL2), ll ) ) )
                        } )
                        case _ => Result( FpUnevaluatedLazyList {
                            Call.from( thisFn( thisEval, FpLazyList.fromFpList( l2.toFpList ) ).flatMap( _.ss ) )
                        } )
                    }
                }
    } ( this, that )

    override def zipWithIndex[ B >: T ] : FpLazyList[ (B, Int) ] = StackSafe.selfCall2[ Int, FpLazyList[ B ], FpLazyList[ (B, Int) ] ] {
        thisFn =>
            (i, ll) => ll match {
                case FpLazyNil => Result( FpLazyNil )
                case FpLazyListEval( v, next ) => Result( FpLazyList.makeLazy(
                    StackSafe( thisFn( i + 1, next ).map( nextL => FpLazyListEval( (v, i), nextL ) ) )
                    ) )
                case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( i, v ) )
            }
    }( 0, this )

    override def mapWithLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpLazyList[ B ] = {
        ( start +: this ).zipWith( this ).map( t => fn( t._1, t._2 ) )
    }

    override def mapWithRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpLazyList[ B ] = {
        this.zipWith( this.tailOption.getOrElse( FpLazyNil ) :+ end ).map( t => fn( t._1, t._2 ) )
    }

    def reduce[ B >: T ]( fn : (B, B) => B ) : () => FpOption[ B ] = StackSafe.selfCall2[ () => FpOption[ B ], FpOption[ FpLazyList[ B ] ], () => FpOption[ B ] ] {
        thisFn =>
            ( aggFn, ll ) => ll match {
                case FpNone => Result( () => FpNone )
                case FpSome( FpLazyNil ) => Result( aggFn )
                case FpSome( FpLazyListEval( v, next ) ) => Call.from {
                    thisFn( () => aggFn().map( (res : B) => fn( res, v ) ), FpSome( next ) )
                }
                case FpSome( FpUnevaluatedLazyList( evaLazy ) ) => evaLazy.flatMap( v => thisFn( aggFn, FpSome( v ) ) )
            }
    }( () => this.headOption, this.tailOption )

    def foldLeft[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : () => A = StackSafe.selfCall2[ () => A, FpLazyList[ B ], () => A ] {
        thisFn =>
            ( aggFn, ll ) => ll match {
                case FpLazyNil => Result( aggFn )
                case FpLazyListEval( v, next ) => Call.from {
                    thisFn( () => fn( aggFn(), v ), next )
                }
                case FpUnevaluatedLazyList( evalLazy ) => evalLazy.flatMap( v => thisFn( aggFn, v ) )
            }
    }( () => aggregate, this )

    def foldRight[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : () => A = reverse.foldLeft( aggregate )( fn )

    private def toStringInternal[ A ]( ll : FpLazyList[ A ] ) : String = StackSafe.selfCall2 [ String, FpLazyList[ A ], String ] {
        thisFn =>
            (str, ll) => if ( str.length > 100 ) Result( str + "..." ) else ll match {
            case FpLazyNil =>
                if ( str == "" ) Result( "FpLazyNil" )
                else Result( s"${str}, FpLazyNil" )
            case FpUnevaluatedLazyList( _ ) =>
                if ( str == "" ) Result( "?? (lazy)" )
                else Result( s"${str}, ?? (lazy)" )
            case FpLazyListEval( head, tail ) => Call.from {
                if ( str == "" ) thisFn( s"${head.toString}", tail )
                else thisFn( s"$str, ${head.toString}", tail )

            }
        }
    }( "", ll )

    override def toString : String = s"[${toStringInternal( this )}]"

}

case class FpUnevaluatedLazyList[ +T ]( override val ss : StackSafe[ FpEvaluatedLazyList[ T ] ] ) extends FpLazyList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toFpList == ll.toFpList
        case _ => false
    }
}

trait FpEvaluatedLazyList[ +T ] extends FpLazyList[ T ] {
    private[ collections ] val ss : StackSafe[ FpEvaluatedLazyList[ T ] ] = Result( this )
}

case object FpLazyNil extends FpEvaluatedLazyList[ Nothing ]

case class FpLazyListEval[ +T ]( head : T, tail : FpLazyList[ T ] ) extends FpEvaluatedLazyList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpLazyList[ _ ] => this.toFpList == ll.toFpList
        case _ => false
    }
}

object FpLazyList extends MonadStatic[ FpLazyList ] with TypedMonoidStatic[ FpLazyList ] {

    def apply[ A ]( ele : A ) : FpLazyList[ A ] = unit( ele )

    def makeLazy[ A ]( ll : => FpLazyList[ A ] ) : FpLazyList[ A ] = FpUnevaluatedLazyList( Call.from( Result( ll ) ).flatMap {
        case lz@FpUnevaluatedLazyList( next ) => next
        case ev@FpLazyListEval( _, _ ) => Result( ev )
        case FpLazyNil => Result( FpLazyNil )
    } )

    def fromFpList[ A ]( list : FpList[ A ] ) : FpLazyList[ A ] = StackSafe.selfCall[ FpList[ A ], FpLazyList[ A ] ] {
        ( thisFn : FpList[ A ] => StackSafe[FpLazyList[ A ] ] ) => {
            case FpNil => Result( FpLazyNil )
            case FpList( tail, head ) => Call.from {
                thisFn( tail ).map( ( ll : FpLazyList[ A ] ) => FpLazyListEval( head, ll ) )
            }
        }
    }( list )

    def fromLeft[ A ]( init : A )( fn : A => A ) : FpLazyList[ A ] = StackSafe.selfCall[ A, FpLazyList[ A ] ] {
        thisFn => a => Result( FpUnevaluatedLazyList( Call.from( thisFn( fn( a ) ).map( ll => FpLazyListEval( a, ll ) ) ) ) )
    }( init )

    def combine[ A ]( a : FpLazyList[ A ], b : A ) : FpLazyList[ A ] = FpLazyListEval( b, a )

    def combine[ A ]( a : FpLazyList[ A ], b : FpLazyList[ A ] ) : FpLazyList[ A ] = StackSafe.selfCall2 [ FpLazyList[ A ], FpLazyList[ A ], FpLazyList[ A ] ] {
        thisFn =>
            (l1, l2) => l1 match {
                case FpLazyNil => Result( l2 )
                case FpUnevaluatedLazyList( evalLazy ) => l2 match {
                    case FpLazyNil => Result( FpLazyList.makeLazy( l1 ) )
                    case _ => Result( FpUnevaluatedLazyList( Call.from( evalLazy.flatMap( v => thisFn( v, l2 ).flatMap( _.ss ) ) ) ) )
                }
                case FpLazyListEval( head, tail ) =>
                    Call.from( thisFn( tail, l2 ).map( v => FpLazyListEval( head, v ) ) )
            }
    } ( a, b )

    override def flatMap[ A, B ]( a : FpLazyList[ A ] )
                                ( fn : A => FpLazyList[ B ] ) : FpLazyList[ B ] = StackSafe.selfCall[ FpLazyList[ A ], FpLazyList[ B ] ] {
        thisFn => {
            case FpLazyNil => Result( FpLazyNil )
            case FpUnevaluatedLazyList( evalLazy ) => Result( FpUnevaluatedLazyList( evalLazy.flatMap( v => thisFn( v ).flatMap( _.ss ) ) ) )
            case FpLazyListEval( head, tail : FpLazyList[ A ] ) => Call.from {
                thisFn( tail ).flatMap( v => (fn( head ) :++ v).ss )
            }
        }
    }( a )

    override def unit[ A ]( ele : A ) : FpLazyList[ A ] = ele +: FpLazyNil

    override def empty : FpLazyList[ Nothing ] = FpLazyNil
}
