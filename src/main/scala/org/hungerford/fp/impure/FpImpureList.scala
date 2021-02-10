package org.hungerford.fp.impure

import org.hungerford.fp.basic.{FpFailure, FpNone, FpOption, FpSome, FpSuccess}
import org.hungerford.fp.collections.{FpList, FpNil, FpSeq}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadStatic, TypedMonoid, TypedMonoidStatic}

sealed trait FpImpureList[ +T ] extends FpSeq[ T ] with Monad[ FpImpureList, T ] with TypedMonoid[ FpImpureList, T ] {
    override val static : MonadStatic[ FpImpureList ] with TypedMonoidStatic[ FpImpureList ] = FpImpureList

    private[ impure ] val impure : FpImpure[ FpEvaluatedImpureList[ T ] ]

    def run() : FpEvaluatedImpureList[ T ] = impure.run() match {
        case FpSuccess( res ) => res
        case FpFailure( t ) => FpImpureFail( t )
    }

    def makeImpure : FpImpureList[ T ] = FpImpureList.makeImpure( this )

    def evaluate[ B >: T ]( num : Int ) : FpImpureList[ B ] = StackSafe.selfCall2[ Int, FpImpureList[ B ], FpImpureList[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
                case FpImpureNil => Result( FpImpureNil )
                case fail@FpImpureFail( _ ) => Result( fail )
                case FpImpureListEval( v, next ) => Call.from {
                    thisFn(i - 1, next).map( ll => FpImpureListEval( v, ll ) )
                }
                case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                    thisFn( i, uneval.run() )
                }
            }
    }( num, this )

    def devaluate( num : Int ) : FpImpureList[ T ] = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn =>
            ( i, ll ) => if ( i <= 0 ) Result( ll ) else ll match {
                case FpImpureNil => Result( FpImpureList.makeImpure( FpImpureNil ) )
                case fail@FpImpureFail( _ ) => Result( FpImpureList.makeImpure( fail ) )
                case FpUnevaluatedImpureList( nextEval ) => Result( FpUnevaluatedImpureList(
                    nextEval.flatMap( nextIl => FpImpure.fromSs( Call.from( thisFn( i - 1, nextIl ).flatMap( _.impure.ss ) ) ) )
                ) )
                case FpImpureListEval( head, tail ) => Result( FpUnevaluatedImpureList(
                    FpImpure.fromSs( thisFn( i, tail ).flatMap( _.impure.ss ) ).map( ll => FpImpureListEval( head, FpImpureList.makeImpure( ll ) ) )
                ) )
            }
    }( num, this )

    def evaluateAll() : FpImpureList[ T ] =  StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case FpImpureListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => FpImpureListEval( v, ll ) )
            }
            case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                thisFn( uneval.run() )
            }
        }
    }( this )

    def devaluateAll() : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureList.makeImpure( FpImpureNil ) )
            case fail@FpImpureFail( _ ) => Result( FpImpureList.makeImpure( fail ) )
            case uneval@FpUnevaluatedImpureList( _ ) => Result( uneval )
            case FpImpureListEval( head, tail ) => Result( FpUnevaluatedImpureList(
                FpImpure.fromSs( thisFn( tail ).flatMap( _.impure.ss ) ).map( ll => FpImpureListEval( head, FpImpureList.makeImpure( ll ) ) )
            ) )
        }
    }( this )

    override def apply[ B >: T ]( index : Int ) : FpOption[ B ] = StackSafe.selfCall2 [ Int, FpImpureList[ B ], FpOption[ B ] ] {
        thisFn =>
            (i, ll) => if ( i < 0 ) Result( FpNone ) else ll match {
                case FpImpureNil => Result( FpNone )
                case FpImpureFail( _ ) => Result( FpNone )
                case FpImpureListEval( v, next ) =>
                    if ( i == 0 ) Result( FpSome( v ) ) else Call.from {
                        thisFn( i - 1, next )
                    }
                case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                    thisFn( i, uneval.run() )
                }
            }
    }( index, this )

    def toFpList : FpList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpNil )
            case FpImpureFail( _ ) => Result( FpNil )
            case FpImpureListEval( head, next ) => Call.from {
                thisFn( next ).map( l => head +: l )
            }
            case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                thisFn( uneval.run() )
            }
        }
    }( this )

    def :+[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, FpImpureList.unit( that ) )
    def +:[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.combine( this, that )

    def ::+[ B >: T ]( that : B ) : FpImpureList[ B ] = {
        FpImpureList.makeImpure( FpImpureList.combine( this, FpImpureList.makeImpure( FpImpureList.unit( that ) ) ) )
    }
    def +::[ B >: T ]( that : B ) : FpImpureList[ B ] = FpImpureList.makeImpure( FpImpureList.combine( FpImpureList.makeImpure( this ), that ) )

    override def :++[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = that match {
        case ll : FpImpureList[ B ] => FpImpureList.combine( this, ll )
        case _ => FpImpureList.combine( this, FpImpureList.fromFpList( that.toFpList ) )
    }

    def ::++[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = that match {
        case ll : FpImpureList[ B ] => FpImpureList.combine( this, FpImpureList.makeImpure( ll ) )
        case _ => FpImpureList.combine( this, FpImpureList.makeImpure( FpImpureList.fromFpList( that.toFpList ) ) )
    }

    override def ++:[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = that match {
        case ll : FpImpureList[ B ] => FpImpureList.combine( ll, this )
        case _ => FpImpureList.combine( FpImpureList.fromFpList( that.toFpList ), this )
    }

    def ++::[ B >: T ]( that : FpSeq[ B ] ) : FpImpureList[ B ] = that match {
        case ll : FpImpureList[ B ] => FpImpureList.combine( FpImpureList.makeImpure( ll ), this )
        case _ => FpImpureList.combine( FpImpureList.makeImpure( FpImpureList.fromFpList( that.toFpList ) ), this )
    }

    override def headOption : FpOption[ T ] = this match {
        case uneval@FpUnevaluatedImpureList( _ ) => uneval.run().headOption
        case FpImpureNil => FpNone
        case FpImpureFail( _ ) => FpNone
        case FpImpureListEval( head, _ ) => FpSome( head )
    }

    override def tailOption : FpOption[ FpImpureList[ T ] ] = this match {
        case uneval@FpUnevaluatedImpureList( _ ) => uneval.run().tailOption
        case FpImpureNil => FpNone
        case FpImpureFail( _ ) => FpNone
        case FpImpureListEval( _, tail ) => FpSome( tail )
    }

    override def tailOrNil : FpSeq[ T ] = tailOption.getOrElse( FpImpureNil )

    override def lastOption : FpOption[ T ] = StackSafe.selfCall2[ FpOption[ T ], FpImpureList[ T ], FpOption[ T ] ] {
        thisFn =>
            ( init, ll ) => ll match {
                case FpImpureNil => Result( init )
                case FpImpureFail( _ ) => Result( init )
                case FpImpureListEval( v, tail ) => Call.from( thisFn( FpSome( v ), tail ) )
                case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                    thisFn( init, uneval.run() )
                }
            }
    }( FpNone, this )

    override def reverse : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        ( thisFn : FpImpureList[ T ] => StackSafe[FpImpureList[ T ] ] ) => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList {
                evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v ).flatMap( _.impure.ss ) ) )
            } )
            case FpImpureListEval( v, next ) => Call.from {
                thisFn( next ).map( ll => ll :++ FpImpureListEval( v, FpImpureNil ) )
            }
        }
    }( this )

    def lengthOpt : FpOption[ Int ] = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpOption[ Int ] ] {
        thisFn =>
            ( i : Int, ll : FpImpureList[ T ] ) => ll match {
                case FpImpureNil => Result( FpSome( i ) )
                case FpImpureFail( _ ) => Result( FpSome( i ) )
                case FpImpureListEval( _, next ) => Call.from {
                    thisFn( i + 1, next )
                }
                case FpUnevaluatedImpureList( _ ) => Result( FpNone )
            }
    }( 0, this )

    def length : Int = StackSafe.selfCall2[ Int, FpImpureList[ T ], Int ] {
        thisFn =>
            ( i : Int, ll : FpImpureList[ T ] ) => ll match {
                case FpImpureNil => Result( i )
                case FpImpureFail( _ ) => Result( i )
                case FpImpureListEval( _, next ) => Call.from {
                    thisFn( i + 1, next )
                }
                case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                    thisFn( i, uneval.run() )
                }
            }
    }( 0, this )

    override def times( num : Int ) : FpImpureList[ T ] = StackSafe.selfCall[ Int, FpImpureList[ T ] ] {
        thisFn =>
            i =>
                if ( this == FpImpureNil ) Result( FpImpureNil )
                else if ( i < 1 ) Result( FpImpureNil )
                else if ( num == 1 ) Result( this )
                else Call.from {
                    thisFn( i - 1 ).map( ll => FpImpureList.makeImpure( this :++ ll ) )
                }
    }( num )

    override def filter( fn : T => Boolean ) : FpImpureList[ T ] = this.flatMap( t => {
        if ( fn( t  ) ) static.unit( t )
        else FpImpureNil
    } )

    override def take( num : Int ) : FpImpureList[ T ] = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn =>
            (i, ll) =>
                if ( i < 1 ) Result( FpImpureNil ) else ll match {
                    case FpImpureNil => Result( FpImpureNil )
                    case fail@FpImpureFail( _ ) => Result( fail )
                    case FpImpureListEval( head, tail ) =>  Call.from {
                        thisFn( i - 1, tail ).map( nextll => FpImpureListEval( head, nextll ) )
                    }
                    case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                        evalImpure.flatMap( v => FpImpure.fromSs( thisFn( i, v ).flatMap( _.impure.ss ) ) )
                    ) )
                }
    }( num, this )


    override def takeWhile( fn : T => Boolean ) : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case FpImpureListEval( head, tail ) => if ( !fn( head ) ) Result( FpImpureNil ) else Call.from {
                thisFn( tail ).map( ll => FpImpureListEval( head, ll ) )
            }
            case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v ).flatMap( _.impure.ss ) ) )
            ) )
        }
    }( this )


    override def drop( num : Int ) : FpImpureList[ T ] = StackSafe.selfCall2[ Int, FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn =>
            (i, ll) => if ( i < 1 ) Result( ll ) else ll match {
                case FpImpureNil => Result( FpImpureNil )
                case fail@FpImpureFail( _ ) => Result( fail )
                case FpImpureListEval( v, next ) => Call.from {
                    thisFn( i - 1, next )
                }
                case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                    evalImpure.flatMap( v => FpImpure.fromSs( thisFn( i , v ).flatMap( _.impure.ss ) ) )
                ) )
            }
    }( num, this )

    override def dropWhile( fn : T => Boolean ) : FpImpureList[ T ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ T ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case ll@FpImpureListEval( head, tail ) => if ( !fn( head ) ) Result( ll ) else Call.from {
                thisFn( tail )
            }
            case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v ).flatMap( _.impure.ss ) ) )
            ) )
        }
    }( this )

    override def slice( start : Int, end : Int ) : FpImpureList[ T ] = drop( start ).take( end - start )

    override def exists( fn : T => Boolean ) : Boolean = StackSafe.selfCall[ FpImpureList[ T ], Boolean ] {
        thisFn => {
            case FpImpureNil => Result( false )
            case FpImpureFail( _ ) => Result( false )
            case FpImpureListEval( v, next ) => if ( fn( v ) ) Result( true ) else Call.from {
                thisFn( next )
            }
            case uneval@FpUnevaluatedImpureList( _ ) => Call.from {
                thisFn( uneval.run() )
            }
        }
    }( this )

    override def distinct : FpImpureList[ T ] = StackSafe.selfCall2[ FpImpureList[ T ], Set[ T ], FpImpureList[ T ] ] {
        thisFn =>
            ( ll, set ) =>
                ll match {
                    case FpImpureNil => Result( FpImpureNil )
                    case fail@FpImpureFail( _ ) => Result( fail )
                    case FpImpureListEval( v, next ) if set.contains( v ) => Call.from {
                        thisFn( next, set )
                    }
                    case FpImpureListEval( v, next ) => Call.from {
                        thisFn( next, set + v ).map( v +: _ )
                    }
                    case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                        evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v, set ).flatMap( _.impure.ss )  ) )
                    ) )
                }
    }( this, Set.empty[ T ] )

    override def partition( fn : T => Boolean ) : (FpImpureList[ T ], FpImpureList[ T ]) = {
        (this.filter( fn ), this.filter( t => !fn( t ) ))
    }

    override def collect[ B ]( fn : PartialFunction[ T, B ] ) : FpImpureList[ B ] = StackSafe.selfCall[ FpImpureList[ T ], FpImpureList[ B ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case FpImpureListEval( v, next ) if fn.isDefinedAt( v ) => Call.from {
                thisFn( next ).map( ll => FpImpureListEval( fn( v ), ll ) )
            }
            case FpImpureListEval( v, next ) => Call.from( thisFn( next ) )
            case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v ).flatMap( _.impure.ss ) ) )
            ) )
        }
    } ( this )

    override def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpImpureList[ B ] = {
        FpImpureList.makeImpure( FpImpureList.fromFpList( toFpList.asInstanceOf[ FpList[ B ] ].sort ) )
    }

    override def sortBy[ C ]( fn : T => C )( implicit ord : Ordering[ C ] ) : FpImpureList[ T ] = sort( Ordering.by( fn ) )

    override def sortWith[ B >: T ]( cmp : (B, B) => Int ) : FpImpureList[ B ] = sort( new Ordering[ B ] {
        override def compare( x : B, y : B ) : Int = cmp( x, y )
    } )

    override def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpImpureList[ (B, C) ] = StackSafe.selfCall2[ FpImpureList[ B ], FpSeq[ C ], FpImpureList[ (B, C) ] ] {
        thisFn =>
            (l1, l2) =>
                l1 match {
                    case FpImpureNil => Result( FpImpureNil )
                    case fail@FpImpureFail( _ ) => Result( fail )
                    case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                        evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v, l2 ).flatMap( _.impure.ss ) ) )
                    ) )
                    case thisEval@FpImpureListEval( vL1, nextL1 ) => l2 match {
                        case FpImpureNil => Result( FpImpureNil )
                        case fail@FpImpureFail( _ ) => Result( fail )
                        case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                            evalImpure.flatMap( v => FpImpure.fromSs( thisFn( thisEval, v ).flatMap( _.impure.ss ) ) )
                        ) )
                        case FpImpureListEval( vL2, nextL2 ) => Call.from {
                            thisFn( nextL1, nextL2 ).map( ll => FpImpureListEval( (vL1, vL2), ll ) )
                        }
                        case _ => Result( FpUnevaluatedImpureList {
                            FpImpure.fromSs( thisFn( thisEval, FpImpureList.fromFpList( l2.toFpList ) ).flatMap( _.impure.ss ) )
                        } )
                    }
                }
    } ( this, that )

    override def zipWithIndex[ B >: T ] : FpImpureList[ (B, Int) ] = StackSafe.selfCall2[ Int, FpImpureList[ B ], FpImpureList[ (B, Int) ] ] {
        thisFn =>
            (i, ll) => ll match {
                case FpImpureNil => Result( FpImpureNil )
                case fail@FpImpureFail( _ ) => Result( fail )
                case FpImpureListEval( v, next ) => Call.from {
                    thisFn( i + 1, next ).map( il => FpImpureListEval( (v, i), il ) )
                }
                case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                    evalImpure.flatMap( v => FpImpure.fromSs( thisFn( i, v ).flatMap( _.impure.ss ) ) )
                ) )
            }
    }( 0, this )

    override def mapWithLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpImpureList[ B ] = {
        ( start +: this ).zipWith( this ).map( t => fn( t._1, t._2 ) )
    }

    override def mapWithRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpImpureList[ B ] = {
        this.zipWith( this.tailOption.getOrElse( FpImpureNil ) :+ end ).map( tup => fn( tup._1, tup._2 ) )
    }

    def reduce[ B >: T ]( fn : (B, B) => B ) : () => FpOption[ B ] = StackSafe.selfCall2[ () => FpOption[ B ], FpOption[ FpImpureList[ B ] ], () => FpOption[ B ] ] {
        thisFn =>
            ( aggFn, ll ) => ll match {
                case FpNone => Result( () => FpNone )
                case FpSome( FpImpureNil ) => Result( aggFn )
                case FpSome( FpImpureFail( _ ) ) => Result( aggFn )
                case FpSome( FpImpureListEval( v, next ) ) => Call.from {
                    thisFn( () => aggFn().map( (res : B) => fn( res, v ) ), FpSome( next ) )
                }
                case FpSome( FpUnevaluatedImpureList( evaImpure ) ) => Call.from {
                    evaImpure.ss.flatMap {
                        case FpSuccess( v ) => thisFn( aggFn, FpSome( v ) )
                        case FpFailure( _ ) => Result( aggFn )
                    }
                }
            }
    }( () => this.headOption, this.tailOption )

    def foldLeft[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : FpImpure[ A ] = StackSafe.selfCall2[ FpImpure[ A ], FpImpureList[ B ], FpImpure[ A ] ] {
        thisFn =>
            ( aggImp, ll ) => ll match {
                case FpImpureNil => Result( aggImp )
                case FpImpureFail( _ ) => Result( aggImp )
                case FpImpureListEval( v, next ) => Call.from {
                    thisFn( aggImp.map( ( a : A ) => fn( a, v ) ), next )
                }
                case FpUnevaluatedImpureList( evalImpure ) => Call.from {
                    evalImpure.ss.flatMap {
                        case FpSuccess( v ) => thisFn( aggImp, v )
                        case FpFailure( _ ) => Result( aggImp )
                    }
                }
            }
    }( FpImpure( aggregate ), this )

    def foldRight[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : FpImpure[ A ] = reverse.foldLeft( aggregate )( fn )

    private def toStringInternal[ A ]( ll : FpImpureList[ A ] ) : String = StackSafe.selfCall2 [ String, FpImpureList[ A ], String ] {
        thisFn =>
            (str, ll) => if ( str.length > 100 ) Result( str + "..." ) else ll match {
                case FpImpureNil =>
                    if ( str == "" ) Result( "FpImpureNil" )
                    else Result( s"$str, FpImpureNil" )
                case FpImpureFail( t ) =>
                    if ( str == "" ) Result( s"FpImpureFail(${t.toString})")
                    else Result( s"${str}, FpImpureFail(${t.toString})")
                case FpUnevaluatedImpureList( _ ) =>
                    if ( str == "" ) Result( "?? (impure)" )
                    else Result( s"${str}, ?? (impure)" )
                case FpImpureListEval( head, tail ) => Call.from {
                    if ( str == "" ) thisFn( s"${head.toString}", tail )
                    else thisFn( s"$str, ${head.toString}", tail )
                }
            }
    }( "", ll )

    override def toString : String = s"[${toStringInternal( this )}]"
}

case class FpUnevaluatedImpureList[ +T ]( override val impure : FpImpure[ FpEvaluatedImpureList[ T ] ] ) extends FpImpureList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toFpList == ll.toFpList
        case _ => false
    }
}

trait FpEvaluatedImpureList[ +T ] extends FpImpureList[ T ] {
    private[ impure ] val impure : FpImpure[ FpEvaluatedImpureList[ T ] ] = FpImpure.fromSs( Result( FpSuccess( this ) ) )
}

trait FpImpureEnd extends FpEvaluatedImpureList[ Nothing ]

case class FpImpureFail( throwable : Throwable ) extends FpImpureEnd

case object FpImpureNil extends FpImpureEnd

case class FpImpureListEval[ +T ]( head : T, tail : FpImpureList[ T ] ) extends FpEvaluatedImpureList[ T ] {
    override def equals( obj : Any ) : Boolean = obj match {
        case ll : FpImpureList[ _ ] => this.toFpList == ll.toFpList
        case _ => false
    }
}

object FpImpureList extends MonadStatic[ FpImpureList ] with TypedMonoidStatic[ FpImpureList ] {

    def apply[ A ]( ele : A ) : FpImpureList[ A ] = unit( ele )

    def fromImpure[ A ]( imp : FpImpure[ FpEvaluatedImpureList[ A ] ] ) : FpImpureList[ A ] = {
        FpUnevaluatedImpureList( imp )
    }

    def makeImpure[ A ]( ll : => FpImpureList[ A ] ) : FpImpureList[ A ] = fromImpure( FpImpure.fromSs( Call.from( ll.impure.ss ) ) )

    def fromFpList[ A ]( list : FpList[ A ] ) : FpImpureList[ A ] = StackSafe.selfCall[ FpList[ A ], FpImpureList[ A ] ] {
        ( thisFn : FpList[ A ] => StackSafe[FpImpureList[ A ] ] ) => {
            case FpNil => Result( FpImpureNil )
            case FpList( tail, head ) => Call.from {
                thisFn( tail ).map( ( ll : FpImpureList[ A ] ) => FpImpureListEval( head, ll ) )
            }
        }
    }( list )

    def fromLeft[ A ]( init : A )( fn : A => A ) : FpImpureList[ A ] = StackSafe.selfCall[ A, FpImpureList[ A ] ] {
        thisFn => a => Result( FpUnevaluatedImpureList(
            FpImpure.fromSs( Call.from( thisFn( fn( a ) ).map( ll => FpSuccess( FpImpureListEval( a, ll ) ) ) ) )
        ) )
    }( init )

    def combine[ A ]( a : FpImpureList[ A ], b : A ) : FpImpureList[ A ] = FpImpureListEval( b, a )

    def combine[ A ]( a : FpImpureList[ A ], b : FpImpureList[ A ] ) : FpImpureList[ A ] = StackSafe.selfCall2 [ FpImpureList[ A ], FpImpureList[ A ], FpImpureList[ A ] ] {
        thisFn =>
            (l1, l2) => l1 match {
                case FpImpureNil => Result( l2 )
                case FpImpureFail( _ ) => Result( l2 )
                case FpUnevaluatedImpureList( evalImpure ) => l2 match {
                    case FpImpureNil => Result( FpImpureList.makeImpure( l1 ) )
                    case FpImpureFail( _ ) => Result( FpImpureList.makeImpure( l1 ) )
                    case _ => Result( FpUnevaluatedImpureList(
                        evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v, l2 ).flatMap( _.impure.ss ) ) )
                    ) )
                }
                case FpImpureListEval( head, tail ) =>
                    Call.from( thisFn( tail, l2 ).map( v => FpImpureListEval( head, v ) ) )
            }
    } ( a, b )

    override def flatMap[ A, B ]( a : FpImpureList[ A ] )
                                ( fn : A => FpImpureList[ B ] ) : FpImpureList[ B ] = StackSafe.selfCall[ FpImpureList[ A ], FpImpureList[ B ] ] {
        thisFn => {
            case FpImpureNil => Result( FpImpureNil )
            case fail@FpImpureFail( _ ) => Result( fail )
            case FpUnevaluatedImpureList( evalImpure ) => Result( FpUnevaluatedImpureList(
                evalImpure.flatMap( v => FpImpure.fromSs( thisFn( v ).flatMap( _.impure.ss ) ) )
            ) )
            case FpImpureListEval( head, tail : FpImpureList[ A ] ) => Call.from {
                thisFn( tail ).map( il => fn( head ) :++ il )
            }
        }
    }( a )

    override def unit[ A ]( ele : A ) : FpImpureList[ A ] = ele +: FpImpureNil

    override def empty : FpImpureList[ _ ] = FpImpureNil
}
