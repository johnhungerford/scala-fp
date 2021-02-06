package org.hungerford.fp.collections

import org.hungerford.fp.basic.{FpNone, FpOption, FpSome}
import org.hungerford.fp.recursion.{Call, Result, StackSafe}
import org.hungerford.fp.types.{Monad, MonadStatic, MonoidCovariant, MonoidCovariantStatic, WithTransformer}

import scala.annotation.tailrec

sealed trait FpList[ +T ] extends FpSeq[ T ] with Monad[ FpList, T ] with MonoidCovariant[ FpList, T ] {
    override val static : MonadStatic[ FpList ] with MonoidCovariantStatic[ FpList ] = FpList

    override def apply[ B >: T ]( index : Int ) : FpOption[ B ] = StackSafe.selfCall2 [Int, FpList[ B ], FpOption[ B ] ] {
        thisFn =>
            (i, l) => if ( i < 0 ) Result( FpNone ) else l match {
                case FpNil => Result( FpNone )
                case FpLs( v, next ) => if ( i == 0 ) Result( FpSome( v ) ) else Call.from {
                    thisFn( i - 1, next )
                }
            }
    }( index, this )

    override def toFpList : FpList[ T ] = this

    override def headOption : FpOption[ T ] = FpList.headOption( this )

    override def tailOption : FpOption[ FpList[ T ] ] = FpList.tailOption( this )

    override def +:[ B >: T ]( ele : B ) : FpList[ B ] = FpList.prepend[ T, B ]( this, ele )
    override def :+[ B >: T ]( ele : B ) : FpList[ B ] = this :++ static.unit( ele )

    override def ++:[ B >: T ]( fpSeq : FpSeq[ B ] ) : FpList[ B ] = FpList.concat[ T, B ]( this, fpSeq.toFpList )
    override def :++[ B >: T ]( fpSeq : FpSeq[ B ] ) : FpList[ B ] = FpList.concat[ T, B ]( fpSeq.toFpList, this )

    override def fpString( sep : FpString ) : FpString = FpList.fpString( this, sep )

    override def reverse : FpList[ T ] = FpList.reverse( this )

    override def lengthOpt : FpOption[ Int ] = FpList.length( this )

    def length : Int = lengthOpt.getOrElse( 0 )

    override def toString : String = FpList.toString( this )

    override def times( num : Int ) : FpList[ T ] = FpList.times( this, num )

    override def filter( fn : T => Boolean ) : FpList[ T ] = FpList.filter( this )( fn )

    override def take( num : Int ) : FpList[ T ] = FpList.take( this )( num )

    override def takeWhile( fn : T => Boolean ) : FpList[ T ] = FpList.takeWhile( this )( fn )

    override def drop( num : Int ) : FpList[ T ] = ???

    override def dropWhile( fn : T => Boolean ) : FpList[ T ] = FpList.dropWhile( this )( fn )

    override def slice( start : Int, end : Int ) : FpList[ T ] = drop( start ).take( end - start )

    override def toList : List[ T ] = FpList.toList( this )

    override def exists( fn : T => Boolean ) : Boolean = FpList.exists( this )( fn )

    override def contains[ B >: T ]( ele : B ) : Boolean = FpList.contains[ T, B ]( this )( ele )

    override def distinct : FpList[ T ] = FpList.distinct( this )

    override def partition( fn : T => Boolean ) : (FpList[ T ], FpList[ T ]) = FpList.partition( this )( fn )

    override def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpList[ B ] = FpList.collect[ T, B ]( this )( fn )

    override def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpList[ B ] = FpList.sort[ T, B ]( this )( ord )

    override def sortBy[ B >: T, C ]( fn : B => C )( implicit ord : Ordering[ C ] ) : FpList[ B ] = FpList.sortBy[ T, B, C ]( this )( fn )

    override def sortWith[ B >: T ]( cmp : (B, B) => Int )  : FpList[ B ] = FpList.sortWith[ T, B ]( this )( cmp )

    override def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpSeq[ (B, C) ] = ???

    override def zipWithIndex[ B >: T ] : FpSeq[ (B, Int) ] = ???

    override def withLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    override def withRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpSeq[ B ] = ???

    def reduce[ B >: T ]( fn : (B, B) => B ) : FpOption[ B ] = FpList.reduce[ B ]( this )( fn )

    def foldLeft[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : A = FpList.foldLeft( this )( aggregate )( fn )

    def foldRight[ A, B >: T ]( aggregate : A )( fn : (A, B) => A ) : A = FpList.foldRight( this )( aggregate )( fn )

    final def lenEq( num : Int ) : Boolean = FpList.lenEq( this )( num )

    final def lenGt( num : Int ) : Boolean = FpList.lenGt( this )( num )

    final def lenGte( num : Int ) : Boolean = FpList.lenGte( this )( num )

    final def lenLt( num : Int ) : Boolean = FpList.lenLt( this )( num )

    final def lenLte( num : Int ) : Boolean = FpList.lenLte( this )( num )

}

case class FpLs[ +T ]( head : T, tail : FpList[ T ] ) extends FpList[ T ]

case object FpNil extends FpList[ Nothing ]

object FpList
  extends MonadStatic[ FpList ]
    with WithTransformer[ FpList ]
    with MonoidCovariantStatic[ FpList ] {

    def apply : FpNil.type = FpNil

    def apply[ T ]( tailIn : FpList[ T ], headIn : T ) : FpList[ T ] = FpLs( headIn, tailIn )

    def apply[ T ]( list : List[ T ] ) : FpList[ T ] = convertList( list ).reverse

    def unapply[ T ]( fpList : FpList[ T ] ) : Option[ (FpList[ T ], T) ] = fpList match {
        case FpNil => None
        case FpLs( head, tail ) => Some( (tail, head) )
    }

    def gen[ A ]( num : Int )( ele : A ) : FpList[ A ] = StackSafe.selfCall[ Int, FpList[ A ] ] {
        thisFn =>
            num =>
                if ( num <= 0 ) {
                    Result( FpNil )
                }
                else Call.from {
                    thisFn( num - 1 ).map( list => list :+ ele )
                }
    }( num )


    private def convertList[ T ] : List[ T ] => FpList[ T ] = StackSafe.selfCall[ List[ T ], FpList[ T ] ] {
        thisFn => {
            case Nil => Result( FpNil )
            case v :: Nil => Result( unit( v ) )
            case v :: fpList => Call.from {
                thisFn( fpList ).map( _ :+ v )
            }
        }
    }

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] =
        StackSafe.selfCall2[ FpList[ A ], A => FpList[ B ], FpList[ B ] ] { thisFn => ( l, f ) => l match {
            case FpNil => Result( FpNil )
            case FpLs( head, FpNil ) => Result( f( head ) )
            case FpLs( h, t ) => Call.from {
                thisFn( t, f ).map( _ :++ fn( h ) )
            }
        }
    }( a, fn )

    override def empty : FpList[ Nothing ] = FpNil

    override def unit[ A ]( ele : A ) : FpList[ A ] = ele +: FpNil

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = a :++ b

    def filter[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] = flatMap( list )( v => {
        if ( fn( v ) ) v +: FpNil
        else FpNil
    } )

    def headOption[ T ]( l : FpList[ T ] ) : FpOption[ T ] = l match {
        case FpNil => FpNone
        case FpLs( head, _ ) => FpSome( head )
    }

    def tailOption[ T ]( l : FpList[ T ] ) : FpOption[ FpList[ T ] ] = l match {
        case FpNil => FpNone
        case FpLs( _, tail ) => FpSome( tail )
    }

    @tailrec
    final def first[ T ]( l : FpList[ T ] ) : T = l match {
        case FpNil => throw new Exception
        case FpLs( v, FpNil ) => v
        case FpLs( _, next ) => first( next )
    }

    def last[ T ]( l : FpList[ T ] ) : T = l match {
        case FpNil => throw new Exception
        case FpLs( v, _ ) => v
    }


    def append[ T, B >: T ]( l : FpList[ T ], ele : B ) : FpList[ B ] = l :++ unit( ele )

    def prepend[ T, B >: T ]( l : FpList[ T ], ele : B ) : FpList[ B ] = FpLs( ele, l )

    def concat[ T, B >: T ] : (FpList[ B ], FpList[ B ]) => FpList[ B ] =
        StackSafe.selfCall2[ FpList[ B ], FpList[ B ], FpList[ B ] ] {
            ( thisFn : (FpList[ B ], FpList[ B ]) => StackSafe[ FpList[ B ] ] ) =>
                ( l1 : FpList[ B ], l2 : FpList[ B ] ) =>
                    l1 match {
                        case FpNil => Result( l2 )
                        case _ => l2 match {
                            case FpNil => Result( l1 )
                            case FpLs( ele, FpNil ) => Result( ele +: l1 )
                            case FpLs( ele, eles ) => Call.from {
                                thisFn( l1, eles ).map( ele +: _ )
                            }
                        }
                    }
        }

    def fpString[ T ]( fpList : FpList[ T ], sep : FpString ) : FpString = StackSafe.selfCall {
        ( thisFn : FpList[ T ] => StackSafe[ FpString ] ) =>
            ( list : FpList[ T ] ) =>
                list match {
                    case FpNil => Result( FpString( "" ) )
                    case FpLs( v, FpNil ) => Result( FpString( v.toString ) )
                    case FpLs( v, next ) => Call.from {
                        thisFn( next ).map( fpStr => FpString( s"$v$sep$fpStr" )  )
                    }
                }
    }( fpList )


    def reverse[ T ] : FpList[ T ] => FpList[ T ] = StackSafe.selfCall[ FpList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpNil => Result( FpNil )
            case l@FpLs( FpNil, _ ) => Result( l )
            case FpLs( v, next ) => Call.from {
                thisFn( next ).map( rvdNext => ( v +: FpNil ) :++ rvdNext )
            }
        }
    }

    def length[ T ] : FpList[ T ] => FpOption[ Int ] = StackSafe.selfCall[ FpList[ T ], FpOption[ Int ] ] {
        thisFn => {
            case FpNil => Result( FpSome( 0 ) )
            case FpLs( _, t ) => Call.from( thisFn( t ).map( _.map( _ + 1 ) ) )
        }
    }

    private def toStringInternal[ A ] : FpList[ A ] => String = StackSafe.selfCall[ FpList[ A ], String ] {
        thisFn => {
            case FpNil => Result( "FpNil" )
            case FpLs( v, next ) => Call.from {
                thisFn( next ).map( str => s"${v}, ${str}" )
            }
        }
    }

    def toString[ T ]( list : FpList[ T ] ) : String = s"[${toStringInternal( list )}]"

    def times[ A ] : (FpList[ A ], Int) => FpList[ A ] = StackSafe.selfCall2[ FpList[ A ], Int, FpList[ A ] ] {
        thisFn =>
            ( l : FpList[ A ], n : Int ) =>
                if ( n <= 0 ) Result( FpNil )
                else Call.from {
                    thisFn( l, n - 1 ).map( l ++: _ )
                }
    }

    private def takeLast[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] = StackSafe.selfCall2[ FpList[ A ], Int, FpList[ A ] ] {
        thisFn => ( l, n ) =>
            if ( n <= 0 ) Result( FpNil )
            else l match {
                case FpNil => Result( FpNil )
                case FpLs( v, FpNil ) => Result( v +: FpNil)
                case FpLs( v, next ) => Call.from {
                    thisFn( next, n - 1 ).map( _ :++ ( v +: FpNil ) )
                }
            }
    }( list, num )

    private def takeFirst[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] = takeLast( list.reverse )( num ).reverse

    def take[ T ]( list : FpList[ T ] )( num : Int ) : FpList[ T ] =
        if ( num <= 0 ) takeLast( list )( 0 - num )
        else takeFirst( list )( num )

    private def takeWhileLast[ A ]( list : FpList[ A ] )( fn : A => Boolean ) : FpList[ A ] = StackSafe {
        def takeWhileLastT( l : FpList[ A ] )( f : A => Boolean ) : StackSafe[ FpList[ A ] ] = l match {
            case FpNil => Result( FpNil )
            case FpLs( v, next ) =>
                if ( f( v ) ) Call.from( takeWhileLastT( next )( fn ).map( v +: _ ) )
                else Result( FpNil )
        }

        takeWhileLastT( list )( fn )
    }

    def takeWhileEnd[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        takeWhileLast( list )( fn )

    def takeWhile[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        takeWhileLast( list.reverse )( fn ).reverse

    @tailrec
    private def dropLast[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] =
        if ( num <= 0 ) list
        else list match {
            case FpNil => FpNil
            case FpLs( _, next ) => dropLast[ A ]( next )( num - 1 )
        }

    def drop[ T ]( list : FpList[ T ] )( num : Int ) : FpList[ T ] =
        if ( num <= 0 ) dropLast( list )( 0 - num )
        else dropLast( list.reverse )( num ).reverse

    @tailrec
    private def dropWhileLast[ A ]( list : FpList[ A ] )( fn : A => Boolean ) : FpList[ A ] = list match {
        case FpNil => FpNil
        case FpLs( v, next ) =>
            if ( fn( v ) ) dropWhileLast( next )( fn )
            else v +: next
    }

    def dropWhileEnd[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        dropWhileLast( list )( fn )

    def dropWhile[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        dropWhileLast( list.reverse )( fn ).reverse

    def toList[ T ]( list : FpList[ T ] ) : List[ T ] = list match {
        case FpNil => Nil
        case FpLs( h, FpNil ) => List( h )
        case fpList : FpList[ T ] =>
            def toListRev( revedList : FpList[ T ] ) : List[ T ] = revedList match {
                case FpNil => Nil
                case FpLs( revH, FpNil ) => List( revH )
                case FpLs( revH, revT ) => revH :: toListRev( revT )
            }

            toListRev( fpList.reverse )
    }

    @tailrec
    final def exists[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : Boolean = list match {
        case FpNil => false
        case FpLs( v, next ) =>
            if ( fn( v ) ) true
            else exists( next )( fn )
    }

    @tailrec
    final def contains[ A, B >: A ]( list : FpList[ A ] )( ele : B ) : Boolean = list match {
        case FpNil => false
        case FpLs( head, next ) => if ( head == ele ) true else contains[ B, B ]( next )( ele )
    }

    private def makeDistinct[ A ]( list : FpList[ A ] ) : FpList[ A ] = StackSafe {
        def makeDistinctT( l : FpList[ A ], set : Set[ A ] = Set.empty ) : StackSafe[ FpList[ A ] ] = l match {
            case FpNil => Result( FpNil )
            case FpLs( v, next ) if ( set.contains( v ) ) => Call.from( makeDistinctT( next, set ) )
            case FpLs( v, next ) =>
                val newSet : Set[ A ] = set + v
                Call.from( makeDistinctT( next, newSet ).map( v +: _ ) )
        }

        makeDistinctT( list )
    }

    def distinct[ T ]( list : FpList[ T ] ) : FpList[ T ] = makeDistinct( list.reverse ).reverse

    def partition[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : (FpList[ T ], FpList[ T ]) = {
        StackSafe.selfCall3[ FpList[ T ], FpList[ T ], FpList[ T ], (FpList[ T ], FpList[ T ]) ] {
            thisFn =>
                (filteredIn, filteredOut, l) => l match {
                    case FpNil => Result( (filteredIn, filteredOut) )
                    case FpLs( x, next ) if ( fn( x ) ) => Call.from {
                        thisFn(x +: filteredIn, filteredOut, next )
                    }
                    case FpLs( x, next ) => Call.from {
                        thisFn(filteredIn, x +: filteredOut, next )
                    }
                }
        }( FpNil, FpNil, list )
    }

    def collect[ T, B >: T ]( list : FpList[ T ] )( fn : PartialFunction[ T, B ] ) : FpList[ B ] = flatMap( list )( v => {
        if ( fn.isDefinedAt( v ) ) fn( v ) +: FpNil
        else FpNil
    } )

    def sort[ T, B >: T ]( list : FpList[ T ] )( implicit ord : Ordering[ B ] ) : FpList[ B ] =
        StackSafe.selfCall[ FpList[ T ], FpList[ T ] ] {
            thisFn => {
                case FpNil => Result( FpNil )
                case FpLs( v, FpNil ) => Result( v +: FpNil )
                case FpLs( v, next ) => Call.from {
                    val (gteList, ltList) = next.partition( ( a : T ) => ord.mkOrderingOps( a ) >= v )
                    for {
                        x <- thisFn( ltList )
                        y <- thisFn( gteList ).map( v +: _ )
                    } yield x :++ y
                }

            }
        }( list )

    def sortBy[ T, B >: T, C ]( list : FpList[ T ] )( fn : B => C )( implicit ord : Ordering[ C ] ) : FpList[ B ] = {
        sort( list )( Ordering.by( fn ) )
    }

    def sortWith[ T, B >: T ]( list : FpList[ T ] )( cmp : (B, B) => Int )  : FpList[ B ] = {
        sort( list.asInstanceOf[ FpList[ B ] ] )( new Ordering[ B ] {
            override def compare( x : B, y : B ) : Int = cmp( x, y )
        } )
    }

    def reduce[ T ]( list : FpList[ T ] )( fn : (T, T) => T ) : FpOption[ T ] = list match {
        case FpNil => FpNone
        case FpLs( head, FpNil ) => FpSome( head )
        case FpLs( head, newList ) => reduce( newList )( fn ) match {
            case FpNone => FpSome( head )
            case FpSome( res ) => FpSome( fn( head, res ) )
        }
    }

    @tailrec
    final def lenEq( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num == 0
        case FpLs( _, FpNil ) => num == 1
        case FpLs( _, _ ) if ( num <= 1 ) => false
        case FpLs( _, next ) => lenEq( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGt( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num < 0
        case FpLs( _, FpNil ) => num < 1
        case FpLs( _, _ ) if ( num < 2 ) => true
        case FpLs( _, next ) => lenGt( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGte( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num <= 0
        case FpLs( _, FpNil ) => num <= 1
        case FpLs( _, _ ) if ( num <= 2 ) => true
        case FpLs( _, next ) => lenGte( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLt( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num > 0
        case FpLs( _, FpNil ) => num > 1
        case FpLs( _, _ ) if ( num < 2 ) => false
        case FpLs( _, next ) => lenLt( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLte( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num >= 0
        case FpLs( _, FpNil ) => num >= 1
        case FpLs( _, _ ) if ( num <= 2 ) => false
        case FpLs( _, next ) => lenLte( next )( num - 1 )
        case _ => false
    }

    def foldLeft[ A, T ]( list : FpList[ T ] )( aggregate : A )( fn : (A, T) => A ) : A = StackSafe.selfCall2[ A, FpList[ T ], A ] {
        ( thisFn : (A, FpList[ T ]) => StackSafe[ A ] ) =>
            ( agg : A, l : FpList[ T ] ) => l match {
                case FpNil => Result( agg )
                case FpLs( res, FpNil ) => Result( fn( agg, res ) )
                case FpLs( res, nextL ) => Call.from {
                    thisFn( fn( agg, res ), nextL )
                }
            }
    }( aggregate, list )

    def foldRight[ A, T ]( list : FpList[ T ] )( aggregate : A )( fn : (A, T) => A ) : A = foldLeft( list.reverse )( aggregate )( fn )

    private class ThisTransformerStatic[ M[ +_ ] <: Monad[ M, _ ] ](
      override val innerStatic : MonadStatic[ FpList ],
      override val outerStatic : MonadStatic[ M ],
    ) extends TransformerStatic[ M ] {
        def combine[ A ]( a : Transformer[ M, A ], b : Transformer[ M, A ] ) : Transformer[ M, A ] = {
            T( a.value.asInstanceOf[ Monad[ M, FpList[ A ] ] ].flatMap( ( l1 : FpList[ A ] ) => b.value.asInstanceOf[ Monad[ M, FpList[ A ] ] ].flatMap { l2 : FpList[ A ] => outerStatic.unit( l1 :++ l2 ) } ) )
        }

        override def flatMap[ A, B ]( a : Transformer[ M, A ] )
                                    ( fn : A => Transformer[ M, B ] ) : Transformer[ M, B ] = {
            T[ M, B ]( a.value.asInstanceOf[ Monad[ M, FpList[ A ] ] ].flatMap {
                case FpNil => outerStatic.unit( FpNil ).asInstanceOf[ M[ FpList[ B ] ] ]
                case list : FpList[ A ] => list.map( fn ).foldLeft[ Transformer[ M, B ], Transformer[ M, B ] ]( T[ M, B ]( outerStatic.unit[ FpList[ B ] ]( FpNil ) ) )( ( x, y ) => combine( x, y ) ).value.asInstanceOf[ M[ FpList[ B ] ] ]
            } )
        }
    }

    override protected def TS[ M[ +_ ] <: Monad[ M, _ ] ]( outerStatic : MonadStatic[ M ] ) : TransformerStatic[ M ] = {
        new ThisTransformerStatic[ M ]( this, outerStatic )
    }
}
