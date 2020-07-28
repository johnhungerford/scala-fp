package hungerford.fp

import hungerford.fp._

import scala.annotation.tailrec

abstract class FpList[ +T ] extends MonadCovariant[ FpList, T ] with MonoidCovariant[ FpList, T ] {

    def tail : FpList[ T ]

    def head : T

    def headOption : Option[ T ] = FpList.headOption( this )

    final def first : T = FpList.first( this )

    def last : T = FpList.last( this )

    def +[ B >: T ]( ele : B ) : FpList[ B ] = FpList.append[ T, B ]( this, ele )
    def append[ B >: T ]( ele : B ) : FpList[ B ] = FpList.append[ T, B ]( this, ele )

    def ++[ B >: T ]( fpList : FpList[ B ] ) : FpList[ B ] = FpList.concat[ T, B ]( this, fpList )

    def fpString : FpString = FpList.fpString( this )

    def reverse : FpList[ T ] = FpList.reverse( this )

    def length : Int = FpList.length( this )

    def get( i : Int ) : Option[ T ] = FpList.get( this, i )

    override def toString : String = FpList.toString( this )

    def times( num : Int ) : FpList[ T ] = FpList.times( this, num )

    override def empty : FpList[ Nothing ] = FpList.empty

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = FpList.combine( a, b )

    override def unit[ A ]( ele : A ) : FpList[ A ] = FpList.unit( ele )

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] = FpList.flatMap( a )( fn )

    override def get[ A ]( ele : FpList[ A ] ) : Option[ A ] = FpList.get( ele )

    def filter( fn : T => Boolean ) : FpList[ T ] = FpList.filter( this )( fn )

    def take( num : Int ) : FpList[ T ] = FpList.take( this )( num )

    def takeWhileEnd( fn : T => Boolean ) : FpList[ T ] = FpList.takeWhileEnd( this )( fn )

    def takeWhile( fn : T => Boolean ) : FpList[ T ] = FpList.takeWhile( this )( fn )

    def dropWhileEnd( fn : T => Boolean ) : FpList[ T ] = FpList.dropWhileEnd( this )( fn )

    def dropWhile( fn : T => Boolean ) : FpList[ T ] = FpList.dropWhile( this )( fn )

    def toList : List[ T ] = FpList.toList( this )

    def exists( fn : T => Boolean ) : Boolean = FpList.exists( this )( fn )

    def contains[ B >: T ]( ele : B ) : Boolean = FpList.contains[ T, B ]( this )( ele )

    def distinct : FpList[ T ] = FpList.distinct( this )

    def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpList[ B ] = FpList.collect[ T, B ]( this )( fn )

    def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpList[ B ] = FpList.sort[ T, B ]( this )( ord )

    final def lenEq( num : Int ) : Boolean = FpList.lenEq( this )( num )

    final def lenGt( num : Int ) : Boolean = FpList.lenGt( this )( num )

    final def lenGte( num : Int ) : Boolean = FpList.lenGte( this )( num )

    final def lenLt( num : Int ) : Boolean = FpList.lenLt( this )( num )

    final def lenLte( num : Int ) : Boolean = FpList.lenLte( this )( num )

}


case object FpNil extends FpList[ Nothing ] {

    override def head : Nothing = throw new Exception

    override def tail : FpList[ Nothing ] = throw new Exception

}


object FpList extends MonadStatic[ FpList ] with MonoidCovariantStatic[ FpList ] {

    def apply : FpNil.type = FpNil

    def apply[ T ]( tailIn : FpList[ T ], headIn : T ) : FpList[ T ] = new FpList[ T ] {
        override def tail : FpList[ T ] = tailIn

        override def head : T = headIn
    }

    def apply[ T ]( list : List[ T ] ) : FpList[ T ] = convertList( list ).reverse

    def gen[ A ]( num : Int )( ele : A ) : FpList[ A ] = StackSafe {

        def genT[ A ]( num : Int )( ele : A ) : StackSafe[ FpList[ A ] ] = {
            if ( num <= 0 ) {
                Result( FpNil )
            }
            else Call.from {
                genT( num - 1 )( ele ).flatMap( ( list : FpList[ A ] ) => Result( list + ele ) )
            }
        }

        genT( num )( ele )
    }


    private def convertList[ T ]( list : List[ T ] ) : FpList[ T ] = StackSafe {
        def clT( l : List[ T ] ) : StackSafe[ FpList[ T ] ] = l match {
            case Nil => Result( FpNil )
            case v :: Nil => Result( unit( v ) )
            case v :: fpList => Call.from {
                clT( fpList ).map( _ + v )
            }
        }

        clT( list )
    }

    def unapply[ T ]( fpList : FpList[ T ] ) : Option[ (FpList[ T ], T) ] = fpList match {
        case FpNil => None
        case _ => Some( (fpList.tail, fpList.head) )
    }

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] =
        StackSafe.selfCall2[ FpList[ A ], A => FpList[ B ], FpList[ B ] ] { thisFn => ( l, f ) => l match {
            case FpNil => Result( FpNil )
            case FpList( FpNil, head ) => Result( f( head ) )
            case FpList( t, h ) => Call.from {
                thisFn( t, f ).map( _ ++ fn( h ) )
            }
        }
    }( a, fn )

    override def empty : FpList[ Nothing ] = FpNil

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = a ++ b

    override def unit[ A ]( ele : A ) : FpList[ A ] = FpNil + ele

    override def get[ A ]( ele : FpList[ A ] ) : Option[ A ] = ele match {
        case FpNil => None
        case FpList( _, v ) => Some( v )
    }

    def filter[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] = flatMap( list )( v => {
        if ( fn( v ) ) FpNil + v
        else FpNil
    } )

    def headOption[ T ]( l : FpList[ T ] ) : Option[ T ] = l match {
        case FpNil => None
        case FpList( _, v ) => Some( v )
    }

    @tailrec
    final def first[ T ]( l : FpList[ T ] ) : T = l match {
        case FpNil => throw new Exception
        case FpList( FpNil, v ) => v
        case FpList( next, v ) => first( next )
    }

    def last[ T ]( l : FpList[ T ] ) : T = l match {
        case FpNil => throw new Exception
        case FpList( _, v ) => v
    }

    def append[ T, B >: T ]( l : FpList[ T ], ele : B ) : FpList[ B ] = FpList( l, ele )

    def concat[ T, B >: T ] : (FpList[ B ], FpList[ B ]) => FpList[ B ] =
        StackSafe.selfCall2[ FpList[ B ], FpList[ B ], FpList[ B ] ] {
            ( thisFn : (FpList[ B ], FpList[ B ]) => StackSafe[ FpList[ B ] ] ) =>
                ( l1 : FpList[ B ], l2 : FpList[ B ] ) =>
                    l1 match {
                        case FpNil => Result( l2 )
                        case _ => l2 match {
                            case FpNil => Result( l1 )
                            case FpList( FpNil, ele ) => Result( l1 + ele )
                            case FpList( eles, ele ) => Call.from {
                                thisFn( l1, eles ).map( _ + ele )
                            }
                        }
                    }
        }

    def fpString[ T ] : FpList[ T ] => FpString = StackSafe.selfCall {
        ( thisFn : FpList[ T ] => StackSafe[ FpString ] ) =>
            ( list : FpList[ T ] ) =>
                list match {
                    case FpNil => Result( FpString( "" ) )
                    case FpList( next, v ) => Call.from {
                        thisFn( next ).map( fpString => FpString( s"${fpString}${v}" ) )
                    }
                }
    }


    def reverse[ T ] : FpList[ T ] => FpList[ T ] = StackSafe.selfCall[ FpList[ T ], FpList[ T ] ] {
        thisFn => {
            case FpNil => Result( FpNil )
            case l@FpList( FpNil, _ ) => Result( l )
            case FpList( next, v ) => Call.from {
                thisFn( next ).map( rvdNext => ( FpNil + v ) ++ rvdNext )
            }
        }
    }

    def length[ T ] : FpList[ T ] => Int = StackSafe.selfCall[ FpList[ T ], Int ] {
        thisFn => {
            case FpNil => Result( 0 )
            case FpList( t, h ) => Call.from( thisFn( t ).map( _ + 1 ) )
        }
    }

    @tailrec
    final def get[ T ]( list : FpList[ T ], i : Int ) : Option[ T ] = list match {
        case FpNil => None
        case _ if ( i < 0 ) => None
        case FpList( _, v ) if ( i == 0 ) => Some( v )
        case FpList( next, _ ) => get( next, i - 1 )
    }

    private def toStringInternal[ A ] : FpList[ A ] => String = StackSafe.selfCall[ FpList[ A ], String ] {
        thisFn => {
            case FpNil => Result( "FpNil" )
            case FpList( next, v ) => Call.from {
                thisFn( next ).map( str => s"${str}, ${v}" )
            }
        }
    }

    def toString[ T ]( list : FpList[ T ] ) : String = s"[${toStringInternal( list )}]"

    def times[ A ] : (FpList[ A ], Int) => FpList[ A ] = StackSafe.selfCall2[ FpList[ A ], Int, FpList[ A ] ] {
        thisFn =>
            ( l : FpList[ A ], n : Int ) =>
                if ( n <= 0 ) Result( FpNil )
                else Call.from {
                    thisFn( l, n - 1 ).map( _ ++ l )
                }
    }

    private def takeLast[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] = StackSafe.selfCall2[ FpList[ A ], Int, FpList[ A ] ] {
        thisFn => ( l, n ) =>
            if ( n <= 0 ) Result( FpNil )
            else l match {
                case FpNil => Result( FpNil )
                case FpList( FpNil, v ) => Result( FpNil + v )
                case FpList( next, v ) => Call.from {
                    thisFn( next, n - 1 ).map( _ ++ ( FpNil + v ) )
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
            case FpList( next, v ) =>
                if ( f( v ) ) Call.from( takeWhileLastT( next )( fn ).map( _ + v ) )
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
            case FpList( next, _ ) => dropLast[ A ]( next )( num - 1 )
        }

    def drop[ T ]( list : FpList[ T ] )( num : Int ) : FpList[ T ] =
        if ( num <= 0 ) dropLast( list )( 0 - num )
        else dropLast( list.reverse )( num ).reverse

    @tailrec
    private def dropWhileLast[ A ]( list : FpList[ A ] )( fn : A => Boolean ) : FpList[ A ] = list match {
        case FpNil => FpNil
        case FpList( next, v ) =>
            if ( fn( v ) ) dropWhileLast( next )( fn )
            else next + v
    }

    def dropWhileEnd[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        dropWhileLast( list )( fn )

    def dropWhile[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : FpList[ T ] =
        dropWhileLast( list.reverse )( fn ).reverse

    def toList[ T ]( list : FpList[ T ] ) : List[ T ] = list match {
        case FpNil => Nil
        case FpList( FpNil, h ) => List( h )
        case fpList : FpList[ T ] =>
            def toListRev( revedList : FpList[ T ] ) : List[ T ] = revedList match {
                case FpNil => Nil
                case FpList( FpNil, revH ) => List( revH )
                case FpList( revT, revH ) => revH :: toListRev( revT )
            }

            toListRev( fpList.reverse )
    }

    @tailrec
    final def exists[ T ]( list : FpList[ T ] )( fn : T => Boolean ) : Boolean = list match {
        case FpNil => false
        case FpList( next, v ) =>
            if ( fn( v ) ) true
            else exists( next )( fn )
    }

    @tailrec
    final def contains[ A, B >: A ]( list : FpList[ A ] )( ele : B ) : Boolean = list match {
        case FpNil => false
        case FpList( _, ele ) => true
        case FpList( next, _ ) => contains[ B, B ]( next )( ele )
    }

    private def makeDistinct[ A ]( list : FpList[ A ] ) : FpList[ A ] = StackSafe {
        def makeDistinctT( l : FpList[ A ], set : Set[ A ] = Set.empty ) : StackSafe[ FpList[ A ] ] = l match {
            case FpNil => Result( FpNil )
            case FpList( next, v ) if ( set.contains( v ) ) => Call.from( makeDistinctT( next, set ) )
            case FpList( next, v ) =>
                val newSet : Set[ A ] = set + v
                Call.from( makeDistinctT( next, newSet ).map( _ + v ) )
        }

        makeDistinctT( list )
    }

    def distinct[ T ]( list : FpList[ T ] ) : FpList[ T ] = makeDistinct( list.reverse ).reverse

    def collect[ T, B >: T ]( list : FpList[ T ] )( fn : PartialFunction[ T, B ] ) : FpList[ B ] = flatMap( list )( v => {
        if ( fn.isDefinedAt( v ) ) FpNil + fn( v )
        else FpNil
    } )

    def sort[ T, B >: T ]( list : FpList[ T ] )( implicit ord : Ordering[ B ] ) : FpList[ T ] =
        StackSafe.selfCall[ FpList[ T ], FpList[ T ] ] {
            thisFn => {
                case FpNil => Result( FpNil )
                case FpList( FpNil, v ) => Result( FpNil + v )
                case FpList( next, v ) => Call.from {
                    for {
                        x <- thisFn( next.filter( ( a : T ) => ord.mkOrderingOps( a ) <= v ) ).map( _ + v )
                        y <- thisFn( next.filter( ( a : T ) => ord.mkOrderingOps( a ) > v ) )
                    } yield x ++ y
                }

            }
        }( list )

    @tailrec
    final def lenEq( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num == 0
        case FpList( FpNil, _ ) => num == 1
        case FpList( _, _ ) if ( num <= 1 ) => false
        case FpList( next, _ ) => lenEq( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGt( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num < 0
        case FpList( FpNil, _ ) => num < 1
        case FpList( _, _ ) if ( num < 2 ) => true
        case FpList( next, _ ) => lenGt( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGte( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num <= 0
        case FpList( FpNil, _ ) => num <= 1
        case FpList( _, _ ) if ( num <= 2 ) => true
        case FpList( next, _ ) => lenGte( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLt( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num > 0
        case FpList( FpNil, _ ) => num > 1
        case FpList( _, _ ) if ( num < 2 ) => false
        case FpList( next, _ ) => lenLt( next )( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLte( list : FpList[ _ ] )( num : Int ) : Boolean = list match {
        case FpNil => num >= 0
        case FpList( FpNil, _ ) => num >= 1
        case FpList( _, _ ) if ( num <= 2 ) => false
        case FpList( next, _ ) => lenLte( next )( num - 1 )
        case _ => false
    }

}
