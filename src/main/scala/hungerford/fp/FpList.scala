package hungerford.fp

import hungerford.fp._

import scala.annotation.tailrec

abstract class FpList[ +T ] extends MonadCovariant[ FpList, T ]  with MonoidCovariant[ FpList, T ] {

    def tail : FpList[ T ]
    def head : T

    def headOption : Option[ T ] = this match {
        case FpNil => None
        case FpList( _, v : T ) => Some( v )
    }

    @tailrec
    final def first : T = this match {
        case FpNil => throw new Exception
        case FpList( FpNil, v ) => v
        case FpList( next, v ) => next.first
    }

    def last : T = this match {
        case FpNil => throw new Exception
        case FpList( _, v ) => v
    }

    def + [B >: T]( ele : B ) : FpList[ B ] = FpList( this, ele )

    def ++ [B >: T]( fpList : FpList[ B ] ) : FpList[ B ] = StackSafe {
        def listAdd( l1 : FpList[ B ], l2 : FpList[ B ] ) : StackSafe[ FpList[ B ] ] = l1 match {
            case FpNil => Result( l2 )
            case _ => l2 match {
                case FpNil => Result( l1 )
                case FpList( FpNil, ele ) => Result( l1 + ele )
                case FpList( eles, ele ) => Call.from {
                    listAdd(l1, eles).map( _ + ele )
                }
            }
        }

        listAdd( this, fpList )
    }

    def fpString : FpString = StackSafe {

        def toStringInternalT( l : FpList[ T ] ) : StackSafe[ FpString ] = l match {
            case FpNil => Result( FpString( "" ) )
            case FpList( next, v ) => Call.from {
                toStringInternalT( next ).map( fpString => FpString( s"${fpString}${v}" ) )
            }
        }

        toStringInternalT( this )
    }

    def reverse : FpList[ T ] = StackSafe {
        def reverseT( l : FpList[ T ] ) : StackSafe[ FpList[ T ] ] = l match {
            case FpNil => Result( FpNil )
            case FpList( FpNil, _ ) => Result( l )
            case _ => Call.from {
                reverseT( l.tail ).map( rvdTail => (FpNil + l.head) ++ rvdTail )
            }
        }

        reverseT( this )
    }

    final def length : Int =  StackSafe {
        def lengthT( l : FpList[ T ] ) : StackSafe[ Int ] = l match {
            case FpNil => Result( 0 )
            case FpList( t, h ) => Call.from {
                lengthT( t ).map( _ + 1 )
            }
        }

        lengthT( this )
    }

    @tailrec
    final def get( i : Int ) : T = {
        if ( i < 0 ) throw new IndexOutOfBoundsException( s"hungerford.fp.FpList index $i is less than 0" )
        if ( i == 0 ) this.head
        else this.tail.get( i - 1 )
    }

    private def toStringInternal[ A ]( list : FpList[ A ] ) : String = StackSafe {

        def toStringInternalT( l : FpList[ A ] ) : StackSafe[ String ] = l match {
            case FpNil => Result( "FpNil" )
            case FpList( next, v ) => Call.from {
                toStringInternalT( next ).map( str => s"${str}, ${v}" )
            }
        }

        toStringInternalT( list )
    }

    override def toString : String = s"[${toStringInternal( this )}]"

    def times( num : Int ) : FpList[ T ] = StackSafe {
        def timesT( l : FpList[ T ] )( n : Int ) : StackSafe[ FpList[ T ] ] =
            if ( n <= 0 ) Result( FpNil )
            else Call.from {
                timesT( l )( n - 1 ).map( _ ++ l )
            }

        timesT( this )( num )
    }

    override def empty : FpList[ Nothing ] = FpList.empty

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = FpList.combine( a, b )

    override def unit[ A ]( ele : A ) : FpList[ A ] = FpList.unit( ele )

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] = FpList.flatMap( a )( fn )

    override def get[ A ]( ele : FpList[ A ] ) : Option[ A ] = FpList.get( ele )

    def filter( fn : T => Boolean ) : FpList[ T ] = flatMap( v => {
       if ( fn( v ) ) FpNil + v
       else FpNil
    } )

    private def takeLast[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] = StackSafe {
        def takeLastT( l : FpList[ A ] )( n : Int ) : StackSafe[ FpList[ A ] ] =
            if ( n <= 0 ) Result( FpNil )
            else l match {
                case FpNil => Result( FpNil )
                case FpList( FpNil, v ) => Result( FpNil + v )
                case FpList( next, v ) => Call.from {
                    takeLastT( next )( n - 1 ).map( _ ++ ( FpNil + v ) )
                }
            }

        takeLastT( list )( num )
    }

    private def takeFirst[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] = takeLast( list.reverse )( num ).reverse

    def take( num : Int ) : FpList[ T ] =
        if ( num <= 0 ) takeLast( this )( 0 - num )
        else takeFirst( this )( num )

    private def takeWhileLast[ A ]( list : FpList[ A ] )( fn : A => Boolean ) : FpList[ A ] = StackSafe {
        def takeWhileLastT( l : FpList[ A ] )( f : A => Boolean ) : StackSafe[ FpList[ A ] ] = l match {
            case FpNil => Result( FpNil )
            case FpList( next : FpList[ A ], v : A ) =>
                if ( f( v ) ) Call.from( takeWhileLastT( next )( fn ).map( _ + v ) )
                else Result( FpNil )
        }

        takeWhileLastT( list )( fn )
    }

    def takeWhileEnd( fn : T => Boolean ) : FpList[ T ] = takeWhileLast( this )( fn )

    def takeWhile( fn: T => Boolean ) : FpList[ T ] = takeWhileLast( this.reverse )( fn ).reverse

    @tailrec
    private def dropLast[ A ]( list : FpList[ A ] )( num : Int ) : FpList[ A ] =
        if ( num <= 0 ) list
        else list match {
            case FpNil => FpNil
            case FpList( next, _ ) => dropLast[ A ]( next )( num - 1 )
        }

    def drop( num : Int ) : FpList[ T ] =
        if ( num <= 0 ) dropLast( this )( 0 - num )
        else dropLast( this.reverse )( num ).reverse

    @tailrec
    private def dropWhileLast[ A ]( list : FpList[ A ] )( fn : A => Boolean ) : FpList[ A ] = list match {
        case FpNil => FpNil
        case FpList( next, v ) =>
            if ( fn( v ) ) dropWhileLast( next )( fn )
            else next + v
    }

    def dropWhileEnd( fn : T => Boolean ) : FpList[ T ] = dropWhileLast( this )( fn )

    def dropWhile( fn : T => Boolean ) : FpList[ T ] = dropWhileLast( this.reverse )( fn ).reverse

    def toList : List[ T ] = this match {
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
    final def exists( fn : T => Boolean ) : Boolean = this match {
        case FpNil => false
        case FpList( next, v ) =>
            if ( fn( v ) ) true
            else next.exists( fn )
    }

    @tailrec
    final def contains[ A ]( ele : A ) : Boolean = this match {
        case FpNil => false
        case FpList( _, ele ) => true
        case FpList( next, _ ) => next.contains( ele )
    }

    private def makeDistinct[ A ]( list : FpList[ A ] ) : FpList[ A ] = StackSafe {
        def makeDistinctT( l : FpList[ A ], set : Set[ A ] = Set.empty ) : StackSafe[ FpList[ A ] ] = l match {
            case FpNil => Result( FpNil )
            case FpList( next, v ) if ( set.contains( v ) ) => Call.from( makeDistinctT( next, set ) )
            case FpList( next : FpList[ A ], v : A ) =>
                val newSet : Set[ A ] = set + v
                Call.from( makeDistinctT( next, newSet ).map( _ + v ) )
        }

        makeDistinctT( list )
    }

    def distinct : FpList[ T ] = makeDistinct( this.reverse ).reverse

    def collect[ B ]( fn : PartialFunction[ T, B ] ) : FpList[ B ] = flatMap( v => {
        if ( fn.isDefinedAt( v ) ) FpNil + fn( v )
        else FpNil
    } )

    def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpList[ B ] = StackSafe {

        def sortT[ A ]( list : FpList[ A ] )( implicit ordr : Ordering[ A ] ) : StackSafe[ FpList[ A ] ] =
            list match {
                case FpNil => Result( FpNil )
                case FpList( FpNil, v ) => Result( FpNil + v )
                case FpList( next : FpList[ A ], v : A ) => Call.from {
                    for {
                        x <- sortT( next.filter( ( a : A ) => ordr.mkOrderingOps( a ) <= v ) ).map( _ + v )
                        y <- sortT( next.filter( ( a : A ) => ordr.mkOrderingOps( a ) > v ) )
                    } yield x ++ y
                }

            }

        sortT( this.asInstanceOf[ FpList[ B ] ] )( ord )
    }

    @tailrec
    final def lenEq( num : Int ) : Boolean = this match {
        case FpNil => num == 0
        case FpList( FpNil, _ ) => num == 1
        case FpList( _, _ ) if ( num <= 1 ) => false
        case FpList( next, _ ) => next.lenEq( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGt( num : Int ) : Boolean = this match {
        case FpNil => num < 0
        case FpList( FpNil, _ ) => num < 1
        case FpList( _, _ ) if ( num < 2 ) => true
        case FpList( next, _ ) => next.lenGt( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenGte( num : Int ) : Boolean = this match {
        case FpNil => num <= 0
        case FpList( FpNil, _ ) => num <= 1
        case FpList( _, _ ) if ( num <= 2 ) => true
        case FpList( next, _ ) => next.lenGte( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLt( num : Int ) : Boolean = this match {
        case FpNil => num > 0
        case FpList( FpNil, _ ) => num > 1
        case FpList( _, _ ) if ( num < 2 ) => false
        case FpList( next, _ ) => next.lenLt( num - 1 )
        case _ => false
    }

    @tailrec
    final def lenLte( num : Int ) : Boolean = this match {
        case FpNil => num >= 0
        case FpList( FpNil, _ ) => num >= 1
        case FpList( _, _ ) if ( num <= 2 ) => false
        case FpList( next, _ ) => next.lenLte( num - 1 )
        case _ => false
    }

}

object FpList extends MonadStatic[ FpList ] with MonoidCovariantStatic[ FpList ] {

    def apply : FpNil.type = FpNil
    def apply[ T ]( tailIn : FpList[ T ], headIn : T ) : FpList[ T ] = new FpList[ T ] {
        override def tail : FpList[ T ] = tailIn
        override def head : T = headIn
    }

    def gen[ A ]( num : Int )( ele : A ) : FpList[ A ] = StackSafe {

        def genT[ A ]( num : Int )( ele : A ) : StackSafe[ FpList[ A ] ] = {
            if ( num <= 0 ) {
                Result( FpNil )
            }
            else Call.from {
                genT( num - 1 )( ele ).flatMap( (list : FpList[ A ]) => Result( list + ele ) )
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

    def apply[ T ]( list : List[ T ] ) : FpList[ T ] = convertList( list ).reverse

    def unapply[ T ]( fpList : FpList[ T ] ) : Option[ (FpList[ T ], T) ] = fpList match {
        case FpNil => None
        case _ => Some( (fpList.tail, fpList.head) )
    }

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] = StackSafe {

        def flatMapT( b : FpList[ A ] )( fn : A => FpList[ B ] ) : StackSafe[ FpList[ B ] ] = b match {
            case FpNil => Result( FpNil )
            case FpList( FpNil, head : A ) => Result( fn( head ) )
            case FpList( t : FpList[ A ], h : A ) => Call.from {
                flatMapT( t )( fn ).map( _ ++ fn( h ) )
            }
        }

        flatMapT( a )( fn )
    }

    override def empty : FpList[ Nothing ] = FpNil

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = a ++ b

    override def unit[ A ]( ele : A ) : FpList[ A ] = FpNil + ele

    override def get[ A ]( ele : FpList[ A ] ) : Option[ A ] = ele match {
        case FpNil => None
        case FpList( _, v : A ) => Some( v )
    }

}

case object FpNil extends FpList[ Nothing ] {

    override def head : Nothing = throw new Exception
    override def tail : FpList[ Nothing ] = throw new Exception

}


