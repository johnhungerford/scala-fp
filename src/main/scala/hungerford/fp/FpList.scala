package hungerford.fp

import scala.annotation.tailrec

sealed abstract class FpList[ +T ] extends Monad[ FpList, T ]  with Monoid[ FpList, T ] {
    def tail : FpList[ T ]
    def head : T

    def + [B >: T]( ele : B ) : FpList[ B ] = FpList( this, ele )
    def ++ [B >: T]( fpList : FpList[ B ] ) : FpList[ B ] = this match {
        case FpNil => fpList
        case _ => fpList match {
            case FpNil => this
            case FpList( FpNil, ele ) => this + ele
            case FpList( eles, ele ) => (this ++ eles) + ele
        }
    }

    def flatten : FpList[ Any ] = this match {
        case FpNil => this
        case FpList( eles : FpList[ _ ], ele : FpList[ _ ] ) => eles.flatten ++ ele.flatten
        case _ => this
    }

    def reverse : FpList[ T ] = this match {
        case FpNil => this
        case FpList( FpNil, _ ) => this
        case _ => ( FpNil + this.head ) ++ this.tail.reverse
    }

    final def length : Int = if ( this == FpNil ) 0 else 1 + this.tail.length

    @tailrec
    final def get( i : Int ) : T = {
        if ( i < 0 ) throw new IndexOutOfBoundsException( s"hungerford.fp.FpList index $i is less than 0" )
        if ( i == 0 ) this.head
        else this.tail.get( i - 1 )
    }

    override def toString : String = this match {
        case FpNil => "FpNil"
        case _ => s"${this.tail} + ${this.head}"
    }

    override def empty : FpList[ Nothing ] = FpList.empty

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = FpList.combine( a, b )

    override def unit[ A ]( ele : A ) : FpList[ A ] = FpList.unit( ele )

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] = FpList.flatMap( a )( fn )

    override def get[ A ]( ele : FpList[ A ] ) : Option[ A ] = FpList.get( ele )

}

object FpList extends MonadStatic[ FpList ] with MonoidStatic[ FpList ] {
    def apply : FpNil.type = FpNil
    def apply[ T ]( tailIn : FpList[ T ], headIn : T ) : FpList[ T ] = new FpList[ T ] {
        override def tail : FpList[ T ] = tailIn
        override def head : T = headIn
    }

    def unapply[ T ]( fpList : FpList[ T ] ) : Option[ (FpList[ T ], T) ] = fpList match {
        case FpNil => None
        case _ => Some( (fpList.tail, fpList.head) )
    }

    override def flatMap[ A, B ]( a : FpList[ A ] )( fn : A => FpList[ B ] ) : FpList[ B ] = a match {
        case FpNil => FpNil
        case FpList( FpNil, head : A ) => fn( head )
        case FpList( tail : FpList[ A ], head : A ) => tail.flatMap( fn ) ++ fn( head )
    }

    override def empty : FpList[ Nothing ] = FpNil

    override def combine[ B ]( a : FpList[ B ], b : FpList[ B ] ) : FpList[ B ] = b match {
        case FpNil => a
        case ls : FpList[ B ] => a ++ ls
        case _ => throw new Exception
    }

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


