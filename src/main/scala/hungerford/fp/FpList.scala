package hungerford.fp

import scala.annotation.tailrec

sealed abstract class FpList[ +T ] extends Monoid[ FpList[ T ] ] with Monad[ FpList[ T ], T ] {
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
        case _ => s"${this.tail}, ${this.head}"
    }

    override def empty : FpList[ T ] = FpNil

    override def combine[ B >: FpList[ T ] ]( ele : B ) : B = ele match {
        case FpNil => this
        case ls : FpList[ T ] => this ++ ls
        case _ => throw new Exception
    }

    override def unit[ C >: FpList[ T ], B >: T ]( ele : B ) : Monad[ C, B ] = FpList( FpNil, ele ).asInstanceOf[ Monad[ C, B ] ]

    override def flatMap[ C, B ]( fn : T => Monad[ C, B ] ) : Monad[ C, B ] = this match {
        case FpNil => FpNil.asInstanceOf[ Monad[ C, B ] ]
        case FpList( FpNil, head : T ) => fn( head )
        case FpList( tail : FpList[ T ], head : T ) => (tail.flatMap( fn ).asInstanceOf[ FpList[ B ] ] ++ ( fn( head ) ).asInstanceOf[ FpList[ B ] ]).asInstanceOf[ Monad[ C, B ] ]
    }

}

object FpList {
    def apply : FpNil.type = FpNil
    def apply[ T ]( tailIn : FpList[ T ], headIn : T ) : FpList[ T ] = new FpList[ T ] {
        override def tail : FpList[ T ] = tailIn
        override def head : T = headIn
    }

    def unapply[ T ]( fpList : FpList[ T ] ) : Option[ (FpList[ T ], T) ] = fpList match {
        case FpNil => None
        case _ => Some( (fpList.tail, fpList.head) )
    }
}

case object FpNil extends FpList[ Nothing ] {
    override def head : Nothing = throw new Exception
    override def tail : FpList[ Nothing ] = throw new Exception
}


