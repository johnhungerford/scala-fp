package org.hungerford.fp.collections

import org.hungerford.fp.basic.{FpNone, FpOption}

trait FpSeq[ +T ] {
    def apply[ B >: T ]( index : Int ) : FpOption[ B ]

    def toFpList : FpList[ T ]

    def toList : List[ T ] = toFpList.toList

    def headOption : FpOption[ T ]

    def tailOption : FpOption[ FpSeq[ T ] ]

    def tailOrNil : FpSeq[ T ]

    def lastOption : FpOption[ T ]

    def isEmpty : Boolean = headOption == FpNone

    def nonEmpty : Boolean = !isEmpty

    def +:[ B >: T ]( ele : B ) : FpSeq[ B ]

    def :+[ B >: T ]( ele : B ) : FpSeq[ B ]

    def ++:[ B >: T ]( fpList : FpSeq[ B ] ) : FpSeq[ B ]

    def :++[ B >: T ]( fpList : FpSeq[ B ] ) : FpSeq[ B ]

    def fpString( sep : FpString ) : FpString = toFpList.fpString( sep )

    def reverse : FpSeq[ T ]

    def length : Int

    def times( num : Int ) : FpSeq[ T ]

    def filter( fn : T => Boolean ) : FpSeq[ T ]

    def take( num : Int ) : FpSeq[ T ]

    def takeWhile( fn : T => Boolean ) : FpSeq[ T ]

    def drop( num : Int ) : FpSeq[ T ]

    def dropWhile( fn : T => Boolean ) : FpSeq[ T ]

    def slice( start : Int, end : Int ) : FpSeq[ T ]

    def exists( fn : T => Boolean ) : Boolean

    def contains[ B >: T ]( ele : B ) : Boolean = exists( _ == ele )

    def distinct : FpSeq[ T ]

    def partition( fn : T => Boolean ) : (FpSeq[ T ], FpSeq[ T ])

    def collect[ B ]( fn : PartialFunction[ T, B ] ) : FpSeq[ B ]

    def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpSeq[ B ]

    def sortBy[ C ]( fn : T => C )( implicit ord : Ordering[ C ] ) : FpSeq[ T ] = {
        sort( Ordering.by( fn ) )
    }

    def sortWith[ B >: T ]( cmp : (B, B) => Int )  : FpSeq[ B ] = {
        sort( new Ordering[ B ] {
            override def compare( x : B, y : B ) : Int = cmp( x, y )
        } )
    }

    def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpSeq[ (B, C) ]

    def zipWithIndex[ B >: T ] : FpSeq[ (B, Int) ]

    def mapWithLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpSeq[ B ]

    def mapWithRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpSeq[ B ]
}
