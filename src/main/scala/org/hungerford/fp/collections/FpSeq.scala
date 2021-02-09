package org.hungerford.fp.collections

import org.hungerford.fp.basic.FpOption

trait FpSeq[ +T ] {
    def apply[ B >: T ]( index : Int ) : FpOption[ B ]

    def toFpList : FpList[ T ]

    def toList : List[ T ] = toFpList.toList

    def headOption : FpOption[ T ]

    def tailOption : FpOption[ FpSeq[ T ] ]

    def lastOption : FpOption[ T ]

    def +:[ B >: T ]( ele : B ) : FpSeq[ B ]

    def :+[ B >: T ]( ele : B ) : FpSeq[ B ]

    def ++:[ B >: T ]( fpList : FpSeq[ B ] ) : FpSeq[ B ]

    def :++[ B >: T ]( fpList : FpSeq[ B ] ) : FpSeq[ B ]

    def fpString( sep : FpString ) : FpString = toFpList.fpString( sep )

    def reverse : FpSeq[ T ]

    def lengthOpt : FpOption[ Int ]

    def times( num : Int ) : FpSeq[ T ]

    def filter( fn : T => Boolean ) : FpSeq[ T ]

    def take( num : Int ) : FpSeq[ T ]

    def takeWhile( fn : T => Boolean ) : FpSeq[ T ]

    def drop( num : Int ) : FpSeq[ T ]

    def dropWhile( fn : T => Boolean ) : FpSeq[ T ]

    def slice( start : Int, end : Int = -1 ) : FpSeq[ T ]

    def exists( fn : T => Boolean ) : Boolean

    def contains[ B >: T ]( ele : B ) : Boolean = exists( _ == ele )

    def distinct : FpSeq[ T ]

    def partition( fn : T => Boolean ) : (FpSeq[ T ], FpSeq[ T ])

    def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpSeq[ B ]

    def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpSeq[ B ]

    def sortBy[ B >: T, C ]( fn : B => C )( implicit ord : Ordering[ C ] ) : FpSeq[ B ] = {
        sort( Ordering.by( fn ) )
    }

    def sortWith[ B >: T ]( cmp : (B, B) => Int )  : FpSeq[ B ] = {
        sort( new Ordering[ B ] {
            override def compare( x : B, y : B ) : Int = cmp( x, y )
        } )
    }

    def zipWith[ B >: T, C ]( that : FpSeq[ C ] ) : FpSeq[ (B, C) ]

    def zipWithIndex[ B >: T ] : FpSeq[ (B, Int) ]

    def withLeft[ B >: T ]( start : B )( fn : (B, B) => B ) : FpSeq[ B ]

    def withRight[ B >: T ]( end : B )( fn : (B, B) => B ) : FpSeq[ B ]
}
