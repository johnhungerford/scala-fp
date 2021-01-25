package org.hungerford.fp.collections

import org.hungerford.fp.basic.FpOption

trait FpSeq[ +T ] {

    def toFpList : FpList[ T ]

    def headOption : FpOption[ T ]

    def +[ B >: T ]( ele : B ) : FpSeq[ B ]
    def append[ B >: T ]( ele : B ) : FpSeq[ B ]

    def ++[ B >: T ]( fpList : FpSeq[ B ] ) : FpSeq[ B ]

    def fpString : FpString

    def reverse : FpSeq[ T ]

    def length : Int

    def times( num : Int ) : FpSeq[ T ]

    def filter( fn : T => Boolean ) : FpSeq[ T ]

    def take( num : Int ) : FpSeq[ T ]

    def takeWhileEnd( fn : T => Boolean ) : FpSeq[ T ]

    def takeWhile( fn : T => Boolean ) : FpSeq[ T ]

    def dropWhileEnd( fn : T => Boolean ) : FpSeq[ T ]

    def dropWhile( fn : T => Boolean ) : FpSeq[ T ]

    def toList : List[ T ]

    def exists( fn : T => Boolean ) : Boolean

    def contains[ B >: T ]( ele : B ) : Boolean

    def distinct : FpSeq[ T ]

    def partition( fn : T => Boolean ) : (FpSeq[ T ], FpSeq[ T ])

    def collect[ B >: T ]( fn : PartialFunction[ T, B ] ) : FpSeq[ B ]

    def sort[ B >: T ]( implicit ord : Ordering[ B ] ) : FpSeq[ B ]

    def sortBy[ B >: T, C ]( fn : B => C )( implicit ord : Ordering[ C ] ) : FpSeq[ B ]

    def sortWith[ B >: T ]( cmp : (B, B) => Int )  : FpSeq[ B ]

    def reduce[ B >: T ]( fn : (B, B) => B ) : B

}
