package org.hungerford.fp.types

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class FunctorCovariantTest[ T[ _ ] <: FunctorStatic[ T ] ]( className : String, staticFunctor : FunctorStatic[ T ] ) extends AnyFlatSpecLike with Matchers {
    val afnList : List[ Int => Double ] = List[ Int => Double ](
        x => x.toDouble / 2,
        x => (3.32 + x) / 23,
        x => x.toDouble,
        x => x - x,
        x => x * ( - 53 ),
        x => ( x / 17 ),
    )

    val bfnList : List[ Double => String ] = List[ Double => String ](
        x => x.toString,
        x => x.toString.toList.sortWith( (a, b) => a > b ).toString(),
        x => x.toString.flatMap( c => s"$c$c$c$c$c"),
    )

    val aVals : List[ Int ] = List( 0, 1, 2, 3, -1, -2, -3, 50, -50, 150, -150, 10232, -10232 )

    val identVals : List[ Any ] = aVals ++ List( "hello", -0.235, Some( true ), false, 563, BigInt( "123233423443234435445" ) )

    behavior of s"Functor (covariant): ${className}"

    it should "obey functor law 1: fa.map(id) = fa" in {
        identVals.foreach( v => {
            val fa : T[ Any ] = staticFunctor.unit( v )
            staticFunctor.map( fa )( x => x ) shouldBe fa
        } )
    }

    it should "obey functor law 2: fa.map(f1 o f2) = fa.map(f1).map(f2)" in {
        for {
            afn <- afnList
            bfn <- bfnList
            aVal <- aVals
        } {
            val fa : T[ Int ] = staticFunctor.unit[ Int ]( aVal )
            val v1 = staticFunctor.map( fa )( afn andThen bfn )
            val v2 = staticFunctor.map( staticFunctor.map( fa )( afn ) )( bfn )
            //            println( s"$v1 shouldBe $v2")
            v1 shouldBe v2
        }
    }
}
