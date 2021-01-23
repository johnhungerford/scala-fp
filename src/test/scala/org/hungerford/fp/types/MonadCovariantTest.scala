package org.hungerford.fp.types

class MonadCovariantTest[ T[ _ ] <: MonadStatic[ T ] ]( className : String, monadStatic : MonadStatic[ T ], exampleMonads : List[MonadCovariant[ T, Int ] ] )
  extends ApplicativeCovariantTest[ T ]( className, monadStatic ) {

    def f[ A ]( ele : A ) : T[ A ] = monadStatic.unit[ A ]( ele : A )
    def flatMap[ A, B ]( ele : T[ A ] )( fn : A => T[ B ] ) : T[ B ] = monadStatic.flatMap( ele )( fn )

    behavior of s"Monad (covariant): ${className}"

    val mAfnList : List[ Int => T[ Double ] ] = afnList.map( ( fn : Int => Double ) => ( x: Int ) => f( fn( x ) ) )
    val mBfnList : List[ Double => T[ String ] ] = bfnList.map( ( fn : Double => String ) => ( x: Double ) => f( fn( x ) ) )

    it should "obey monad law 1: flatMap(f(a))(fn) = fn(a)" in {
        for {
            fn <- mAfnList
            aVal <- aVals
        } {
            flatMap(f(aVal))(fn) shouldBe fn(aVal)
        }
    }

    it should "obey monad law 2: flatMap(m)(y => f(y)) = m" in {
        for {
            m <- exampleMonads
        } {
            flatMap(m.asInstanceOf[T[ Int ]])(y => f(y)) shouldBe m
        }
    }

    it should "obey monad law 3: flatMap(flatMap(m)(fn1))(fn2) = flatMap(flatMap(m)(x => fn1(x))(f2)" in {
        for {
            fn1 <- mAfnList
            fn2 <- mBfnList
            m <- exampleMonads
        } {
            flatMap(flatMap(m.asInstanceOf[T[ Int ]])(fn1))(fn2) shouldBe flatMap(flatMap(m.asInstanceOf[T[ Int ]])(x => fn1(x)))(fn2)
        }
    }
}
