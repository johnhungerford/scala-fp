package org.hungerford.fp.types

class ApplicativeTest[ T[ +_ ] <: Applicative[ T, _ ] ]( className : String, staticApplicative : ApplicativeStatic[ T ] )
  extends FunctorTest[ T ]( className, staticApplicative ) {

    behavior of s"Applicative (covariant): ${className}"

    it should "obey applicative law 1: appl(f(id))(fa) = fa" in {
        identVals.foreach( v => {
            val fid = staticApplicative.unit[ Any => Any ]( x => x )
            val fa = staticApplicative.unit[ Any ]( v )
            staticApplicative.appl( fid )( fa ) shouldBe fa
        } )
    }

    it should "obey applicative law 2: appl(f(fn))(fa) = f(fn(a))" in {
        for {
            afn <- afnList
            bfn <- bfnList
            aVal <- aVals
        } {
            staticApplicative.appl( staticApplicative.unit( afn ) )( staticApplicative.unit( aVal ) ) shouldBe staticApplicative.unit( afn( aVal ) )
            staticApplicative.appl( staticApplicative.unit( afn andThen bfn ) )( staticApplicative.unit( aVal ) ) shouldBe staticApplicative.unit( ( afn andThen bfn )( aVal ) )
        }
    }

    it should "obey applicative law 3: appl(f(fn))(fa) = appl(f($(a)))(u)" in {
        def $[ A, B ]( x : A ) : (A => B) => B = ( f : A => B ) => f( x )
        for {
            afn <- afnList
            bfn <- bfnList
            aVal <- aVals
        } {
            val u1 = staticApplicative.unit( afn )
            staticApplicative.appl( u1 )( staticApplicative.unit( aVal ) ) shouldBe staticApplicative.appl( staticApplicative.unit( $[ Int, Double ]( aVal ) ) )( u1 )
            val u2 = staticApplicative.unit( afn andThen bfn )
            staticApplicative.appl( u2 )( staticApplicative.unit( aVal ) ) shouldBe staticApplicative.appl( staticApplicative.unit( $[ Int, String ]( aVal ) ) )( u2 )
        }

    }

    it should "obey applicative law 4: u <*> (v <*> w) = pure (.) <*> u <*> v <*> w" in {
        def o[ A, B, C ] : ( B => C ) => ( A => B ) => ( A => C ) = ( f1 : B => C ) => ( f2 : A => B ) => ( a : A ) => f1( f2( a ) )
        for {
            afn <- afnList
            bfn <- bfnList
            aVal <- aVals
        } {
            val ffn2 : T[ Int => Double ] = staticApplicative.unit( afn )
            val ffn1 : T[ Double => String ] = staticApplicative.unit( bfn )
            def appl[ A, B ]( fn : T[ A => B ] ) : T[ A ] => T[ B ] = staticApplicative.appl[ A, B ]( fn )
            def f[ A ]( v : A ) : T[ A ] = staticApplicative.unit[ A ]( v )

            appl[ Int, String ](appl[ Int => Double, Int => String ](appl[ Double => String, (Int => Double) => Int => String ](f[ ( Double => String ) => ( Int => Double ) => Int => String ]( o[ Int, Double, String ] ))(ffn1))(ffn2))(f(aVal)) shouldBe appl(ffn1)(appl(ffn2)(f(aVal)))
        }
    }
}
