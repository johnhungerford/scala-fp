package org.hungerford.fp.types

import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

class MonoidTest[ T <: Monoid[ T ] ]( className : String, static : MonoidStatic[ T ], exampleMonoids : List[ Monoid[ T ] ] ) extends AnyFlatSpecLike with Matchers {

    behavior of s"Monoid (invariant): $className"

    it should "obey monoid law 1: (x + y) + z = x + (y + z)" in {
        for {
            x <- exampleMonoids
            y <- exampleMonoids
            z <- exampleMonoids
        } {
            x.combine( y.asInstanceOf[ T ] ).combine( z.asInstanceOf[ T ] ) shouldBe x.asInstanceOf[ T ].combine(y.asInstanceOf[ T ].combine(z.asInstanceOf[ T ]))
        }
    }

    it should "obey monoid law 2: empty + x = x" in {
        for {
            x <- exampleMonoids
        } {
            static.empty.combine(x.asInstanceOf[ T ]) shouldBe x
        }
    }

    it should "obey monoid law 2: x + empty = x" in {
        for {
            x <- exampleMonoids
        } {
            x.combine(static.empty) shouldBe x
        }
    }

}

class TypedMonoidTest[ T[ +_ ] <: TypedMonoid[ T, _ ], B ]( className : String, static : TypedMonoidStatic[ T ], exampleMonoids : List[ TypedMonoid[ T, B ] ] ) extends AnyFlatSpecLike with Matchers {

    behavior of s"Monoid (invariant): $className"

    it should "obey monoid law 1: (x + y) + z = x + (y + z)" in {
        for {
            x <- exampleMonoids
            y <- exampleMonoids
            z <- exampleMonoids
        } {
            x.combine( y.asInstanceOf[ T[ B ] ] ).combine( z.asInstanceOf[ T[ B ] ] ) shouldBe x.asInstanceOf[ T[ B ] ].combine(y.asInstanceOf[ T[ B ] ].combine(z.asInstanceOf[ T[ B ] ]))
        }
    }

    it should "obey monoid law 2: empty + x = x" in {
        for {
            x <- exampleMonoids
        } {
            static.empty.combine(x.asInstanceOf[ T[ B ] ]) shouldBe x
        }
    }

    it should "obey monoid law 2: x + empty = x" in {
        for {
            x <- exampleMonoids
        } {
            x.combine(static.empty) shouldBe x
        }
    }

}

