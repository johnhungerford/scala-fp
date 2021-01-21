package org.hungerford.fp.types

trait Transformer[ A[ _ ] <: Monad[ A, _ ], B[ _ ] <: Monad[ B, _ ], T ] extends Monad[ ({ type U[ X ] = Transformer[ A, B, X ] })#U, T ] {
    val value : B[ A[ T ] ]
}
