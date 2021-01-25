package org.hungerford.fp.collections

import org.hungerford.fp.types.MonadCovariantTest

class FpLazyListTest extends MonadCovariantTest(
    "FpLazyList",
    FpLazyList,
    List(
       FpLazyNil + 1 + 2 + 3,
       FpLazyNil,
       FpUnevaluatedLazyList( () => FpLazyNil ),
       FpUnevaluatedLazyList( () => (FpLazyNil + 1 + 2 + 3 + 4 ).evaluate() ),
       FpUnevaluatedLazyList( () => FpUnevaluatedLazyList( () => FpUnevaluatedLazyList( () => FpLazyListEval( FpLazyNil + -1 + 34534, 2 ) ).evaluate() ).evaluate() )
    ),
)
