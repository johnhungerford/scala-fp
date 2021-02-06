package org.hungerford.fp.collections

import org.hungerford.fp.types.MonadTest

class FpLazyListTest extends MonadTest(
    "FpLazyList",
    FpLazyList,
    List(
       1 +: 2 +: 3 +: FpLazyNil,
       FpLazyNil,
       FpLazyList.makeLazy( FpLazyNil ),
       FpLazyList.makeLazy( 1 +: 2 +: 3 +: 4 +: FpLazyNil ),
       FpLazyList.makeLazy( FpLazyList.makeLazy( FpLazyList.makeLazy( FpLazyListEval( 100000, -1 +: 34534 +: 2 +: FpLazyNil ) ) ) )
    ),
)
